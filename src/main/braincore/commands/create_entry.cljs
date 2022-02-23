(ns braincore.commands.create-entry
  (:require
   [clojure.string :as s]
   [cljs.pprint :refer [pprint]]
   [clojure.edn :refer [read-string]]
   [promesa.core :as p]
   [braincore.env :as env]
   [braincore.notion.blocks :as b]
   [braincore.notion.api :as notion]
   [braincore.notion.utils :as nu]
   [braincore.formats :as date]
   [braincore.linear.api :as linear]
   [braincore.google-cal.api :as gcal]
   [braincore.utils :refer [on-error pprint-str with-redefs-async] :as u]
   ["fs/promises" :as fs]))


(def page-id (env/get :NOTION_PAGE_ID))

(defn last-entry
  [blocks]
  (->> blocks
       (filter nu/entry?)
       (last)))

(defn entry-columns-block
  [blocks]
  (->> blocks
       (filter #(= (:type %) "column_list"))
       (first)))

(defn fetch-prev-entry
  "
  Fetches the target page then fetches its blocks, gets the latest date entry
  then recursively fetches all of its blocks
  Takes no args
  Returns a map containing the page map, entry map, and columns with children
  "
  [page-id]
  (println "Fetching prev entry...")
  (p/let [page (on-error notion/fetch-page
                         {:page-id page-id}
                         "Could not fetch page" page-id)
          page-blocks (on-error notion/fetch-blocks
                                {:block-id page-id}
                                "Could not fetch page blocks" page-id)
          entry (last-entry page-blocks)
          entry-blocks (on-error notion/fetch-blocks
                                 {:block-id (:id entry)}
                                 "Could not fetch entry blocks" entry)
          columns (entry-columns-block entry-blocks)
          columns-blocks (on-error notion/fetch-all-blocks
                                   {:block-id (:id columns)}
                                   "Could not fetch columns blocks" columns)]
    {:page         page
     :entry        entry
     :columns      (assoc columns :children columns-blocks)}))

(defn clean-block
  "
  Sometimes we want to copy an existing block over to reduce complexity
  This strips away the fields that are not typically needed
  "
  [block]
  (let [block (dissoc block
                      :archived
                      :children
                      :created_time
                      :has_children
                      :id
                      :last_edited_time
                      :last_edited_by
                      :created_by)
        type (get block :type)]
    block
    ))

(defn block-canceled?
  [block]
  (let [type (keyword (get block :type))
        text-entries (get-in block [type :text])
        struck (->> text-entries
                      (filter #(get-in % [:annotations :strikethrough])))]
    (= (count struck) (count text-entries))))

(comment
  (block-canceled?
   (b/paragraph [(b/text {:strikethrough true} "all text")
                 (b/text {:strikethrough true} "canceled")]))
  (block-canceled?
   (b/paragraph [(b/text {:strikethrough true} "some text")
                 (b/text {} "canceled")]))
  )

(defn persist-block?
  [block]
  (let [type (get block :type)]
    (cond
      ;; Remove paragraphs with no text entries
      (= type "paragraph")
      (seq (get-in block [:paragraph :text]))

      ;; Remove blocks that are stricken through
      ;; but only if all their text is stricken through
      (block-canceled? block)
      false

      ;; Keep items like headings and dividers
      (not= type "to_do")
      true

      ;; Drop completed to do items
      (and (= type "to_do")
           (not (get block :archived)))
      (not (get-in block [:to_do :checked])))))

(defn collect-incomplete-tasks
  "
  Each Notion entry is expected to have 3 columns
  - Tasks
  - Linear tickets
  - Notes

  Takes a sequence of blocks that make up the children of the tasks column
  Returns a list of the unchecked todo item blocks

  The original blocks are preserved here as they may contain links or other
  metadata that may be expected to carry over as well
  "
  [tasks-blocks]
  (->> tasks-blocks
       (filter persist-block?)
       ;; If block has children like a :toggle or collapsible heading
       ;; parse them recursively
       (map (fn [block]
              (let [children (:children block)
                    type (:type block)]
                (if (seq children)
                  (assoc-in block [(keyword type) :children]
                            (collect-incomplete-tasks (:children block)))
                  block))))
       (map clean-block)))

(defn collect-headers
  "
  Takes the columns block of an entry which contains the tasks, linear, and notes columns
  Returns a vector of strings containing their heading

  This way if other users use this and want to rename a heading it should still work
  Order of the columns is really important
  "
  [columns-block]
  (->> columns-block
       (:children)
       (map #(->> % :children first))
       (filter #(= (:type %) "heading_2"))
       (map clean-block)
       #_(map #(get-in % [:heading_2 :text 0 :text :content]))))

(defn valid-date?
  [date-str]
  (let [date (new js/Date date-str)]
    (not= (.toString date) "Invalid Date")))

(defn parse-entry
  "
  Takes a map containing the page, entry, and columns blocks then the
  target-date str in \"Monday, Feb 21, 2022\" format
  Returns an obj
  "
  [{:keys [page entry columns]} target-date]
  (let [[tasks _linear _notes] (:children columns)
        headers (collect-headers columns)
        incomplete-tasks (->> tasks
                              (:children)
                              ;; Drop the heading-2 and divider
                              (drop 2)
                              (collect-incomplete-tasks))
        last-entry-date (get-in entry [:heading_1 :text 0 :text :content])]

    (when (or (not last-entry-date) (not (valid-date? last-entry-date)))
      (throw (new js/Error "Could not parse date")))

    (when (= last-entry-date target-date)
      (throw (new js/Error (str "Entry for date " last-entry-date " already exists, skipping"))))

    {:page             page
     :entry            entry
     :columns          columns
     :headers          headers
     :incomplete-tasks incomplete-tasks
     :target-date      target-date
     :last-entry-date  last-entry-date}))

(defn fetch-linear-summary
  []
  (println "Fetching linear summary...")
  (p/let [[in-progress in-review todos completed]
          (p/all [(linear/in-progress-issues)
                  (linear/in-review-issues)
                  (linear/todo-issues)
                  (linear/completed-issues)])]
    {:in-progress in-progress
     :in-review in-review
     :todos     todos
     :completed completed}))

(defn fetch-cal-events
  [target-date]
  (-> target-date
      (date/day-of)
      (gcal/fetch-events-list)))

(defn create-section
  "
  Creates the parent notion section that is a collapsible heading-1 block
  This will eventually contain the three columns but for now these requests
  must be done separately as Notion's API does not let you create heading-1
  with columns in its children otherwise fails validation and returns errors.
  "
  [{:keys [target-date] }]
  [(b/heading-1
    target-date
    [(b/divider)])
   (b/divider)])

(defn linear-block
  [{:keys [identifier title url]}]
  (b/bullet [(b/text {:href url} (str identifier " - " title))]))

(defn when-empty
  [coll default]
  (if (seq coll)
    (map linear-block coll)
    [(b/paragraph [(b/text {:italic true} default)])]))

(defn linear-blocks
  [{:keys [in-progress in-review todos completed]}]
  (->> [[(b/heading-3 "In-Progress" [])]
        (when-empty in-progress "Nothing in progress. Get started")
        [(b/heading-3 "To do" [])]
        (when-empty todos "No todos today")
        (b/heading-3
         "In-Review"
         (when-empty in-review "No tasks in-review"))
        (b/heading-3
         "Completed"
         (when-empty completed "No completed tasks"))]
       (flatten)))


(defn event-block
  [{:keys [id summary start end url meeting-type meeting-url location]} time-zone]
  (b/toggle
   (concat []
           (if meeting-url
             [(b/text {:href meeting-url} "☎")]
             [(b/text {} "📵")])
           [(b/text "\t")
            (b/text
             {:href url}
             (str (date/time time-zone start)
                  " - "
                  (date/time time-zone end)
                  " "
                  summary))])
   (concat []

           (when meeting-type
             [(b/bullet
               [(b/text (str "Type: "
                             (if meeting-type
                               meeting-type
                               "In-Person")))])])

           (when meeting-url
             [(b/bullet
               [(b/text {:href meeting-url} meeting-url)])])

           (when location
             [(b/bullet
               [(b/text (str "Location: " location))])])
           )
   ))

(defn event-blocks
  [{:keys [time-zone items] :as events}]
  (->> items
       (map #(event-block % time-zone))))

(defn create-columns
  [{:keys [notion linear events]}]
  (let [incomplete-tasks (get notion :incomplete-tasks)
        [tasks-title linear-title events-title notes-title] (get notion :headers [])]
    [(b/columns
      [(b/column (into [tasks-title
                        (b/divider)]
                       incomplete-tasks))
       (b/column (into [linear-title
                        (b/divider)]
                       (linear-blocks linear)))
       (b/column (into [events-title
                        (b/divider)]
                       (event-blocks events)))
       (b/column [notes-title
                  (b/divider)
                  (b/bullet [(b/text "How was your day...")])])])]))

(defn build-notion-entry
  [{:keys [_notion _linear _target-date] :as data}]
  {:section-blocks (create-section data)
   :columns-blocks (create-columns data)})

(defn append-entry-to-page
  [{:keys [section-blocks columns-blocks]} page-id]
  (p/let [section (on-error notion/append-blocks
                            {:block-id page-id
                             :children section-blocks}
                            "Could not create new entry section" page-id)
          section-id (-> section (first) (:id))
          columns (on-error notion/append-blocks
                            {:block-id section-id
                             :children columns-blocks}
                            "Could not create columns in section" section-id section)
          result {:section-id section-id
                  :columns-id (-> columns (first) (:id))}]

    (println "Created sections and columns")
    result
    ))

(defn create-notion-entry
  [target-date]
  (let [page-id (env/get :NOTION_PAGE_ID)
        date-title (date/full-date target-date)]
    (p/let [[notion-summary linear-summary events]
            (p/all [(p/-> page-id
                          (fetch-prev-entry)
                          (parse-entry date-title))
                    (fetch-linear-summary)
                    (fetch-cal-events target-date)])]

      (-> {:notion notion-summary
           :linear linear-summary
           :events events
           :target-date date-title}
          (build-notion-entry)
          (append-entry-to-page page-id)))))

(defn save-edn-file
  [content filename & {:keys [append]}]
  (.writeFile fs filename (pprint-str content)
              #js {:flag (if append "a" "w")})
  content)

(defn load-edn-file
  [filename]
  (p/-> (.readFile fs filename #js {:encoding "utf-8"})
        (read-string )))


(defn str->date
  [date-str]
  (-> (new js/Date (str date-str "T00:00"))
      (date/date->map)
      (date/iso)
      (date/parse)))

(defn create-entry-cmd
  [target-date & _args]
  (println "Creating journal entry in notion")
  (create-notion-entry
   (cond (s/blank? target-date)    (new js/Date)
         (valid-date? target-date) (str->date target-date)
         :else
         (throw (new js/Error (str "Invalid date value provided. Received " target-date))))))

(defn save-mock
  [f-sym filename & args]
  (let [[name f] (u/sym-pair f-sym)]
    (p/let [entry (apply f args)]
      (save-edn-file entry filename)
      (println "Saved mock for" name "as" filename))))

(comment

  (save-mock 'fetch-prev-entry "entry.edn" page-id)
  (save-mock 'fetch-linear-summary "linear.edn")
  (save-mock 'fetch-cal-events "events.edn" (new js/Date))


  (p/-> (with-redefs-async
          [fetch-prev-entry     (fn [] (load-edn-file "blocks.edn"))
           fetch-linear-summary (fn [] (load-edn-file "linear.edn"))
           fetch-cal-events     (fn [] (load-edn-file "events.edn"))

           notion/append-blocks
           (fn [data]
             (let [id (str (random-uuid))
                   data (assoc-in data [:children 0 :id] id)]
               (p/do!
                (save-edn-file data "debug.edn" :append true)
                (:children data))))]
          (save-edn-file "" "debug.edn")
          (create-entry-cmd "2022-02-23"))
        (p/catch js/console.error))

  )

(comment
  (let [body (load-edn-file "debug.edn")]
    (p/->> (get-in body [:columns-blocks 0 :column_list :children 0 :column :children 8])
           (pprint)))
)
