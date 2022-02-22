(ns braincore.commands.create-entry
  (:require
   [clojure.string :as s]
   [cljs.pprint :refer [pprint]]
   [clojure.edn :refer [read-string]]
   [promesa.core :as p]
   [braincore.env :as env]
   [braincore.notion.blocks :as b]
   [braincore.notion.api :as notion]
   [braincore.formats :as date]
   [braincore.linear.api :as linear]
   [braincore.google-cal.api :as gcal]))

(def fs (js/require "fs"))

(def page-id (env/get :NOTION_PAGE_ID))
(defonce entry-atom (atom {}))
(defonce linear-atom (atom {}))

(defn last-entry
  [blocks]
  (->> blocks
       (filter #(and (= (:type %) "heading_1")
                     (true? (:has_children %))))
       (last)))

(defn entry-columns-block
  [blocks]
  (->> blocks
       (filter #(= (:type %) "column_list"))
       (first)))

(defn pprint-str
  [arg]
  (if (or (not arg) (empty? arg))
    (str arg)
    (with-out-str
      (pprint arg))))

(defn on-error
  "
  We don't want to swallow errors but we do want to get a better sense
  for what request failed given we may hit the same notion api with
  different args
  "
  [f params error-msg & error-msg-args]
  (-> (f params)
      (p/catch
       (fn [err]
         (js/console.error err)
         (let [error-msg (str error-msg " " (s/join " " (map pprint-str error-msg-args)))]
           (throw (new js/Error error-msg)))))))

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
  (let [page-id js/process.env.NOTION_PAGE_ID
        date-title (date/full-date target-date)]
    (p/let [[notion-summary linear-summary events]
            (p/all [(p/-> page-id
                          (fetch-prev-entry)
                          (parse-entry date-title))
                    (fetch-linear-summary)
                    (gcal/fetch-events-list (date/day-of target-date))])]

      (-> {:notion notion-summary
           :linear linear-summary
           :events events
           :target-date date-title}
          (build-notion-entry)
          (append-entry-to-page page-id)))))

(defn save-edn-file
  [content filename]
  (.writeFileSync fs filename (pprint-str content))
  content)

(defn load-edn-file
  [filename]
  (read-string (.readFileSync fs filename #js {:encoding "utf-8"})))

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

(comment

  (p/let [entry (fetch-prev-entry page-id)]
    (save-edn-file entry "blocks.edn")
    (reset! entry-atom entry)
    (println "Fetched prev entry")
    (pprint entry))

  (p/let [linear-summary (fetch-linear-summary)]
    (save-edn-file linear-summary "linear.edn")
    (reset! linear-atom linear-summary)
    (println "Fetched linear summary")
    (pprint linear-summary))

  (pprint @entry-atom)
  (pprint @linear-atom)

  (do
    (reset! entry-atom (load-edn-file "blocks.edn"))
    (reset! linear-atom (load-edn-file "linear.edn"))
    nil)

  (str->date "2022-02-22")

  (let [target-date (str->date "2022-02-22")
        #_#_linear-data @linear-atom
        linear-data {}
        notion-data (parse-entry @entry-atom target-date)]
    (p/-> (build-notion-entry
           {:notion notion-data
            :linear linear-data
            :target-date target-date})
          (save-edn-file "debug.edn")
          (append-entry-to-page page-id)
          (save-edn-file "debug-out.edn")
          (p/catch (fn [error]
                     (js/console.error error)))))

  (let [body (load-edn-file "debug.edn")]
    (p/->> (get-in body [:columns-blocks 0 :column_list :children 0 :column :children 8])
           (pprint)))
)
