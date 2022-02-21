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
   [braincore.linear.api :as linear]))

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
  (dissoc block
          :archived
          :children
          :created_time
          :has_children
          :id
          :last_edited_time))


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
       (filter #(and (= (:type %) "to_do")
                     (not (get % :archived))
                     (not (get-in % [:to_do :checked]))))
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
  (println "date-str" date-str)
  (let [date (new js/Date date-str)]
    (not= (.toString date) "Invalid Date")))

(defn parse-entry
  "
  Takes a map containing the page, entry, and columns blocks then the
  target-date js/Date instance
  Returns an obj
  "
  [{:keys [page entry columns]} target-date]
  (let [[tasks _linear _notes] (:children columns)
        headers (collect-headers columns)
        incomplete-tasks (collect-incomplete-tasks (:children tasks))
        last-entry-date (get-in entry [:heading_1 :text 0 :text :content])]

    (when (or (not last-entry-date) (not (valid-date? last-entry-date)))
      (throw (new js/Error "Could not parse date")))

    (println "last-entry-date" "=" last-entry-date "target-date " (date/full-date target-date))
    (when (= last-entry-date (date/full-date target-date))
      (throw (new js/Error (str "Entry for date " last-entry-date " already exists, skipping"))))

    {:page             page
     :entry            entry
     :columns          columns
     :headers          headers
     :incomplete-tasks incomplete-tasks
     :last-entry-date  last-entry-date}))

(defn fetch-linear-summary
  []
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
  []
  [(b/heading-1
    (date/full-date)
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

(defn create-columns
  [{:keys [notion linear]}]
  (let [incomplete-tasks (get notion :incomplete-tasks)
        [tasks-title linear-title notes-title] (get notion :headers [])]
    [(b/columns
      [(b/column (into [tasks-title
                        (b/divider)]
                       incomplete-tasks))
       (b/column (into [linear-title
                        (b/divider)]
                       (linear-blocks linear)))
       (b/column [notes-title
                  (b/divider)
                  (b/bullet [(b/text "How was your day...")])])])]))

(defn build-notion-entry
  [{:keys [_notion _linear] :as data}]
  {:section-blocks (create-section)
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
                            "Could not create columns in section" section-id section)]

    (println "Created sections and columns")
    (pprint {:section-id section-id
             :columns-id (-> columns (first) (:id))})))

(defn create-notion-entry
  [target-date]
  (let [page-id js/process.env.NOTION_PAGE_ID]
    (p/let [[notion-summary linear-summary]
            (p/all [(p/-> page-id
                          (fetch-prev-entry)
                          (parse-entry target-date))
                    (fetch-linear-summary)])]

      (-> {:notion notion-summary
           :linear linear-summary}
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

  (-> (parse-entry @entry-atom (new js/Date))
      (pprint))

  (p/-> (fetch-linear-summary)
        (get-in [:create :linear])
        #_(linear-blocks)
        (pprint))


  (-> (build-notion-entry
       {:notion (-> @entry-atom (parse-entry js/Date))
        :linear @linear-atom})
      (save-edn-file "debug.edn"))

  (do
    (reset! entry-atom (load-edn-file "blocks.edn"))
    (reset! linear-atom (load-edn-file "linear.edn"))
    nil)

  (str->date "2022-02-21")

  (-> @entry-atom
      (parse-entry (str->date "2022-02-21")))

  (let [datemap (date/date->map (new js/Date "2022-02-21T00:00"))]
    (-> datemap
        (date/iso)))

  (p/-> (build-notion-entry
         {:notion (-> @entry-atom (parse-entry (new js/Date "2022-02-21T00:00")))
          :linear @linear-atom})
        (save-edn-file "debug.edn")
        (append-entry-to-page page-id)
        (save-edn-file "debug-out.edn")
        (p/catch (fn [error]
                   (js/console.error error))))

  (let [body (load-edn-file "debug.edn")]
    (get-in body [:columns-blocks 0 :column_list :children 2 :column :children 2 :bulleted_list_item]))
)
