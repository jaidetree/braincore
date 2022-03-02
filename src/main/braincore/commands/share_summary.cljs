(ns braincore.commands.share-summary
  (:require
   [cljs.pprint :refer [pprint]]
   [clojure.string :as s]
   [promesa.core :as p]
   [braincore.env :as env]
   [braincore.notion.api :as notion]
   [braincore.notion.utils :as nu]
   [braincore.utils :refer [on-error]]
   ["@slack/web-api" :refer [WebClient]]))

(def page-id (env/get :NOTION_PAGE_ID))
(def slack-api-key (env/get :SLACK_API_KEY))
(def slack-channel-id (env/get :SLACK_CHANNEL_ID))

(defn fetch-entries
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
          entries (on-error notion/fetch-blocks
                            {:block-id page-id}
                            "Could not fetch page blocks" page-id)]
    {:page         page
     :entries      (filter nu/entry? entries)}))

(defn not-personal?
  [block]
  (let [text (s/lower-case (get block :text-str))]
    (->> ["personal"
          "hide"
          "hidden"
          "no-share"
          "dont-share"
          "not-shared"
          "secret"
          "[p]"]
         (some (partial s/includes? text))
         (not))))


(defn task?
  [block]
  (and (= (:type block) "to_do")
       (not-personal? block)))

(defn container?
  [block]
  (and (not (task? block))
       (get block :has_children)
       (vector? (get block :children))))

(defn text->str
  [block]
  (let [type (keyword (get block :type))]
    (->> (get-in block [type :text])
         (map #(get-in % [:text :content]))
         (s/join " "))))

(defn parse-task
  [task]
  {:id (get task :id)
   :complete (get-in task [:to_do :checked])
   :title (get task :text-str)})

(defn collect-tasks
  [task-blocks]
  (->> task-blocks
       (map #(assoc % :text-str (text->str %)))
       (take-while not-personal?)
       (mapcat (fn [block]
                 (cond
                   (task? block)      [(parse-task block)]
                   (container? block) (map collect-tasks (get :children block))
                   :else              [])))))

(defn incomplete?
  [task]
  (= (:complete task) false))

(defn complete?
  [task]
  (= (:complete task) true))

(defn fetch-entry-blocks
  [entry-block]
  (p/let [[_divider columns-block] (on-error notion/fetch-all-blocks
                                             {:block-id (:id entry-block)}
                                             "Could not fetch entry blocks"
                                             (:id entry-block))
          tasks (->> (get-in columns-block [:children 0 :children])
                     (drop 2)
                     (collect-tasks))]
    {:title (get-in entry-block [:heading_1 :text 0 :text :content])
     :incompleted-tasks (->> tasks
                             (filter incomplete?))
     :completed-tasks (->> tasks
                           (filter complete?))
     }))

(defn slack-summary-task
  [{:keys [title complete]}]
  (str "[" (if complete "X" " ") "]" " " title))

(defn slack-summary
  [{:keys [title incompleted-tasks completed-tasks]}]
  (str title "\n"
       (->> (repeat 60 "-") (s/join ""))
       "\n"
       (->> [completed-tasks
             incompleted-tasks]
            (mapcat #(map slack-summary-task %))
            (s/join "\n"))))

(defn summary->str
  [entries]
  (str
   "```\n"
   (->> entries
        (s/join "\n\n"))
   "```\n"
   ))

(defn format-slack-message
  [summaries]
  {:text (->> summaries
              (map slack-summary)
              (summary->str))
   :channel slack-channel-id
   :blocks
   (concat [{:type "context",
             :elements [{:type "mrkdwn",
                         :text "sent from :brain: https://github.com/eccentric-j/braincore"}]}
            ])})

(defn send-to-slack
  [message]
  (let [client (new WebClient slack-api-key)]
    (p/->
     (p/let [response (-> client .-chat (.postMessage (clj->js message)))]
       (pprint (js->clj (.-request response))))
     (p/catch (fn [err]
                (js/console.error (.-request err)))))))

(defn share-yesterday-today-summary
  []
  (p/let [{:keys [entries page]} (fetch-entries page-id)
          [yesterday today] (p/all
                             (->> entries
                                  (take-last 2)
                                  (map fetch-entry-blocks)))
          slack-message (format-slack-message
                         [yesterday
                          today])]
    (send-to-slack slack-message)))

(comment

  (share-yesterday-today-summary)

  )

(defn share-yesterday-summary
  []
  (p/let [{:keys [entries page]} (fetch-entries page-id)
          yesterday (p/do!
                             (->> entries
                                  (take-last 2)
                                  (first)
                                  (map fetch-entry-blocks)))
          slack-message (format-slack-message
                         [yesterday])]
    (send-to-slack slack-message)))

(defn share-today-summary
  []
  (p/let [{:keys [entries page]} (fetch-entries page-id)
          today (p/do!
                 (->> entries
                      (last)
                      (map fetch-entry-blocks)))
          slack-message (format-slack-message
                         [today])]
    (send-to-slack slack-message)))

(defn share-summary
  [format]
  (case format
    :yesterday-today (share-yesterday-today-summary)
    :yesterday       (share-yesterday-summary)
    :today           (share-today-summary)
    (throw (new js/Error (str "Could not share summary in format" format)))))

(defn share-summary-cmd
  [format & _args]
  (share-summary (keyword format)))

(comment
  (share-summary :yesterday-today))
