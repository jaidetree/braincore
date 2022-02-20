(ns braincore.notion.api
  (:require
   [promesa.core :as p]))

(def notion-api (js/require "@notionhq/client"))
(def api-key js/process.env.NOTION_API_KEY)

(def notion (new (.-Client notion-api) #js {:auth api-key}))


(defn fetch-page
  [{:keys [page-id]}]
  (p/-> (.. notion -pages (retrieve #js {:page_id page-id}))
        (js->clj :keywordize-keys true)
      #_(:results)))

(defn fetch-blocks
  [{:keys [block-id]}]
  (p/-> (.. notion -blocks -children (list #js {:block_id block-id}))
        (js->clj :keywordize-keys true)
        (:results)))

(defn fetch-all-blocks
  [{:keys [block-id]}]
  (p/let [blocks (fetch-blocks {:block-id block-id})]
    (p/-> (p/all (->> blocks
                      (filter #(not (:archived %)))
                      (map #(if (true? (:has_children %))
                              (p/let [blocks (fetch-all-blocks {:block-id (:id %)})]
                                (assoc % :children blocks))
                              (p/resolved (assoc % :children []))))
                      (vec)))
          (js->clj :keywordize-keys true))))

(defn append-blocks
  [{:keys [block-id children]}]
  (let [children (clj->js children)]
    (js/console.log (js/JSON.stringify #js {:block-id "[redacted]"
                                            :children (clj->js children)} nil 2))
    (p/let [response (.. notion -blocks -children
                         (append #js {:block_id block-id
                                      :children (clj->js children)}))]
      (-> (js->clj response :keywordize-keys true)
          (get :results [])))))
