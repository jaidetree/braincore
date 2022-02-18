(ns braincore.linear.api
  (:require
   [kitchen-async.promise :as p]
   [clojure.string :as s]
   [cljs.pprint :refer [pprint]]))

(def api-key js/process.env.LINEAR_API_KEY)
(def linear-sdk (js/require "@linear/sdk"))
(def ^js/Object linear (new (.. linear-sdk -LinearClient) #js {:apiKey api-key}))

(def graphql-client (.-client linear))

(defn gql-filter
  [filter-data]
  (let [json-str (js/JSON.stringify (clj->js filter-data))]
    (-> json-str
        (clojure.string/replace #"\\" "")
        (clojure.string/replace #"\"([\w]+)\":" "$1: ")
        #_(subs 1))))

(comment

  (println (gql-filter {:state {:name {:eq "Todo"}}}))

  )

(defn clj->gql
  "
  Implementation mostly comes from
  https://medium.com/@kirill.ishanov/poor-mans-graphql-client-for-clojurescript-apps-8dc4b04e8738
  "
  [q]
  (cond
    (keyword? q)
    (let [[query alias] (clojure.string/split (name q) #"->")]
      (if alias
        (str alias ":" query)
        query))


    (map? q)
    (str \(
         (clojure.string/join
           \,
           (map (fn [[k v]]
                  (let [v (if (keyword? v) (name v) v)]
                    (str (name k) ":" (gql-filter v))))
                q))
         \))


    (vector? q)
    (str \{
         (clojure.string/join
           \space
           (map clj->gql q))
         \})

    :else
    (throw (js/Error. (str "Cannot parse query " (pr-str q))))))


(defn graphql-query
  "
  Runs a graphql query against the raw graphql client that powers linear's sdk

  Using this approach because the sdk api functions return class instances of
  their data types. This means processing them with (js->clj) has no effect.

  Using the raw graphql client, it just returns json data that can easily parse
  into edn with (js->clj) making it trivial to log, and operate on with the clj
  toolbox
  "
  [query-str]
  (p/let [response (.rawRequest graphql-client (str "query " query-str))]
    (js->clj response :keywordize-keys true)))


(defn todo-issues
  []
  (p/-> [:viewer
         [:id
          :name
          :email
          :assignedIssues
          {:filter {:state {:name {:eq "Todo"}}}}
          [:nodes
           [:id
            :title
            :updatedAt
            :completedAt
            :url
            :identifier
            :state
            [:id :name]]]]]
        (clj->gql)
        (graphql-query)
        (get-in [:data :viewer :assignedIssues :nodes])))

(comment

  (p/try
    (p/-> (todo-issues) (pprint))
    (p/catch js/Error error
      (js/console.error error))))

(defn in-progress-issues
  []
  (p/-> [:viewer
         [:id
          :name
          :email
          :assignedIssues
          {:filter {:state {:name {:eq "In Progress" }}}}
          [:nodes
           [:id
            :title
            :updatedAt
            :completedAt
            :url
            :identifier
            :state
            [:id :name]]]]]
        (clj->gql)
        (graphql-query)
        (get-in [:data :viewer :assignedIssues :nodes])))

(defn in-review-issues
  []
  (p/-> [:viewer
         [:id
          :name
          :email
          :assignedIssues
          {:filter {:state {:name {:eq "In Review" }}}}
          [:nodes
           [:id
            :title
            :updatedAt
            :completedAt
            :url
            :identifier
            :state
            [:id :name]]]]]
        (clj->gql)
        (graphql-query)
        (get-in [:data :viewer :assignedIssues :nodes])))

(defn completed-issues
  "
  Get the tickets that are now done and completed a day ago or earlier
  "
  []
  (p/-> [:viewer
         [:id
          :name
          :email
          :assignedIssues
          {:filter {:state {:name {:eq "Done" }}
                    :completedAt {:gt "-P1D"}}}
          [:nodes
           [:id
            :title
            :updatedAt
            :completedAt
            :url
            :identifier
            :state
            [:id :name]]]]]
        (clj->gql)
        (graphql-query)
        (get-in [:data :viewer :assignedIssues :nodes])))

(comment
  (p/-> (completed-issues) pprint))
