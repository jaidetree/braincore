(ns braincore.google-cal.api
  (:require
   [cljs.pprint :refer [pprint]]
   [clojure.string :as s]
   [braincore.env :as env]
   [braincore.formats :as date]
   [cljs-bean.core :refer [bean ->clj]]
   [promesa.core :as p]
   ["fs" :as fs]
   ["googleapis" :refer [google]]))

;; (defn get-auth-file
;;   []
;;   (->> (.readFileSync fs "googleauth.private.json" #js {:encoding "utf-8"})
;;        (js/JSON.parse)))

;; const auth = new google.auth.GoogleAuth({
;;   keyFile: '/path/to/your-secret-key.json',
;;   scopes: ['https://www.googleapis.com/auth/cloud-platform'],
;; });
;;

(defn get-key-file
  []
  (-> (.readFileSync fs "googleauth.private.json" #js {:encoding "utf-8"})
      #_(js/JSON.parse)))

(println "")
(-> (get-key-file)
    (println "GOOGLE_CAL_KEY_FILE\n"))

(comment
  (get-key-file))

#_(def auth-key (-> (env/get :GOOGLE_CAL_KEY)
                  (s/replace #"\\" "")
                  (s/replace #"\n", "\\n")
                  #_(js/JSON.parse)))

#_(js/console.log auth-key)

(def auth (new (.. google -auth -GoogleAuth)
               #js {:keyFile "googleauth.private.json"
                    :scopes #js ["https://www.googleapis.com/auth/calendar.events.readonly"]}))

(def cal-id (env/get :GOOGLE_CAL_ID))

(def calendar
  (.calendar google
             #js {:version "v3"
                  :auth auth}))

(defn fetch-events-list
  [{:keys [min max]}]
  (p/-> (-> calendar
            (.-events)
            (.list
             #js {:calendarId cal-id
                  :timeMin min
                  :timeMax max
                  :maxResults 10
                  :singleEvents true}))
        (->clj)
        (get-in [:data :items])
        #_(pprint)))

(defn fetch-event
  [{:keys [id]}]
  (p/-> (-> calendar
            (.-events))
        (.get
         #js {:calendarId cal-id
              :eventId id
              :timeZone "America/New_York"})
        (->clj)
        (get-in [:data])))


(comment

  (week)

  (-> js/Date new .toISOString)
;; => "2022-02-20T03:49:16.831Z"

  (p/-> (date/range (date/days (date/next-week :monday)))
        (fetch-events-list)
        (pprint))

  )
