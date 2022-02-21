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

#_(defn get-key-file
  []
  (-> (.readFileSync fs "googleauth.private.json" #js {:encoding "utf-8"})
      #_(js/JSON.parse)))


(def auth (new (.. google -auth -GoogleAuth)
               #js {:keyFile "googleauth.private.json"
                    :scopes #js ["https://www.googleapis.com/auth/calendar.events.readonly"]}))

(def cal-id (env/get :GOOGLE_CAL_ID))

(def calendar
  (.calendar google
             #js {:version "v3"
                  :auth auth}))

(defn format-event
  [event]
  (let [{:keys [id summary location start end conferenceData htmlLink]} event]
    {:id id
     :summary summary
     :time-zone (get start :timeZone)
     :start (new js/Date (get start :dateTime) )
     :end   (new js/Date (get end :dateTime))
     :url   htmlLink
     :location location
     :meeting-type (get-in conferenceData [:conferenceSolution :name])
     :meeting-url (->> conferenceData
                       (:entryPoints)
                       (first)
                       (:uri))}))

(defn format-events
  [{:keys [timeZone items]}]
  {:time-zone timeZone
   :items (->> items
               (sort-by #(get-in % [:start :dateTime]))
               (map format-event))})

(defn fetch-events-list
  [{:keys [min max]}]
  (p/-> (-> calendar
            (.-events)
            (.list
             #js {:calendarId cal-id
                  :timeMin min
                  :timeMax max
                  :maxResults 10
                  :singleEvents true
                  :orderBy :startTime}))
        (->clj)
        (get-in [:data])
        (format-events)
        #_(->> (map format-event))
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
        (get-in [:data])
        (->> map format-event)))

(comment

  (week)

  (-> js/Date new .toISOString)
;; => "2022-02-20T03:49:16.831Z"

  (p/-> (date/range (date/days (date/next-week :tuesday)))
        (fetch-events-list)
        (pprint))

  )
