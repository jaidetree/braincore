(ns braincore.commands.sync-cal
  (:require
   [cljs.pprint :refer [pprint]]
   [promesa.core :as p]
   [braincore.formats :as date]
   [braincore.google-cal.api :as gcal]))

(defn sync-cal-cmd
  []
  (p/-> (date/range (date/days (date/next-week :monday)))
        (gcal/fetch-events-list)
        (pprint)))
