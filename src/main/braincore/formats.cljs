(ns braincore.formats
  (:require
   [clojure.string :as s]))

(defn format-date
  [fmt ^js/Date date]
  (let [fmttr (new js/Intl.DateTimeFormat "en-US" (clj->js fmt))]
    (.format fmttr date)))

(defn full-date
  "
  Returns a date string like \"Tuesday, Feb 16, 2022\"
  "
  ([]
   (full-date (new js/Date)))
  ([date]
   (format-date {:weekday :long
                 :month :short
                 :day :numeric
                 :year :numeric} date)))

(defn time
  [time-zone date]
  (format-date {:timeStyle :short
                :timeZone time-zone}
               date))

(defn date->map
  [date]
  (let [keys [:year :month :day :hour :minute :second :nano]
        vals (-> date
                 (.toISOString)
                 (s/split #"-|T|:|\.|Z"))]
    (zipmap keys (map js/Number vals))))

(comment
  (date->map (new js/Date)))

(defn pad
  [n {:keys [fill min]}]
  (let [number-str (str n)
        len (count number-str)]
    (if (< len min)
      (str (s/join "" (repeat (- min len) fill)) number-str)
      number-str)))

(defn iso
  [{:keys [year month day hour minute second nano]}]
  (let [f2 #(pad %1 {:fill "0" :min 2})
        f3 #(pad %1 {:fill "0" :min 3})]
    (str year \- (f2 month) \- (f2 day) \T
         (f2 hour) \: (f2 minute) \: (f2 second) \. (f3 nano)
         \Z)))


(defn today
  []
  (let [dt (date->map (new js/Date))]
    {:min (iso (assoc dt
                      :hour   0
                      :minute 0
                      :second 0
                      :nano   0))
     :max (iso  (assoc dt
                       :hour   23
                       :minute 59
                       :second 59
                       :nano   999))}))

(defn offset
  [n unit]
  (case unit
    :weeks   (* n 1000 60 60 24 7)
    :days    (* n 1000 60 60 24)
    :hours   (* n 1000 60 60)
    :minutes (* n 1000 60)
    :seconds (* n 1000)))

(defn start-of-day
  [date]
  (-> date
      (date->map)
      (assoc :hour   0
             :minute 0
             :second 0
             :nano   0)
      (iso)))

(defn end-of-day
  [date]
  (-> date
      (date->map)
      (assoc :hour   23
             :minute 59
             :second 59
             :nano   999)
      (iso)))

(defn range
  ([offset]
   (range offset offset))
  ([start end]
   (let [now (js/Date.now)
         start-dt (new js/Date (+ now start))
         end-dt   (new js/Date (+ now end))]
     {:min (start-of-day start-dt)
      :max (end-of-day end-dt)})))

(defn day-of
  [date]
  {:min (start-of-day date)
   :max (end-of-day   date)})

(defn week
  []
  (let [d (new js/Date)
        days (.getDay d)]
    (range
     (offset (* days -1) :days)
     (offset (- 6 days) :days))))

(defn day
  []
  (range 0 0))

(defn days
  [n]
  (offset n :days))

(defn day-of-week
  [day-kw]
  (case day-kw
    :sunday    0
    :monday    1
    :tuesday   2
    :wednesday 3
    :thursday  4
    :friday    5
    :saturday  6))

(defn last-week
  [day-kw]
  (let [target-day (day-of-week day-kw)
        date (new js/Date)
        day (.getDay date)]
    (if (< target-day day)
      (- day target-day)
      (- target-day (+ 7 day)))))

(defn next-week
  [day-kw]
  (let [target-day (day-of-week day-kw)
        date (new js/Date)
        day (.getDay date)]
    (if (< target-day day)
      (- (+ 7 target-day) day)
      (- target-day day))))

(defn parse
  [date-str]
  (new js/Date date-str))

(comment
  (day)
  (week)

  (last-week :sunday)
  (last-week :monday)
  (last-week :tuesday)
  (last-week :wednesday)
  (last-week :thursday)
  (last-week :friday)
  (last-week :saturday)

  (next-week :sunday)
  (next-week :monday)
  (next-week :tuesday)
  (next-week :wednesday)
  (next-week :thursday)
  (next-week :friday)
  (next-week :saturday)

  (let [current (day-of-week :tuesday)
        target (day-of-week :monday)]
    (- (+ 7 target) current))

  ;; 3 days ago
  (range (days -3) 0)

  (range (days (last-week :wednesday)))
  (range (days (next-week :tuesday)))

  )
