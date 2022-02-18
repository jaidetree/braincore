(ns braincore.formats)

(defn format-date
  [fmt ^js/Date date]
  (let [fmttr (new js/Intl.DateTimeFormat 'en-US' (clj->js fmt))]
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
