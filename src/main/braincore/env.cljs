(ns braincore.env
  (:refer-clojure :exclude [get])
  (:require
   ["dotenv/config"]
   [cljs-bean.core :refer [bean]]))

(defn get
  "
  Get the environment variable for `varname`. If not found throws error

  Takes a keyword varname like :NOTION_API_KEY
  Returns the value if present in js/process.env.NOTION_API_KEY
  "
  [varname]
  (let [env (bean js/process.env)
        value (clojure.core/get env varname ::not-found)]
    (if (= value ::not-found)
      (throw (new js/Error (str "Could not find env var " varname)))
      value)))
