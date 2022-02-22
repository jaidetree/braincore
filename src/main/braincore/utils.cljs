(ns braincore.utils
  (:require
   [promesa.core :as p]
   [clojure.string :as s]
   [cljs.pprint :refer [pprint]]))


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
