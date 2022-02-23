(ns braincore.utils
  (:require
   [cljs.core :as core]
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


(defmacro with-redefs-async
  "binding => var-symbol temp-value-expr

  Temporarily redefines vars while executing the body.  The
  temp-value-exprs will be evaluated and each resulting value will
  replace in parallel the root value of its var.  After the body is
  executed, the root values of all the vars will be set back to their
  old values. Useful for mocking out functions during testing."
  [bindings & body]
  (core/let [names (take-nth 2 bindings)
             vals (take-nth 2 (drop 1 bindings))
             orig-val-syms (map (comp gensym #(core/str % "-orig-val__") name) names)
             temp-val-syms (map (comp gensym #(core/str % "-temp-val__") name) names)
             binds (map core/vector names temp-val-syms)
             resets (reverse (map core/vector names orig-val-syms))
             bind-value (core/fn [[k v]] (core/list 'alter-var-root (core/list 'var k) (core/list 'constantly v)))]
    `(let [~@(interleave orig-val-syms names)
           ~@(interleave temp-val-syms vals)]
       ~@(map bind-value binds)
       (-> (p/do! ~@body)
           (p/finally
            (fn []
              ~@(map bind-value resets))))
       )))

(defn sym-pair
  [sym]
  [(name sym) (deref (resolve sym))])
