#!/bin/env /home1/jayzawro/bin/bb

(ns cgi.dispatch
  (:require
   [clojure.pprint :refer [pprint]]
   [babashka.classpath :refer [add-classpath]]
   [babashka.fs :as fs]
   [babashka.pods :as pods]
   [babashka.curl :as curl]
   [cheshire.core :as json]
   [clojure.java.shell :refer [sh]]
   [clojure.edn :as edn]
   [clojure.string :as s]
   [hiccup2.core :refer [html]]))

;; Dynamic Libs
(def LIB-DIR "/home1/jayzawro/lib/")
(def CWD
  (if-let [filename (System/getenv "SCRIPT_FILENAME")]
    (str (fs/parent filename))
    (System/getenv "PWD")))


(defn lib
  "
  Create an absolute path to a jar file in sibling lib directory
  Takes a string filename like \"honeysql.jar\"
  Returns a string like \"/path/to/dir/lib/honeysql.jar\".
  "
  [path]
  (str LIB-DIR path))

;; Add jars and current directory to classpath to import library code

(add-classpath (s/join ":" [CWD
                            (lib "gaka.jar")]))



;; (println "Content-type:text/html\r\n")
;; (println "Content-type:application/json\r\n")

(def raw-postbody (slurp *in*))

(def post-data
  (->> (s/split (s/trim raw-postbody) #"&")
       (map #(s/split % #"="))
       (filter (fn [[k v]]
                 (and (some? k) (some? v))))
       (map (fn [[k v]]
              [(keyword k) v]))
       (into {})))

(def request
  {:throw   false
   :headers {"Accept" "application/vnd.github.v3+json"
             "Content-Type" "application/json"
             "Authorization" (str "Token " (:api_token post-data))}
   :raw-args ["--data" (json/generate-string
                        {:event_type (:event_type post-data)})]})

(def response
  (curl/post
   (str "https://api.github.com/repos/" (:owner post-data) "/" (:repo post-data) "/dispatches")
             request))

;; (println (str (json/generate-string request)))
;; (println (str (json/generate-string {:body (:body response)
;;                                      :status (:status response)})))
;; (println (str (json/generate-string {:input post-data})))

(println (str "Location: https://eccentric-j.com/braincore/widgets/index.html?" raw-postbody "\r\n"))

(System/exit 0)
