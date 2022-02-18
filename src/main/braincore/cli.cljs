(ns braincore.cli
  (:require
   ["dotenv/config"]
   [braincore.commands.create-entry :refer [create-entry-cmd]]
   [braincore.commands.sync-tickets :refer [sync-tickets]]
   [braincore.commands.share-summary :refer [share-summary]]))

(defn main
  "
  Dispatch commands based on the first arg provided
  "
  [cmd & args]
  (let [cmd-kw (keyword cmd)
        cmd-fn (case cmd-kw
                 :create-entry create-entry-cmd
                 :sync-tickets sync-tickets
                 :share-summary share-summary)]
    (apply cmd-fn args)))


(comment
  (main "create-entry"))
