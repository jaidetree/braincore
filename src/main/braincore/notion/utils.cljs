(ns braincore.notion.utils)

(defn entry?
  [block]
  (and (= (:type block) "heading_1")
       (true? (:has_children block))))
