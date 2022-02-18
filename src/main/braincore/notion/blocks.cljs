(ns braincore.notion.blocks)

(defn text
  ([content]
   (text {} content))
  ([{:keys [bold italic strikethrough underline code color href]} content]
   {:type :text
    :text (->> [(when href {:link {:url href}})]
               (into {:content content}))
    :annotations {:bold (true? bold)
                  :italic (true? italic)
                  :strikethrough (true? strikethrough)
                  :underline (true? underline)
                  :code (true? code)
                  :color (or color "default")}
    :href href
    :plain_text content}))

(comment
  (->> [(when false
         {:link {:url :some-link}})]
       (into {:content :some-content})))

(defn heading-1
  [title children]
  {:object       :block
   :type         :heading_1
   :heading_1 {:text [(text {} title)]
               :children children}})

(defn heading-2
  [title children]
  {:object       :block
   :type         :heading_2
   :heading_2 {:text [(text {} title)]
               :children (or children [])}})

(defn heading-3
  [title children]
  {:object       :block
   :type         :heading_3
   :heading_3 {:text [(text {} title)]
               :children (or children [])}})

(defn column
  [children]
  {:object :block
   :type :column
   :column {:children children}})

(defn columns
  [children]
  {:object :block
   :type   :column_list
   :column_list {:children children}})

(defn paragraph
  [children]
  {:object :block
   :type :paragraph
   :paragraph {:text children}})

(defn toggle
  [title]
  {:object :block
   :type :toggle
   :toggle {:text [(text {} title)]}})

(defn divider
  []
  {:object :block
   :type :divider
   :divider {}})

(defn bullet
  [children]
  {:object :block
   :type :bulleted_list_item
   :bulleted_list_item {:text children}})

(defn todo
  [children]
  {:object :block
   :type "to_do"
   :to_do {:text children}})
