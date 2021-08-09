(ns h1ckory.core)

(require '[clj-http.lite.client :as client])
(require '[clojure.string :as string])
(require '[hickory.select :as s])

(use 'hickory.core)

(defn current-date []
  (.format 
    (new java.text.SimpleDateFormat "yyyy-MM")
    (java.util.Date.)))

(defn generate-filename [domain]
  (clojure.string/join "" [(current-date) "-"  domain]))

(defn get-html [url]
    (get (client/get url) :body))

(defn hickory-this [html]
  (as-hickory (parse html)))

(defn url-to-hickory [url]
  (-> url
      (get-html)
      (hickory-this)))

(defn clean-text [hickory-struct]
  (cond
    (vector? hickory-struct) (clean-text (first hickory-struct))
    (map? hickory-struct) (clean-text (get hickory-struct :content))
    (string? hickory-struct) hickory-struct
    (nil? hickory-struct)  ""))

(defn all-clean-text [hickory-struct]
  (->> hickory-struct
       (map clean-text)
       (apply str)))

(defmacro retrieve-text [hickory-struct identifier value]
  (list s/select
        (list s/descendant
        (list identifier value))
        hickory-struct))

(defn get-text [hickory-struct]
    (->
     (retrieve-text hickory-struct s/tag :p)
     (all-clean-text)))

(defn get-links [hickory-struct]
    (->
       (retrieve-text hickory-struct s/tag :a)
       (all-clean-text)))

