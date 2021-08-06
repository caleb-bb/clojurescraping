(ns h1ckory.core)

(require '[hickory.select :as s])
(require '[clj-http.lite.client :as client])
(require '[clojure.string :as string])

(use 'hickory.core)
(use 'hiccup.core)

(defn get-html 
  [url]
  (get (client/get url) :body))

(defn hickory-this
  [html]
  (as-hickory (parse html)))

(defn hiccup-this
  [html]
  (as-hiccup (parse html)))

(defn url-to-hickory
  [url]
  (->
    (get-html url)
    (hickory-this)))

(defn url-to-hiccup
  [url]
  (->
    (get-html url)
    (hiccup-this)))

(defn retrieve-from-guardian
  [url]
  (s/select
    (s/id "maincontent")
    (url-to-hickory url)))


;;The next step is to add something between hic-this and retrieve-content to find stuff based on tag name/id/class. Read about map, filter, reduce, and apply in the clojure docs.

(defn retrieve-content
  [hicvec]
  (-> (flatten hicvec) (last)))




