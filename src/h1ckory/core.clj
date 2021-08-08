(ns h1ckory.core)

(require '[clj-http.lite.client :as client])
(require '[clojure.string :as string])
(require '[hickory.select :as s])

(use 'hickory.core)

(defn get-html [url]
    (get (client/get url) :body))

(defn hickory-this [html]
  (as-hickory (parse html)))

(defn url-to-hickory [url]
  (-> url
      (get-html)
      (hickory-this)))

(defmacro retrieve-text [url identifier value]
  (list s/select
        (list s/descendant
        (list identifier value))
        (list url-to-hickory url)))

(defn get-guardian-text [url]
    (retrieve-text url s/tag :p))

(defn get-guardian-links [url]
  (->> url
       (url-to-hickory)
       (s/select (s/descendant (s/tag :a)))))

;tree is just a test case for developing the guardian scraper
(def tree (get-guardian-text "https://www.theguardian.com/world/2021/aug/06/four-areas-where-what-is-known-about-the-covid-virus-has-evolved"))

(defn clean-text [hickory-vect]
  (cond
    (= (type hickory-vect) clojure.lang.PersistentVector) (clean-text (get hickory-vect 0))
    (= (type hickory-vect) clojure.lang.PersistentArrayMap) (clean-text (get hickory-vect :content))
    (= (type hickory-vect) java.lang.String) hickory-vect))

(defn all-clean-text [hickory-vect]
  (->> hickory-vect
       (map clean-text)
       (apply str)))

;this one also works for the NY Post
;(defn retrieve-from-federalist [url]
;  (s/select
;   (s/descendant 
;     (s/tag :p))
;   (url-to-hickory url)))

;(defn retrieve-from-vox [url]
;  (s/select
;   (s/class "c-entry-content")
;   (url-to-hickory url)))



;;The next step is to add something between hic-this and retrieve-content to find stuff based on tag name/id/class. Read about map, filter, reduce, and apply in the clojure docs.

(defn retrieve-content [hicvec]
  (-> hicvec (flatten) (last)))
