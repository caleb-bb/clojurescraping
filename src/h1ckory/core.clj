(ns h1ckory.core)

(require '[clj-http.lite.client :as client])
(require '[clojure.string :as string])
(require '[hickory.select :as s])

(use 'hiccup.core)
(use 'hickory.core)

(defn get-html [url]
  (get (client/get url) :body))

(defn hickory-this [html]
  (as-hickory (parse html)))

(defn hiccup-this [html]
  (as-hiccup (parse html)))

(defn url-to-hickory [url]
  (-> url
      (get-html)
      (hickory-this)))

(defn url-to-hiccup [url]
  (-> url
      (get-html)
      (hicup-this)))

(defn retrieve-from-guardian [url]
  (s/select
   (s/id "maincontent")
   (url-to-hickory url)))

;this one also works for the NY Post
(defn retrieve-from-federalist [url]
  (s/select
   (s/class "entry-content")
   (url-to-hickory url)))

(defn retrieve-from-vox [url]
  (s/select
   (s/class "c-entry-content")
   (url-to-hickory url)))

;re-implement this with a map later
(defn guardian-retrieve-fragment [hickory]
  (if (= (type hickory) "clojure.lang.PersistentVector") (do guardian-retrieve-fragment (first hickory)))
  (if (= (type hickory) "clojure.lang.PersistentArrayMap") (do guardian-retrieve-fragment (get hickory :content)) hickory))

(defmacro retrieve-fragment [url identifier value]
  (list s/select
        (list identifier value)
        (list url-to-hickory url)))

;;The next step is to add something between hic-this and retrieve-content to find stuff based on tag name/id/class. Read about map, filter, reduce, and apply in the clojure docs.

(defn retrieve-content [hicvec]
  (-> hicvec (flatten) (last)))
