(ns h1ckory.core)

(require '[clj-http.lite.client :as client])
(require '[clojure.string :as string])
(require '[hickory.select :as s])

(use 'hickory.core)

(defn current-date-time []
     (clojure.string/split (.toString (java.util.Date.)) #" "))
        
(defn destruct [six-arity-vec]
  (let [[_ a b _ _ c] six-arity-vec]
    [(clojure.string/join [a "."]) (clojure.string/join [b "."]) c]))

(defn generate-filename [date-time domain]
  (def filename (apply str (destruct (current-date-time))))
  (clojure.string/join "" [filename "-" domain ".txt"]))


(defn get-html [url]
    (get (client/get url) :body))

(defn hickory-this [html]
  (as-hickory (parse html)))

(defn url-to-hickory [url]
  (-> url
      (get-html)
      (hickory-this)))

(defn clean-text [hickory-vect]
  (cond
    (= (type hickory-vect) clojure.lang.PersistentVector) (clean-text (get hickory-vect 0))
    (= (type hickory-vect) clojure.lang.PersistentArrayMap) (clean-text (get hickory-vect :content))
    (= (type hickory-vect) java.lang.String) hickory-vect
    (= (type hickory-vect) nil) ""))

(defn all-clean-text [hickory-vect]
  (->> hickory-vect
       (map clean-text)
       (apply str)))

(defmacro retrieve-text [url identifier value]
  (list s/select
        (list s/descendant
        (list identifier value))
        (list url-to-hickory url)))

(defn get-text [url]
    (->
     (retrieve-text url s/tag :p)
     (all-clean-text)))

(defn get-links [url]
    (->
       (retrieve-text url s/tag :a)
       (all-clean-text)))

