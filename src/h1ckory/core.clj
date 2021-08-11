(ns h1ckory.core)

(require '[clj-http.lite.client :as client])
(require '[clojure.string :as string])
(require '[hickory.select :as s])

(use 'hickory.core)

(def commonwords #{"" "the" "of" "and" "a" "to" "in" "is" "you" "that" "it" "he" "was" "for" "on" "are" "as" "with" "his" "they" "I" "at" "be" "this" "have" "from" "or" "one" "had" "by" "word" "but" "not" "what" "all" "were" "we" "when" "your" "can" "said" "there" "use" "an" "each" "which" "she" "do" "how" "their" "if" "will" "up" "other" "about" "out" "many" "then" "them" "these" "so" "some" "her" "would" "make" "like" "him" "into" "time" "has" "look" "two" "more" "write" "go" "see" "number" "no" "way" "could" "people" "my" "than" "first" "water" "been" "call" "who" "oil" "its" "now" "find" "long" "down" "day" "did" "get" "come" "made" "may" "part"})

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
    (vector? hickory-struct) (recur (first hickory-struct))
    (map? hickory-struct) (recur (get hickory-struct :content))
    (string? hickory-struct) hickory-struct
    :else  ""))

(defn all-clean-text [hickory-struct separator]
  (->> hickory-struct
       (map clean-text)
       (string/join separator)))

(defmacro retrieve-text [hickory-struct identifier value]
  (list s/select
        (list s/descendant
              (list identifier value))
        hickory-struct))

(defn get-text [hickory-struct]
  (->
   (retrieve-text hickory-struct s/tag :p)
   (all-clean-text " ")))

(defn get-links [hickory-struct]
  (remove clojure.string/blank?
          (->
           (retrieve-text hickory-struct s/tag :a)
           (all-clean-text "!!!!!")
           (clojure.string/split #"!!!!!"))))

(defn freq-map [gotten-text]
  (->> gotten-text
       (re-seq #"[\w|’|']*")
       (map clojure.string/lower-case)
       (remove commonwords)
       (frequencies)))

(defn n-most-common [mapped-text n]
  (->> mapped-text
       (sort-by val)
       (reverse)
       (take n)))

(defn top-ten [mapped-text]
  (n-most-common mapped-text 10))

;these are just some pre-defined values for use in the REPL when testing/developing
(def url "https://www.nytimes.com/2021/08/10/nyregion/andrew-cuomo-resigns.html")
(def hick-struct (url-to-hickory url))
(def scraped (get-text hick-struct))
