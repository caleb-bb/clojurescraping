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

(defn generate-filename [date domain title]
  (clojure.string/join "" [date "-"  domain "-" title ".txt"]))

(defn save-article [date domain title text]
  (def filename (generate-filename date domain title))
  (spit filename text))

(defn get-html [url]
  (get (client/get url) :body))

(defn nyt-get-html [params]
  (as-> params p
        (clojure.string/join ["https://api.nytimes.com/svc/search/v2/articlesearch.json?q=" p "&api-key=wGNUhgyB28zUKs7VIfyy4mjuQm3EPXMN"])
        (client/get p)
        (get p :body)))

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

(defn get-title-vec [hickory-struct]
  (->
   (retrieve-text hickory-struct s/tag :a)
   (all-clean-text "!!!!!")
   (clojure.string/replace #" " "_")
   (clojure.string/split #"!!!!!")))

(defn get-domain-vec [domain-name length]
  (->> domain-name
       (repeat length)
       (vec)))

(defn get-filename-vec [date-vec domain-vec title-vec]
  (map generate-filename date-vec domain-vec title-vec))

(defn get-url [item]
  (-> item
      (get :attrs)
      (get :href)))

(defn extract-date-from-url [raw-url]
  (as-> raw-url X
        (clojure.string/split X  #"/")
        (subvec X 1 4)
        (clojure.string/join "-" X)))

(defn complete-url [incomplete-url]
  (->> incomplete-url
       (vector "https://www.nytimes.com")
       (clojure.string/join)))

(defn clean-url [raw-url]
  (-> raw-url
      (clojure.string/split #"\?")
      (first)))

(defn finished-url [raw-url]
  (-> raw-url
      (complete-url)
      (clean-url)))

(defn freq-map [gotten-text]
  (->> gotten-text
       (re-seq #"[\w|’|']*")
       (map clojure.string/lower-case)
       (remove commonwords)
       (frequencies)))

(defn n-most-common [mapped-text n]
  (->> mapped-text
       (sort-by val #(compare %2 %1))
       (take n)))

(defn top-ten [mapped-text]
  (n-most-common mapped-text 10))

;these are just some pre-defined values for use in the REPL when testing/developing
(def url "https://www.nytimes.com/search?dropmab=true&endDate=20200801&query=&sort=best&startDate=20200401")
(def hick-struct (url-to-hickory url))
(def scraped (get-text hick-struct))
(def lynx (map get-url (retrieve-text hick-struct s/tag :a)))
