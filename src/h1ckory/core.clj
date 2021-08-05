(ns h1ckory.core)

(require '[hickory.select :as s])
(require '[clj-http.lite.client :as client])
(require '[clojure.string :as string])

(use 'hickory.core)
(use 'hiccup.core)

(defn get-html 
  [url]
  (get (client/get "http://google.com") :body))

(defn hicthis
  [html]
  (as-hiccup (parse html)))

(defn retrieve-content
  [hicvec]
  (-> (flatten hicvec) (last)))




