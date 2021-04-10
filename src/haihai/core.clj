(ns haihai.core
  (:require
   [clojure.java.io :as io]
   [haihai.jsoup.core :as soup]
   [lambdaisland.uri :as uri]))

(def +irasutoya-base+ "www.irasutoya.com")
(def +irasutoya-url+ (str "https://" +irasutoya-base+))

(defn copy-uri-to-file! [uri file]
  (with-open [in (io/input-stream uri)
              out (io/output-stream file)]
    (io/copy in out)))

#_(defmulti elem-fn! ())

(defn- in-domain? [domain a]
  (-> a
      (soup/attr "href")
      uri/uri
      :host
      (= domain)))

(defn crawl
  [url {domain :domain
        tags :tags
        elem-fn! :elem-fn!
        seen :seen}]
  (let [domain (or domain
                   (-> url
                       uri/uri
                       :host))
        tags (conj tags :a) ;; Can't image it could work without this
        seen (or seen
                 (->> tags
                      (map #(vector % #{}))
                      (into {})
                      atom))
        conn (soup/connection url)
        elem-groups
        (into
         {}
         (map
          (fn [tag]
            (->> tag
                 name
                 ;; Get all elements of tag type
                 (soup/extract conn)
                 ;; Remove elements that we've already seen
                 (remove #((tag @seen) (soup/elem->id %)))
                 ;; Mark seen
                 (map (fn [elem]
                        (swap! seen
                               (fn [seen]
                                 (update seen
                                         tag
                                         #(conj % (soup/elem->id elem)))))
                        elem))
                 ;; Remove links that don't stay in the domain
                 (filter #(or (not= "a" (soup/tag-name %))
                              (in-domain? domain %)))
                 ;; Place into tuple
                 (vector tag))) tags))]
    (map count (vals elem-groups))))

(defn -main [])
