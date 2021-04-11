(ns haihai.core
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [haihai.jsoup.core :as soup]
   [lambdaisland.uri :as uri]
   [clojure.string :as str]))

(defn- in-domain? [domain a]
  (-> a
      (soup/attr "href")
      uri/uri
      :host
      (= domain)))

(defn- backoff
  ([f]
   (backoff f 3 2))
  ([f max-attempts]
   (backoff f max-attempts 2))
  ([f max-attempts wait-interval]
   (loop [attempts 1]
     (if-let [res (try (f)
                       (catch Exception e
                         (log/warnf "Failed %d times, error: %s" attempts e)
                         (when (>= attempts max-attempts)
                           (throw e))
                         nil))]
       res
       (do (Thread/sleep (* attempts wait-interval))
           (recur (inc attempts)))))))

(defmulti pre-process soup/tag-name)
(defmethod pre-process :default [x] x)

(defmulti elem-fn! soup/tag-name)
(defmethod elem-fn! :default [x] x)

(defn crawl
  [url {:keys [elem-fn! depth depth-limit domain seen tags]
        :as opts
        :or {depth 0}}]
  (log/infof "Starting on URL: %s" url)
  (let [domain (or domain
                   (-> url
                       uri/uri
                       :host))
        tags (conj tags :a) ;; Can't image it could work without this
        seen (or seen (atom #{}))
        conn (backoff #(soup/connection url))
        elems
        (apply
         concat
         (map
          (fn [tag]
            (->> tag
                 name
                 ;; Get all elements of tag type
                 (soup/extract conn)
                 ;;
                 (map pre-process)
                 ;; Remove elements that we've already seen
                 (remove #(@seen (soup/elem->id %)))
                 ;; Mark seen
                 (map (fn [elem]
                        (swap! seen conj (soup/elem->id elem))
                        elem))
                 ;; Remove links that don't stay in the domain
                 (filter #(or (not= "a" (soup/tag-name %))
                              (in-domain? domain %)))))
          tags))]
    (doall (pmap elem-fn! elems))
    (when (or (and depth-limit (< depth depth-limit))
              (not depth-limit))
      (->> elems
           (filter #(-> % soup/tag-name (= "a")))
           (map #(soup/attr % "href"))
           (map #(crawl % (assoc opts
                                  :depth (inc depth)
                                  :seen seen)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Irasutoya Example

(def +irasutoya-url+ "https://www.irasutoya.com")
(def +irasutoya-uri-comps+ (uri/uri +irasutoya-url+))

;; Expand relative paths
(defmethod pre-process "a"
  [a]
  (let [href-curr (soup/attr a "href")]
    (cond (str/starts-with? href-curr "//")
          (soup/attr a "href" (format "%s:%s"
                                      (:scheme +irasutoya-uri-comps+)
                                      href-curr))
          (str/starts-with? href-curr "/")
          (soup/attr a "href" (format "%s://%s%s"
                                      (:scheme +irasutoya-uri-comps+)
                                      (:host +irasutoya-uri-comps+)
                                      href-curr)))
    #_(log/infof "Href: %s -> %s" href-curr (soup/attr a "href"))
    a))

(defn copy-uri-to-file! [uri file]
  (try
    (with-open [in (io/input-stream uri)
                out (io/output-stream file)]
      (log/infof "Writing to URI -> %s" uri file)
      (io/copy in out))
    (catch Exception e
      (log/warnf "Could not write file at URI %s" uri))))

(defmethod elem-fn! "img" [img]
  (let [img-src (soup/attr img "src")
        img-src (if (str/starts-with? img-src "//")
                  (str (:scheme +irasutoya-uri-comps+) img-src)
                  img-src)
        image-name (-> img-src uri/uri :path (str/split #"/") last)
        image-path (str "output/" image-name)]
    (when-not (java.io.File. image-path)
      (copy-uri-to-file! img-src image-path))))

(defn -main []
  (.mkdir (java.io.File. "output"))
  (crawl +irasutoya-url+
         {:tags [:img]
          :elem-fn! elem-fn!}))
