(ns haihai.core
  (:require
   [clojure.tools.logging :as log]
   [clojure.java.io :as io]
   [haihai.jsoup.core :as soup]
   [lambdaisland.uri :as uri]
   [clojure.string :as str]))

(defn- allowed-domain? [domains a]
  (let [testing (-> a (soup/attr "href") uri/uri :host)
        res (contains? domains testing)]
    (when-not res
      (log/debugf "Filtered out domain %s" testing))
    res))

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
                         nil))]
       res
       (if (>= attempts max-attempts)
         (log/warn "Reached max attempts. Quitting.")
         (do (Thread/sleep (* attempts wait-interval))
             (recur (inc attempts))))))))

(defmulti pre-process soup/tag-name)
(defmethod pre-process :default [x] x)

(defmulti elem-fn! soup/tag-name)
(defmethod elem-fn! :default [x] x)

(defn- loops-back? [domains]
  (fn [url]
    (let [{host :host path :path} (uri/uri url)]
      (and (= path "/")
           (domains host)))))

(defn crawl
  [url {:keys [elem-fn! depth depth-limit domains ignore-domain seen tags]
        :as opts
        :or {depth 0}}]
  (log/debugf "Start processing %s." url)
  (let [domains (or domains
                    (-> url
                        uri/uri
                        :host
                        set))
        tags (conj (set tags) :a) ;; Can't image it could work without this
        seen (or seen (atom #{}))]
    (if-let [conn (backoff #(soup/connection url))]
      (let [elems
            (apply
             interleave
             (pmap
              (fn [tag]
                (->> tag
                     name
                     ;; Get all elements of tag type
                     (soup/get-elements-by-tag conn)
                     ;; Apply pre-processing
                     (map pre-process)
                     (filter some?)
                     ;; Get a little weird
                     #_shuffle
                     ;; Remove elements that we've already seen
                     (remove #(@seen (soup/elem->id %)))
                     ;; Mark seen
                     (map (fn [elem]
                            (swap! seen conj (soup/elem->id elem))
                            elem))
                     ;; Optionally remove links that don't stay in the allowed domains
                     (filter #(or (not= "a" (soup/tag-name %))
                                  (or ignore-domain
                                      (allowed-domain? domains %))))))
              tags))]
        (doall (pmap elem-fn! elems))
        (if (or (and depth-limit (< depth depth-limit))
                (not depth-limit))
          (->> elems
               (filter #(-> % soup/tag-name (= "a")))
               ;; Attempt to catch home-page links
               (remove (loops-back? domains))
               (map #(soup/attr % "href"))
               (map #(crawl % (assoc opts
                                     :depth (inc depth)
                                     :seen seen))))
          (log/debug "Depth limit reached. Ending search.")))
      (log/warnf "Unable to connect to URL %s. Skipping" url))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Irasutoya Example


(def +irasutoya-url+ "https://www.irasutoya.com")
(def +irasutoya-uri-comps+ (uri/uri +irasutoya-url+))

(defn copy-uri-to-file! [uri file]
  (try
    (with-open [in (io/input-stream uri)
                out (io/output-stream file)]
      (log/infof "[WRITE] -> %s" file)
      (io/copy in out))
    (catch Exception e
      (log/warnf "Could not write file at URI %s, Error: %s" uri e))))

(defn- is-some-bullshit? [image-name]
  (or (str/starts-with? image-name "twitter")
      (str/starts-with? image-name "line_")
      (str/includes? image-name "searchbtn")
      (str/includes? image-name "sidebar")
      (str/includes? image-name "button")
      (str/includes? image-name "navigation")))

(defn- is-character? [image-name]
  (or (str/starts-with? image-name "alphabet_")
      (str/starts-with? image-name "capital_")
      (str/starts-with? image-name "lower_")
      (str/starts-with? image-name "hiragana_")
      (str/starts-with? image-name "katakana_")
      (str/starts-with? image-name "number_")
      (str/starts-with? image-name "hoka_")
      (str/starts-with? image-name "hoka2_")
      (str/starts-with? image-name "roman_number")
      (str/starts-with? image-name "number_kanji")
      (str/starts-with? image-name "paint_lower_")
      (str/starts-with? image-name "paint_capital_")
      (str/starts-with? image-name "paint_hiragana")
      (str/starts-with? image-name "paint_katakana")
      (str/starts-with? image-name "paint_number_")
      (str/starts-with? image-name "paint_hoka")))

(defn process-image [uri]
  (let [uri (if (str/starts-with? uri "//")
              (str (:scheme +irasutoya-uri-comps+) ":" uri)
              uri)
        image-name (-> uri uri/uri :path (str/split #"/") last)
        image-folder (cond
                       (str/includes? image-name "banner") "banners"
                       (str/includes? image-name "icon") "icons"
                       (str/includes? image-name "logo") "logos"
                       (str/includes? image-name "thumbnail") "thumbnails"
                       (is-character? image-name) "characters"
                       (is-some-bullshit? image-name) "bs"
                       :else "main")
        image-path (str "output/" image-folder "/" image-name)]
    (when-not (.exists (java.io.File. image-path))
      (copy-uri-to-file! uri image-path))))

(defn- process-href-image [href]
  (process-image href))

(defn- process-img-image [img]
  (process-image (soup/attr img "src")))

(defn- is-image? [uri]
  (let [uri (str/lower-case uri)]
    (or
     (str/ends-with? uri ".png")
     (str/ends-with? uri ".jpeg")
     (str/ends-with? uri ".jpg"))))

;; Expand relative paths
(defmethod pre-process "a"
  [a]
  (let [href-curr (soup/attr a "href")]
    (cond
      (str/starts-with? href-curr "//")
      (soup/attr a "href" (format "%s:%s"
                                  (:scheme +irasutoya-uri-comps+)
                                  href-curr))

      (str/starts-with? href-curr "/")
      (soup/attr a "href" (format "%s://%s%s"
                                  (:scheme +irasutoya-uri-comps+)
                                  (:host +irasutoya-uri-comps+)
                                  href-curr))

      (str/starts-with? href-curr "#")
      nil)
    (if (is-image? href-curr)
      (do (process-href-image href-curr) nil)
      a)))

(defmethod elem-fn! "img" [img]
  (process-img-image img))

(defn -main [& _]
  (.mkdir (java.io.File. "output"))
  (.mkdir (java.io.File. "output/banners"))
  (.mkdir (java.io.File. "output/icons"))
  (.mkdir (java.io.File. "output/logos"))
  (.mkdir (java.io.File. "output/characters"))
  (.mkdir (java.io.File. "output/thumbnails"))
  (.mkdir (java.io.File. "output/bs"))
  (.mkdir (java.io.File. "output/main"))
  (crawl +irasutoya-url+
         {:tags #{"img"}
          :domains #{"www.irasutoya.com"
                     "irasutoya.com"
                     "www.blogger.com"
                     "b.hatena.jp"
                     "1.bp.blogspot.com"
                     "2.bp.blogspot.com"
                     "3.bp.blogspot.com"
                     "4.bp.blogspot.com"
                     "draft.blogger.com"
                     "irasutoya-sear.ch"}
          :elem-fn! elem-fn!}))
