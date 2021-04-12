(ns haihai.jsoup.core
  (:import org.jsoup.Jsoup
           org.jsoup.nodes.Element))

(defn connection [url]
  (-> url
      (Jsoup/connect)
      (.followRedirects true)
      (.get)))

(defn get-elements-by-tag [root tag-name]
  (.getElementsByTag root tag-name))

(defn select [connection selector]
  (.select connection selector))

(defn attr
  ([elem to-get-attr]
   (.attr elem to-get-attr))
  ([elem to-set-attr new-attr-val]
   (.attr elem to-set-attr new-attr-val)))

(defn tag-name [elem]
  (.tagName elem))

(defmulti elem->id tag-name)
(defmethod elem->id "img" [e] (attr e "src"))
(defmethod elem->id "a" [e] (attr e "href"))
