(ns haihai.jsoup.core
  (:import org.jsoup.Jsoup
           org.jsoup.nodes.Element))

(defn connection [url]
  (.get (Jsoup/connect url)))

(defn extract [connection selector]
  (.select connection selector))

(defn attr [elem a]
  (.attr elem a))

(defn tag-name [elem]
  (.tagName elem))

(defmulti elem->id tag-name)
(defmethod elem->id "img" [e] (attr e "src"))
(defmethod elem->id "a" [e] (attr e "href"))
