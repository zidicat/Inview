(ns com.zidicat.inview.parse-html
  (:require [clojure.java.io :as io])
  (:import [javax.xml.parsers SAXParserFactory SAXParser]
           [java.io ByteArrayInputStream]
           [org.xml.sax Attributes]
           [org.xml.sax.ext DefaultHandler2]))

(set! *warn-on-reflection* true)

(defn- attr->map [^Attributes attr]
  (let [tx (map #(vector (keyword (.getLocalName attr %)) (.getValue attr ^long %)))]
    (into {} tx (range (.getLength attr)))))

(defn- merge-end [stack]
  (let [new-child (peek stack)
        new-stack (when (seq stack) (pop stack))]
    (if (seq new-stack)
      (conj (pop new-stack) (conj (peek new-stack) new-child))
      (peek stack))))

(defn- add-child [stack content]
  (conj (pop stack) (conj (peek stack) content)))

(defn- create-parser []
  (let [stack   (volatile! [])
        doctype (volatile! [])
        handler (proxy [DefaultHandler2]
                    []
                  (startElement [uri localName qName attributes]
                    (vswap! stack conj [(keyword qName) (attr->map attributes)]))
                  (endElement [uri localName qName]
                    (vswap! stack merge-end))
                  (characters [ch start length]
                    (vswap! stack add-child (String. ^chars ch ^int start ^int length)))
                  (startDTD [n public-id system-id]
                    (vswap! doctype into (remove nil?) [n public-id system-id])))
        parser  (.newSAXParser (SAXParserFactory/newInstance))]
    (.setProperty parser "http://xml.org/sax/properties/lexical-handler" handler)
    {:stack   stack
     :doctype doctype
     :handler handler
     :parser  parser }))

(defn parse-file [^java.io.File input]
  (let [{:keys [stack doctype handler ^SAXParser parser]} (create-parser)]
    (.parse parser input ^DefaultHandler2 handler)
    (cond-> @stack
      (seq @doctype) (with-meta {:com.zidicat.inview/doctype @doctype}))))

(defn parse-stream [^java.io.InputStream input]
  (let [{:keys [stack doctype handler ^SAXParser parser]} (create-parser)]
    (.parse parser input ^DefaultHandler2 handler)
    (cond-> @stack
      (seq @doctype) (with-meta {:com.zidicat.inview/doctype @doctype}))))

(defprotocol ParseSource
  (parse* [input]))

(extend-protocol ParseSource
  java.io.File
  (parse* [input] (parse-file input))

  java.io.InputStream
  (parse* [input] (parse-stream input))

  java.io.Reader
  (parse* [input] (parse-stream (io/input-stream input)))

  java.lang.String
  (parse* [input] (parse-stream (ByteArrayInputStream. (.getBytes input))))

  clojure.lang.PersistentVector
  (parse* [input] input))

(defn parse [input]
  (parse* input))




(comment

  (let [render-attrs (fn [attrs]
                       (some->> (not-empty attrs)
                                (map #(str (name (key %)) "=\"" (val %) "\""))
                                (interpose " ")
                                (apply str " ")))
        render (fn render [dom]
                 (if (sequential? dom)
                   (let [[tag attrs & children] dom
                         doctype                (-> dom meta :com.zidicat.inview/doctype)]
                     (flatten [(when doctype ["<!DOCTYPE " (interpose " " doctype) ">"])
                               (str "<" (name tag) (render-attrs attrs) (if (seq children) ">" " />" ))
                               (map render children)
                               (when (seq children) (str "</" (name tag) ">"))]))
                   dom))]

    #_ (println (apply str (render [:html {} [:body {} [:p {:class "para"} "test"]]])))
    (apply str (render (parse (java.io.File. "htdocs/snippets/login.html")))))

  (parse "<!DOCTYPE html [<!ENTITY copy \"&#169;\"><!ENTITY nbsp \"&#160;\" >]>
  <html>
    <head>
      <title>Title</title>
    </head>
    <body>
      <div class=\"header\">
        <h1>Title</h1>
        <div class=\"nav\">
          <ol>
            <li><a class=\"home\">Home</a></li>
          </ol>
        </div>
      </div>
      <div class=\"main\">
        <div class=\"content\" />
      </div>
      <div class=\"footer\">
        <p>&copy; &nbsp; 2001 A Space Oddessy</p>
      </div>
    </body>
  </html>")
  )
