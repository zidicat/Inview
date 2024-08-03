(ns com.zidicat.inview.ring
  (:require [com.zidicat.inview.render-as :as render]))

(defmulti doctype->content-type render/get-doctype-class-from-dom)

(defmethod doctype->content-type :default [_] "text/html")
(defmethod doctype->content-type "xhtml" [_] "application/xhtml+xml")

(defn render-middleware
  "Middleware to convert hiccup to a string for use with ring."
  [h]
  (fn [req]
    (let [res (h req)
          r (cond (vector? (:body res))
                  (update res :body render/render-str)

                  (vector? res)
                  {:status 200 :headers {"Content-Type" (doctype->content-type res)} :body (render/render-str res)}
                  
                  :else
                  res)]
      r)))
