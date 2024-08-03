(ns com.zidicat.inview.render-as
  (:require [clojure.string :as string])
  #?(:clj (:import [java.io File]
                   [clojure.lang Named Associative Sequential])))

(defn render-doctype
  "Render the DOCTYPE element from the dom metadata if present."
  [dom]
  (let [doctype (-> dom meta :com.zidicat.inview/doctype)]
    (when (and doctype (sequential? doctype)) (apply str "<!DOCTYPE " (concat (interpose " " doctype) [">"])))))

(defn render-string-rf
  "Reducing function for building a string."
  []
  #?(:clj (fn to-string
            ([] (java.lang.StringBuilder.))
            ([r] (.toString r))
            ([r ^String x] (.append r x)))
     :cljs (fn to-string
             ([] "")
             ([r] r)
             ([r x] (str r x)))))

#?(:clj
   (defn render-writer-rf
     "Reducing function for writing strings to a Writer."
     [writer]
     (fn to-string
       ([] writer)
       ([r] r)
       ([r ^String x] (.append r x)))))

(defn string-concat-rf
  []
  (fn to-string
    ([] "")
    ([r] r)
    ([r x] (str r x))))

(defn get-doctype-class-from-dom [dom]
  (let [dt (-> dom meta :com.zidicat.inview/doctype)]
    (if (sequential? dt)
      (first dt)
      dt)))

;; TODO perf test this vs case lookup vs .... ??
(defmulti doctype->render-options get-doctype-class-from-dom)

(def ^:private requires-separate-close-tag? (complement #{:area :base :br :col :embed :hr :img :input :link :meta :source :track :wbr :svg})) #_ #{:script :style :textarea :title :svg :template}

(defn str-render-settings []
  (let [strip-quotes (fn [s] (string/replace s #"['\"]" ""))]
    {:tag-fn     (fn [x] (str (namespace x) (when (namespace x) ":") (name x)))
     :str-fn     (fn [s] (-> s (string/replace #"<" "&lt;") (string/replace #">" "&gt;")))
     :attr-xform (mapcat #(vector " " (strip-quotes (name (key %))) "=\"" (strip-quotes (val %)) "\""))
     :add-close  (fn [{:keys [tag content?]}]
                   (if (and (not content?) (keyword? tag)
                            (not (requires-separate-close-tag? tag)))
                     " />"
                     (apply str (when (nil? content?) ">") (when (keyword? tag) ["</" (name tag) ">"]))))
     :close-tag  (constantly ">")
     :open-tag   (constantly "<")}))

(defmethod doctype->render-options "html" [_]
 (str-render-settings))

(defmethod doctype->render-options "xhtml" [_]
 (str-render-settings))

(defmethod doctype->render-options :default [_]
  {:tag-fn        identity
   :str-fn        identity
   :attr-xform    (map identity)
   :empty-attr    {}
   :empty-content []})

#?(:cljs
   (defmethod doctype->render-options :js [_]
     {:tag-fn     (fn [x]
                    (if-let [nom (namespace x)]
                      (js/document.createElementNS nom (name x))
                      (js/document.createElement (name x))))
      :str-fn     identity
      :empty-attr (fn [tag]
                    (let [nss {"svg" "http://www.w3.org/2000/svg"
                               "xhtml" "http://www.w3.org/1999/xhtml"
                               "xlink" "http://www.w3.org/1999/xlink"
                               "rdf" "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                               "cc" "http://creativecommons.org/ns#"
                               "dc" "http://purl.org/dc/elements/1.1/"}]
                      (fn
                        ([] tag)
                        ([t] t)
                        ([el [k v]]     ;TODO read study learn and inwardly digest ... and correct??
                         (cond
                           (qualified-keyword? k)
                           (.setAttributeNS el (get nss (namespace k)) (name k) v)

                           (= "on-" (subs (name k) 0 3))
                           ;; Set event handlers directly, rather than through setAttribute
                           (unchecked-set el (string/lower-case (name k)) v)

                           (and (= :class k) (sequential? v))
                           (.setAttribute el "class" (string/join " " v))

                           (and (= :style k) (map? v))
                           (doseq [[prop val] v]
                             (.setProperty (.-style el) (name prop) val))

                           :else
                           (.setAttribute el (name k) v))))))
      :attr-xform (mapcat (juxt key val))}))

(defn- strip-quotes [s] (some-> s (string/replace #"['\"]" "")))
(defn- esc-entities [s] (some-> s (string/replace #"<" "&lt;") (string/replace #">" "&gt;")))

(defn tree-duce
  ([xform rf dom]
   (tree-duce xform rf (doctype->render-options dom) dom))
  ([xform rf {:keys [tag-fn attr-xform empty-attr empty-content add-close open-tag close-tag str-fn]} dom]
   (let [close-tag*  (fn [a rf {:keys [tag content?]}]
                       (if-let [c (when (and close-tag tag (not content?))
                                    (close-tag tag))]
                         (rf a c)
                         a))
         group-rf    (fn [init r rf]
                       (if init
                         (fn ([] (transient init))
                           ([c] (rf r (persistent! c)))
                           ([c x] (conj! c x)))
                         rf))
         tag-fn      (or tag-fn identity)
         attr-xform  (or attr-xform (map identity))
         str-fn      (or str-fn identity)
         convert-tag (fn convert-tag []
                       (let [t (volatile! nil)]
                         (fn [rf]
                           (fn tag-converter-rf
                             ([] (rf))
                             ([r]
                              (let [t @t
                                    c (when (and t add-close) (add-close t))]
                                (-> r (cond-> c (rf c)) rf)))
                             ([r x]
                              (let [{:keys [tag attrs content?] :as t-} @t]
                                (cond
                                  (and (not tag) (keyword? x)) (vswap! t assoc :tag x)
                                  (and (not attrs) (map? x))   (vswap! t assoc :attrs true)
                                  (not content?)               (vswap! t assoc :content? true))
                                (cond
                                  (keyword? x)
                                  (let [o (when (and open-tag (nil? tag)) (open-tag x))
                                        t (tag-fn x)]
                                    (-> r
                                        (cond-> o (rf o)) 
                                        (cond-> t (rf t))))

                                  (and (not attrs) (map? x))
                                  (if empty-attr
                                    (if (fn? empty-attr)
                                      (transduce attr-xform (empty-attr tag r rf) (sort-by key x))
                                      (transduce attr-xform (group-rf empty-attr r rf) (sort-by key x)))
                                    (reduce (attr-xform rf) r (sort-by key x)))

                                  (string? x)
                                  (rf (close-tag* r rf t-) (str-fn x))

                                  (sequential? x)
                                  (let [r (close-tag* r rf t-)]
                                    (if empty-content
                                      (transduce (comp xform (convert-tag)) (group-rf empty-content r rf) x)
                                      (transduce (comp xform (convert-tag)) (completing rf) r x)))

                                  :else
                                  (rf (close-tag* r rf t-) (pr-str x)))))))))
         out (rf)
         out (if-let [doctype (render-doctype dom)]
               (rf out doctype)
               out)]
     (transduce (comp xform (convert-tag)) rf out dom))))

(defn- render-attrs [attrs]
  (some->> (not-empty attrs)
           (sort-by key)
           (map #(str (strip-quotes (name (key %))) "=\"" (strip-quotes (val %)) "\""))
           (interpose " ")
           (apply str " ")))

(defn- render-dom [dom]
  (if (sequential? dom)
    (let [[tag attrs & children] dom]
      (if-not (sequential? tag)
        (let [c (seq children)]
          (flatten [(str "<" (name tag) (render-attrs attrs) (if (or c (requires-separate-close-tag? tag)) ">" " />" ))
                    (map (comp render-dom #(cond-> % (string? %) esc-entities)) children)
                    (when (or c (requires-separate-close-tag? tag)) (str "</" (name tag) ">"))]))
        (mapcat render-dom dom)))
    dom))

(defn render-str
  "Render dom as a string with string concatenation."
  [dom]
  (try
    (let [dt (render-doctype dom)]
      (apply str dt (render-dom dom)))
    #?@(:clj
        [(catch clojure.lang.ExceptionInfo e
           (throw e))
         (catch Exception e
           (throw (ex-info "Error Rendering" {:dom dom} e)))]
        :cljs
        [(catch :default e
           (throw (ex-info "Error Rendering" {:dom dom} e)))])))
