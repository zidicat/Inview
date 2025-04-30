(ns com.zidicat.inview.render-p
  (:require [clojure.string :as string])
  #?(:clj
     (:import [clojure.lang Keyword Sequential]
              [java.util Map]
              [java.io StringWriter]
              [java.lang String])))

(defn- strip-quotes [s]
  (string/replace s #"['\"]" ""))

(defn- meta->doctype
  "Render the DOCTYPE element from the dom metadata if present."
  [dom]
  (let [doctype (-> dom meta :com.zidicat.inview/doctype)]
    (when (and doctype (sequential? doctype) (every? string? doctype))
      (apply str "<!DOCTYPE " (concat (interpose " " doctype) [">"])))))

(defn- close-the-tag [tag content? requires-separate-close-tag?]
  (if (and (not content?) (keyword? tag)
           (not (requires-separate-close-tag? tag)))
    " />"
    (apply str (when (not content?) ">") (when (keyword? tag) ["</" (name tag) ">"]))))

(defprotocol TreeDucer
  (render-doctype [this dom])
  (get-rf [this])
  (write-tag [this tag])
  (write-close [this tag content?])
  (start-tag [this tag])                 ;TODO rename start-tag / finish-tag??
  (finish-tag [this tag-obj tag])
  (attr-rf [this tag-obj tag])
  (attr-xform [this ])
  (write-str [this s])
  (empty-content [this]))

(defprotocol TreeDuceStep
  (tree-step [obj treeDucer xform tag-state result]))

(defn- convert-tag [treeDucer xform] ;; TODO moving convert-tag into a var
  (let [t (volatile! nil)]
    (fn [rf]
      (fn tag-converter-rf
        ([] (rf))
        ([r]
         (let [t @t
               c (when t (write-close treeDucer (:tag t) (:content? t)))]
           (-> r (cond-> c (rf c)) rf)))
        ([r x]
         (tree-step x treeDucer xform t r))))))

(deftype NoopTreeDucer []
  TreeDucer
  (render-doctype [this dom] (fn [result] (with-meta result (meta dom))))
  (get-rf [this] conj)
  (write-tag [this tag] tag)
  (write-close [this tag content?] nil)
  (start-tag [this tag] nil)
  (finish-tag [this tag-obj tag] nil)
  (attr-rf [this tag-obj tag] (fn ([] {}) ([r] r) ([r x] (conj r x))))
  (attr-xform [this] (map identity))
  (write-str [this s] s)
  (empty-content [this] []))

(deftype StringTreeDucer [requires-separate-close-tag?]
  TreeDucer
  (render-doctype [this dom] (meta->doctype dom))
  (get-rf [this] str)
  (write-tag [this tag] (str (namespace tag) (when (namespace tag) ":") (name tag)))
  (write-close [this tag content?] (close-the-tag tag content? requires-separate-close-tag?))
  (start-tag [this tag] "<")
  (finish-tag [this tag-obj tag] ">")
  (attr-rf [this tag-obj tag] (fn ([] "") ([r] r) ([r x] (str r x))))
  (attr-xform [this] (mapcat #(vector " " (strip-quotes (name (key %))) "=\"" (strip-quotes (val %)) "\"")))
  (write-str [this s] s)
  (empty-content [this] nil))

(deftype WriterTreeDucer [writer rf requires-separate-close-tag?]
  TreeDucer
  (render-doctype [this dom] (meta->doctype dom))
  (get-rf [this] rf)
  (write-tag [this tag] (str (namespace tag) (when (namespace tag) ":") (name tag)))
  (write-close [this tag content?] (close-the-tag tag content? requires-separate-close-tag?))
  (start-tag [this tag] "<")
  (finish-tag [this tag-obj tag] ">")
  (attr-rf [this tag-obj tag] (fn ([] "") ([r] r) ([r x] (str r x))))
  (attr-xform [this] (mapcat #(vector " " (strip-quotes (name (key %))) "=\"" (strip-quotes (val %)) "\"")))
  (write-str [this s] s)
  (empty-content [this] nil))

#?(:cljs
   (deftype JsDomTreeDucer [rf nss]
     TreeDucer
     (render-doctype [this dom] nil)
     (get-rf [this] rf)
     (write-tag [this tag]
       (if-let [nom (namespace tag)]
         (js/document.createElementNS nom (name tag))
         (js/document.createElement (name tag))))
     (write-close [this tag content?] nil)
     (start-tag [this tag] nil)
     (finish-tag [this tag-obj tag] nil)
     (attr-rf [this tag-obj tag]
       (fn
         ([] tag-obj)
         ([t] t)
         ([el [k v]]     ;TODO read study learn and inwardly digest ... and correct??
          (cond
            (qualified-keyword? k)
            (.setAttributeNS el (get nss (namespace k)) (name k) v)

            (fn? v) ;; Set event handlers directly, rather than through setAttribute??? #_ (unchecked-set el (name k) v)
            (.setAttribute el (name k) (str "const __func__ = " v "; __func__()"))

            (and (= :class k) (sequential? v))
            (.setAttribute el "class" (string/join " " v))

            (and (= :style k) (map? v))
            (doseq [[prop val] v]
              (.setProperty (.-style el) (name prop) val))

            :else
            (.setAttribute el (name k) v))
          el)))
     (attr-xform [this] nil)
     (write-str [this s] (js/document.createTextNode s))
     (empty-content [this]
       ::empty-content)))

#?(:cljs
   (defn js-dom-tree-ducer []
     (let [js-dom-rf (fn to-js-dom-rf
                       ([] nil)
                       ([r]
                        r)
                       ([r x]
                        (if (not= r ::empty-content)
                          (if (and r (not (== r x)))
                            (doto r (.appendChild x))
                            x)
                          nil)))]
       (->JsDomTreeDucer js-dom-rf
                         {"svg"   "http://www.w3.org/2000/svg"
                          "xhtml" "http://www.w3.org/1999/xhtml"
                          "xlink" "http://www.w3.org/1999/xlink"
                          "rdf"   "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                          "cc"    "http://creativecommons.org/ns#"
                          "dc"    "http://purl.org/dc/elements/1.1/"}))))

(defn string-tree-ducer []
  (->StringTreeDucer (complement #{:area :base :br :col :embed :hr :img :input :link :meta :source :track :wbr :svg})))

(defn writer-tree-ducer [writer]
  (let [rf (fn writer-rf
             ([] writer)
             ([r] (.toString r))
             ([r ^String x] (.append r x)))]
    (->WriterTreeDucer writer rf (complement #{:area :base :br :col :embed :hr :img :input :link :meta :source :track :wbr :svg}))))

(defn- close-tag* [treeDucer t r rf]
  (let [{:keys [tag tag-obj content?]} @t]
    (if-let [c (when (and tag (not content?))
                 (finish-tag treeDucer tag-obj tag))]
      (rf r c)
      r)))

(defn- associative-tree-step [m treeDucer t result]
  (let [{:keys [tag attrs tag-obj]} @t]
    (vswap! t (fn [x] (when (nil? (:attrs x)) (assoc x :attrs true))))
    (if (nil? attrs)
      (let [rf (get-rf treeDucer)
            a  (transduce (or (attr-xform treeDucer) (map identity)) (attr-rf treeDucer tag-obj tag) (sort-by key m))]
        (rf result a))
      result)))

(extend-protocol TreeDuceStep
  Keyword
  (tree-step [k treeDucer xform t result]
    (let [rf                           (get-rf treeDucer)
          {:keys [opened-tag tag-obj]} (vswap! t (fn [x]
                                                   (if (nil? (:tag x))
                                                     (assoc x
                                                            :tag k
                                                            :opened-tag (start-tag treeDucer k)
                                                            :tag-obj (write-tag treeDucer k)
                                                            :content? nil)
                                                     (dissoc x :opened-tag :tag-obj))))]
      (-> result
          (cond-> opened-tag (rf opened-tag))
          (cond-> tag-obj (rf tag-obj)))))

  #?(:clj Map :cljs cljs.core/PersistentArrayMap)
  (tree-step [m treeDucer xform t result]
    (associative-tree-step m treeDucer t result))

  #?(:cljs cljs.core/PersistentHashMap)
  #?(:cljs (tree-step [m treeDucer xform t result]
                      (associative-tree-step m treeDucer t result)))

  #?(:clj String :cljs string)
  (tree-step [s treeDucer xform t r]
    (let [rf (get-rf treeDucer)
          c  (close-tag* treeDucer t r rf)]
      (vswap! t assoc :content? true)
      (rf c (write-str treeDucer s))))

  #?(:clj Sequential :cljs cljs.core/PersistentVector)
  (tree-step [s treeDucer xform t result]
    (let [rf (get-rf treeDucer)
          r  (close-tag* treeDucer t result rf)]
      (vswap! t assoc :content? true)
      (if-let [init (empty-content treeDucer)]
        (->> s
             (transduce (comp xform (convert-tag treeDucer xform))
                        (fn             ;TODO make-persistent?? - confused rfs - probably caused by putting rf into treeducer
                          ([] init)
                          ([c] (rf r c))
                          ([c x] (conj c x)))))
        (transduce (comp xform (convert-tag treeDucer xform)) (completing rf) r s))))

  nil
  (tree-step [s treeDucer xform t r]
    (let [rf (get-rf treeDucer)
          c  (close-tag* treeDucer t r rf)]
      (vswap! t assoc :content? true)
      (rf c (pr-str s)))))

(defn tree-duce
  "Traverse the DOM tree, using provided transducer and reducing function. Optionally takes a map of settings (see below).
  If this options map is not provdided, it will be resolved by calling the mulitmethod
  `com.zidicat.inview.render-as/doctype->render-options` with the `dom`."
  ([xform treeDucer dom]
   (let [rf  (get-rf treeDucer)
         out (rf)
         out (if-let [doctype (render-doctype treeDucer dom)]
               (if (fn? doctype)
                 (doctype out)
                 (rf out doctype))
               out)]
     (transduce (comp xform (convert-tag treeDucer xform)) rf out dom))))

(comment

  (let [dom [:html {}
             [:head {}]
             [:body {}
              [:div {:class "login"}
               [:form {:class "fish", :method "POST", :action "/api/login"}
                [:fieldset {}
                 [:label {:for "login-id"} "Login:"]
                 [:input {:type "text", :name "login-id"}]
                 [:label {:for "login-password"} "Password:"]
                 [:input {:type "password", :name "login-password"}]
                 [:input {:type "submit", :name "login", :value "Login"}]]]]
              [:div {:class "logout"}
               [:span {} "Logged in as " [:span {:class "user"} "Mr Bob Dabolina"]]
               [:form {:class "fish", :method "POST", :action "/api/logout"}
                [:input {:type "submit", :name "logout", :value "Logout"}]]]]]]
    (tree-duce (map identity)
               #_ (string-tree-ducer) (writer-tree-ducer (StringWriter.)) #_ (->NoopTreeDucer)
               dom))

  (import '[java.io StringWriter])

  )
