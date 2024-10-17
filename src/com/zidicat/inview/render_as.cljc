(ns com.zidicat.inview.render-as
  (:require [clojure.string :as string])
  #?(:clj (:import [java.io File]
                   [clojure.lang Named Associative Sequential])))

;; TODO make it so we can render to react components, a-la : https://github.com/tonsky/rum/blob/1e1fb15c65b80c4962288fbbbab0610e0daa3250/src/rum/server_render.clj#L464

(defn render-doctype
  "Render the DOCTYPE element from the dom metadata if present."
  [dom]
  (let [doctype (-> dom meta :com.zidicat.inview/doctype)]
    (when (and doctype (sequential? doctype) (every? string? doctype))
      (apply str "<!DOCTYPE " (concat (interpose " " doctype) [">"])))))

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

(defn noop-render-settings []
  {:tag-fn        identity
   :str-fn        identity
   :attr-xform    (map identity)
   :empty-attr    {}
   :empty-content []})

(defmethod doctype->render-options :default [_]
  (noop-render-settings))

#?(:cljs
   (defn js-dom-rf []
     (fn to-js-dom-rf
       ([] nil)
       ([r] r)
       ([r x]
        (if r
          (doto r (.appendChild x))
          x)))))

#?(:cljs
   (defn js-dom-render-settings []
     {:tag-fn     (fn [x]
                    (if-let [nom (namespace x)]
                      (js/document.createElementNS nom (name x))
                      (js/document.createElement (name x))))
      :str-fn     (fn [x]
                    (js/document.createTextNode x))
      :close-tag  (constantly nil)
      :open-tag   (constantly nil)
      :empty-attr (fn [tag _acc _rf]
                    (let [nss {"svg"   "http://www.w3.org/2000/svg"
                               "xhtml" "http://www.w3.org/1999/xhtml"
                               "xlink" "http://www.w3.org/1999/xlink"
                               "rdf"   "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                               "cc"    "http://creativecommons.org/ns#"
                               "dc"    "http://purl.org/dc/elements/1.1/"}]
                      (fn
                        ([] tag)
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
                         el))))}))

#?(:cljs
   (defmethod doctype->render-options :js [_]
     (js-dom-render-settings)))

(defn- strip-quotes [s] (some-> s (string/replace #"['\"]" "")))
(defn- esc-entities [s] (some-> s (string/replace #"<" "&lt;") (string/replace #">" "&gt;")))

(defn tree-duce
  "Traverse the DOM tree, using provided transducer and reducing function. Optionally takes a map of settings (see below).
  If this options map is not provdided, it will be resolved by calling the mulitmethod
  `com.zidicat.inview.render-as/doctype->render-options` with the `dom`.

  :tag-fn        - function to convert the tag keyword to the desired target. Defaults to `identity`.

  :add-close     - function to add close tag. Takes a map of `:tag` (keyword from the hiccup source) and `:content?`
                   (`true` if content has been added). Optional.

  :open-tag      - function called to initialise a tag. Takes the tag keyword from the hiccup source and returns an
                   element to be added before the result of `tag-fn`. Optional.

  :close-tag     - similar to `open-tag` but called after the result of `tag-fn`. Will not be called if no content
                   has been added (see `add-close`). Optional.

  :empty-attr    - either a collection (that must support `transient`) to initialise the empty attributes of a tag
                   or a constructor of a reducing function that will be passed 3 args
                   `tag` the result of `tag-fn` if truthy, otherwise the keyword from the hiccup source
                   `acc` the containing accumulator
                   `rf`  the containing reducing function

  :attr-xform    - transducer used to process the attributes before adding them to the `empty-attr`. Defaults to
                   `(map identity)`

  :str-fn        - function to convert string content. Defaults to `identity`.

  :empty-content - collection (that must support `transient`) to initialise the empty content of a tag. Optional.
  "
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
                              (let [{:keys [tag attrs content? tag-obj] :as t-} @t]
                                (cond
                                  (and (not tag) (keyword? x)) (vswap! t assoc :tag x)
                                  (and (not attrs) (map? x))   (vswap! t assoc :attrs true)
                                  (not content?)               (vswap! t assoc :content? true))
                                (cond
                                  (keyword? x)
                                  (let [o (when (and open-tag (nil? tag)) (open-tag x))
                                        to (tag-fn x)]
                                    (when to (vswap! t assoc :tag-obj to))
                                    (-> r
                                        (cond-> o (rf o))
                                        (cond-> to (rf to))))

                                  (and (not attrs) (map? x))
                                  (if empty-attr
                                    (if (fn? empty-attr)
                                      (transduce attr-xform (empty-attr (or tag-obj tag) r rf) (sort-by key x))
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


(comment

  (render-str [:h1 {} "test" :thing])


  (-> [:html {}
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
          [:input {:type "submit", :name "logout", :value "Logout"}]]]]]
      render-str)

  (= "<html><head></head><body><div class=\"login\"><form action=\"/api/login\" class=\"fish\" method=\"POST\"><fieldset><label for=\"login-id\">Login:</label><input name=\"login-id\" type=\"text\" /><label for=\"login-password\">Password:</label><input name=\"login-password\" type=\"password\" /><input name=\"login\" type=\"submit\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Mr Bob Dabolina</span></span><form action=\"/api/logout\" class=\"fish\" method=\"POST\"><input name=\"logout\" type=\"submit\" value=\"Logout\" /></form></div></body></html>"  "<html><head></head><body><div class=\"login\"><form action=\"/api/login\" class=\"fish\" method=\"POST\"><fieldset><label for=\"login-id\">Login:</label><input name=\"login-id\" type=\"text\" /><label for=\"login-password\">Password:</label><input name=\"login-password\" type=\"password\" /><input name=\"login\" type=\"submit\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Mr Bob Dabolina</span></span><form action=\"/api/logout\" class=\"fish\" method=\"POST\"><input name=\"logout\" type=\"submit\" value=\"Logout\" /></form></div></body></html>")



  )
