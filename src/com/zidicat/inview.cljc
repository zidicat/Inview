(ns com.zidicat.inview
  (:refer-clojure :exclude [replace])
  #?(:clj (:import [java.io File]))
  (:require [clojure.string :as string]
            #?(:clj [com.zidicat.inview.parse-html :as html])
            [com.zidicat.inview.render-as :as rndr]
            [com.zidicat.inview.selector :as selector]))

;; TODO add spec for hiccup
;; TODO perf testing (write to string buffer / output stream ?)

;; TODO remove @templates?
;; DONE FIX stripping whitespace?? - see com.zidicat.tbd.home (home/show-remotes @home/remotes)
;; DONE FIX move settings to an eval'd arg --- multimethods?
;; DONE multimethod / protocol for html parsing / rendering - document level - https://github.com/davidsantiago/hickory
;; DONE make it work in cljs
;; DONE do we really want keep searching children of matched els??
;; DONE paths to search for templates
;; WONT routing + loading data
;;       - add uuid to html for reloading??
;; DONE optional rendering
;; DONE load resources so it works from an uberjar?

(defn filter-whitespace
  "Recursively remove whitespace from strings in the dom contents. Whitespace only strings are removed, and strings
  containing whitespace have the whitespace reduced to a single space."
  [dom]
  (if (sequential? dom)
    (let [tx (comp (remove selector/is-whitespace?)
                   (map selector/trim-string)
                   (map filter-whitespace))]
      (with-meta (into [] tx dom) (meta dom)))
    dom))

(defmulti get-settings "Settings used by `load-source`. Allows for over-ridding settings on a per file basis." :file)
(defmethod get-settings :default [s]
  (merge {:strip-whitespace     true
          :template-search-path ["./"]
          :inline #?(:clj false :cljs true)}
         s))

(defn ensure-attrs-exist
  "Insert any missing empty attribute maps."
  [dom]
  (if (sequential? dom)
    (let [[tag & content] dom
          [t p c]         (when-not (sequential? tag)
                            (if (map? (first content))
                              [tag (first content) (next content)]
                              [tag {} content]))]
      (if (sequential? tag)
        (with-meta (mapv ensure-attrs-exist dom) (meta dom))
        (with-meta (into [t p] (map ensure-attrs-exist) c) (meta dom))))
    dom))

(defn render
  "Render dom as a string with string concatenation."
  ([dom]
   (rndr/render-str dom))
  #_ ([xform dom]
   (rndr/tree-duce xform (rndr/render-string-rf) dom))
  #_ ([xform rf dom]
   (rndr/tree-duce xform rf dom))
  #_ ([xform rf settings dom]
   (rndr/tree-duce xform rf settings dom)))

(defn template
  "Make a template from `dom` with supplied `sels`. Generally only useful from `def-view` macro."
  [dom & sels]
  (selector/make-template dom sels))

(defn transform-template
  "Apply transformations to template (created with `template`). `transformations` must match `sels` previously provdided
  to `template`."
  [template & transformations]
  (selector/render-template template transformations))

(defn transform
  "Transform `dom` with `sel-transform-pairs` in turn. Does not separate `template` and `transform-template` steps."
  [dom & [sel-or-fn tform & sel-transform-pairs]]
  (cond (nil? sel-or-fn) dom
        (fn? sel-or-fn) (sel-or-fn dom)
        (and sel-or-fn (fn? tform)) (recur (transform-template (template dom sel-or-fn) tform) sel-transform-pairs)
        :else dom))

(defn subform
  "Create transform that applies `transform` to `selector` on the matched snippet."
  [selector transform]
  (fn subform [dom]
    (transform-template (template dom selector) transform)))

(defn content
  "Creates transform Set content on matched element."
  [c]
  (selector/content (constantly c)))

(defn clone-map
  "Creates transfom that repeats matched element with supplied transformation."
  [f list & other-lists]
  (fn clone-map [el]
    (apply mapv #(let [func (try
                              (apply f %&)
                              (catch #?(:clj Throwable :cljs :default) e
                                (throw (ex-info "Error updating template element"
                                                {:replacement-fn f
                                                 :element el
                                                 :args %&
                                                 :subform :clone-map}
                                                e))))]
                   (if (vector? func)
                     func
                     (try
                       (func el)
                       (catch #?(:clj Throwable :cljs :default) e
                         (throw (ex-info "Error updating template element"
                                         {:replacement-fn func
                                          :element el
                                          :subform :clone-map}
                                         e))))))
           list other-lists)))

(defn append
  "Creates transfom that adds `els` onto the end of the matched element."
  [& els]
  (selector/content #(into (vec %) els)))

(defn prepend
  "Creates transfom that adds `els` onto the beginning of the matched element."
  [& els]
  (selector/content #(into (vec els) %)))

(defn wrap
  "Creates transfom that wraps the matched element in provided tag."
  ([tag]
   (selector/content #(into [tag {}] %)))
  ([tag attrs]
   (selector/content #(into [tag attrs] %))))

(defn unwrap
  "Creates transfom that unwraps the matched element, returning it's content."
  []
  (fn [el] (into [] (drop 2) el)))

(defn set-attrs
  "Creates transfom that sets attributes for the matched element."
  [& {:as attrs}]
  (selector/alter-attr merge attrs))

(defn remove-attrs
  "Creates transfom that removes attributes for the matched element."
  [& attrs]
  (selector/alter-attr dissoc attrs))

(defn update-attr
  "Creates transfom that applies `f` to the attribute `attr` for the matched element."
  [attr f]
  (fn [el]
    (assoc-in el [1 attr] (f el))))

(defn get-classes
  "Return the classes for an element."
  [[_ {:keys [class]}]]
  (selector/split-class-names class))

(defn add-class
  "Creates transfom that adds adds a class to matched element."
  [c]
  (update-attr :class #(string/join " " (distinct (conj (get-classes %) c)))))

(defn remove-class
  "Creates transfom that removes a class from the matched element."
  [c]
  (update-attr :class #(transduce (comp (remove (fn [x] (= c x))) (interpose " ")) str "" (get-classes %))))

(defn has-class?
  "Creates a predicate that returns truthy if the matched element has class `c`."
  [c]
  #(some #{c} (get-classes %)))

(defn some-attr?
  "Creates a predicate for use on matched element. Takes a map of attribute names to predicates. Matches if any of the
  provided predicates matches any of the matched element attributes."
  [map-of-attrs-to-preds]
  (fn [[_ attrs]] (some (fn [[k v]] (when-let [f (get map-of-attrs-to-preds k)] (f v))) attrs)))

(defn replace
  "Creates transfom that replaces the matched element with `c`."
  [c]
  (constantly (if (sequential? c) c [c])))

(defn delete
  "Creates transfom that removes the matched element."
  []
  (constantly nil))

(defn alter-if
  "Creates transfom that transfoms the matched element with `func` if the function `test` is truthy when provided the
  element."
  ([test func]
   (alter-if test func nil))
  ([test func else-fn]
   (let [test (if (fn? test) test (constantly test))]
     (fn [dom]
       (if (test dom)
         (let [d (func dom)]
           (if (vector? d)
             (with-meta d (meta dom))
             d))
         (if (fn? else-fn)
           (with-meta (else-fn dom) (meta dom))
           dom))))))

(defn load-source
  "Load and parse dom. `parser` defaults to `com.zidicat.inview.parse-html/parse`. `file` can be a string or a
  `java.util.File`. If `file` is a string, `template-search-path` (defaults to `[\"./\"]`) will be searched for a file
  to load. If `strip-whitespace` is anything other than `false` (including `nil`) `filter-whitespace` will be called on
  the parsed results. `selector` optionally specifies an element to select from the loaded dom. If more than one element
  is matched, only the first one is returned."
  #?@(:clj
      [[settings]
       (let [{:keys [file content selector strip-whitespace parser template-search-path]} settings

             parse-fn        (if (fn? parser) parser html/parse)
             assert-parsed   (fn [x]
                               (if (= x [])
                                 (throw (ex-info "Failed to find file or content" {:file file :content content :template-search-path template-search-path}))
                                 x))
             _               (when (and file content) (throw (ex-info "specify only one of `:file` and `:content`" {:file file :content content})))
             dom             (some-> (cond
                                       content               content
                                       (instance? File file) file
                                       (string? file)        (transduce (comp (keep #(some-> % (File.)))
                                                                              (keep #(some-> % (File. file)))
                                                                              (filter #(and (.exists %) (.isFile %)))
                                                                              (halt-when identity (fn [_ y] y)))
                                                                        conj (or template-search-path ["./"])))
                                     assert-parsed
                                     parse-fn
                                     (cond-> (not= false strip-whitespace) filter-whitespace)
                                     ensure-attrs-exist)
             {[path] :paths} (when selector
                               (selector/make-path dom selector))
             dom             (if path
                               (get-in dom path)
                               dom)]
         (when (nil? dom) (throw (ex-info "No markup found" {:dom dom :source settings :name name})))
         dom)]
      :cljs
      [[dom]
       (:content dom)]))

(defn clean-settings
  "Internal function for use by `def-view` macro. Normalises various input forms for the `source` to a map."
  [source]
  (cond (map? source)
        (cond-> source
          (or (list? (:parser source))
              (symbol? (:parser source))) (update :parser eval)
          (string? (:content source))     (assoc :inline true))

        (string? source)
        {:file source}

        (and (vector? source)
             (string? (first source)))
        (cond-> {:file (first source)}
          (vector? (second source)) (assoc :selector (second source))
          (keyword? (second source)) (assoc :selector [(second source)]))

        (and (vector? source)
             (keyword? (first source)))
        {:content source :parser identity}

        (list? source)
        {:form source :parser identity}

        :else
        {}))

(defn can-inline?
  "Internal function for use by `def-view` macro. Compares the given `args` to the `source` and decides if the generated
  template requires runtime evaluation."
  [args source]
  #?(:clj
     (let [arg-symbols (->> args
                            (mapcat #(vector % nil))
                            destructure
                            (partition-all 2)
                            (map first)
                            set)
           needs-eval? (list? source)]
       (not
        (and needs-eval?
             (->> source
                  (tree-seq (some-fn sequential? map?) (fn [x] (if (map? x) (mapcat seq x) (seq x))))
                  (some arg-symbols)))))))

(defmacro def-view
  "Macro that defines a function that loads the template and applies the provided transforms."
  [name args source & body]
  (when (and (map? source) (symbol? (:parser source)) (get &env (:parser source)))
    (throw (ex-info "Can't use a local as a parser. Define a Var instead." {:def-view name :parser (:parser source)})))
  (let [settings (-> source clean-settings get-settings)

        [local-lets transforms] (if (and (list? (first body))
                                         (= 1 (count body))
                                         (or (= (ffirst body) `let)
                                             (= (ffirst body) 'let))
                                         (vector? (second (first body))))
                                  [(second (first body)) (drop 2 (first body))]
                                  [nil body])

        pairs  (partition 2 transforms)
        ret-fn (when (not= (last transforms) (second (last pairs)))
                 (last transforms))
        sels   (mapv first pairs)
        tfns   (mapv second pairs)
        docstr (str "Template definied by `com.zidicat.inview/def-view` from " source ". Takes " args ".")]
    (if (and (:inline settings) (can-inline? args source))
      (let [dom (load-source settings)]
        `(let [template# (with-meta (template ~dom ~@sels) ~(meta dom))]
           (defn ~name
             ~(str docstr " Template has been inlined.")
             ~args
             (let [~@local-lets
                   transform# ~(if ret-fn `(comp ~ret-fn transform-template) `transform-template)]
               (transform# template# ~@tfns)))))
      `(defn ~name
         ~docstr
         ~args
         (let [dom# (-> ~source clean-settings get-settings)
               dom# (if-let [[f# & args#] (:form dom#)]
                      (apply f# args#)
                      (load-source dom#))
               template# (with-meta (template dom# ~@sels) (meta dom#))
               ~@local-lets
               transform# ~(if ret-fn `(comp ~ret-fn transform-template) `transform-template)]
           (transform# template# ~@tfns))))))

(comment


  (def template-conf {:strip-whitespace     true
                      :parser               :default
                      :file                 "test/com/zidicat/inview-test.html"
                      :template-search-path [".." "../libs/inview" "."]
                      :render               :default
                      :inline               true})

  (def-view default-logged-in-user [user]
    template-conf
    [:.user] (content (:name user))
    [:form]  (comp (add-class "fish")
                   (remove-class "ajaxform")))

  (default-logged-in-user {:name "Mr Bob Dabolina"})
  
  )
