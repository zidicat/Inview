(ns com.zidicat.inview.selector
  (:require [clojure.set :refer [rename-keys]]
            [clojure.string :as string]))

(defn split-class-names [cs]
  (into []
        (-> (distinct)
            (cond-> (sequential? cs) (comp (map name)))
            (comp (filter seq)))
        (if (string? cs)
          (string/split cs #"\s")
          cs)))

(defn- selector-element->attr [k]
  (cond (keyword? k)
        (let [[_ tag id-or-class v] (re-find #"([^.#]*)([.#])?(.*)" (name k))
              t                     (some-> tag not-empty keyword)]
          (case id-or-class
            "." [t {:class #(some #{v} (split-class-names %))}]
            "#" [t {:id v}]
            [t {}]))

        (map? k)
        [nil k]

        (vector? k)
        k))

(defn- make-selector [k]
  (let [[tag attr] (selector-element->attr k)]
    (when (or tag attr)
      (fn sel [[t a]]
        (and (map? a)
             (keyword? t)
             (or (empty? attr)
                 (and (every? string? (vals attr))
                      (= attr (select-keys a (keys attr))))
                 (and (every? fn? (vals attr))
                      (reduce (fn [_ [k f]] (when (f (k a)) (reduced true))) nil attr)))
             (or (nil? tag) (= t tag)))))))

(defn- select-indexed [f]
  (fn [i d]
    (let [c (drop 2 d)]
      (if (f d)
        {:index i :children c}
        {:index i :not-matched c}))))

(defn- update-path [path]
  (fn [e]
    (cond-> e
      (nil? (:path e)) (assoc :path (conj path (+ 2 (:index e))))
      true             (dissoc :index))))

(defn not-html-element? [t]
  (-> t :children first char?))

(defn- apply-selectors [v f]            ;TODO cleanup
  (->> v
       (mapcat
        (fn [{:keys [children path]}]
          (let [indexed        (map-indexed (select-indexed f) children)
                update-path-tx (comp (filter :children)
                                     (map (update-path path)))
                child-matches  (sequence (comp
                                          (filter :not-matched)
                                          (map #(rename-keys % {:not-matched :children}))
                                          update-path-tx
                                          (remove not-html-element?))
                                         indexed)]
            (-> (sequence update-path-tx indexed)
                (concat (apply-selectors child-matches f))))))))

(defn make-path [dom sel]
  (some->> sel
           (keep make-selector)
           (reduce apply-selectors [{:children [dom] :path []}])
           (mapv #(-> % :path (subvec 1)))
           (array-map :selector sel :paths)))

(defn make-template [dom sels]
  {:dom       dom
   :selectors (mapv (partial make-path dom) sels)})

(defn is-whitespace? [e]
  (and (string? e) (boolean (re-matches #"\s*" e))))

(defn trim-string [s]
  (if (string? s)
    (string/replace s #"[ \r\n\t][ \r\n\t]*" " ")
    s))

(defn- should-splice? [v]
  (and (sequential? v) (not (keyword? (first v)))))

(defn content [f & args]
  (fn [[t a & c]]
    (let [x (apply f c args)]
      (into [t a] (if (should-splice? x) x [x])))))

(defn alter-attr [f attrs]
  (fn [el]
    (apply update el 1 f attrs)))

(defn- update-el [r s]
  (if (vector? r)
    (fn update-el [el path]
      (try
        (if (empty? path)
          r
          (if (should-splice? r)
            (if-let [p (not-empty (pop path))]
              (update-in el p (fn [[t a]] (into [t a] r)))
              (into [(first el) (or (second el) {})] r))
            (assoc-in el path r)))
        (catch #?(:clj Throwable :cljs :default) e
          (throw (ex-info "Error updating template element"
                          {:replacement r
                           :snippet el
                           :path path
                           :selector s}
                          e)))))
    (fn update-el [el path]
      (let [v (volatile! nil)]
        (try
          (if (empty? path)
            (r el)
            (do
              (vreset! v (get-in el path))
              (vswap! v r)
              (if (should-splice? @v)
                (if-let [p (not-empty (pop path))]
                  (update-in el p (fn [[t a]] (into [t a] @v)))
                  (into [(first el) (or (second el) {})] @v))
                (assoc-in el path @v))))
          (catch #?(:clj Throwable :cljs :default) e
            (throw (ex-info "Error updating template element"
                            {:replacement-fn r
                             :element @v
                             :snippet el
                             :path path
                             :selector s}
                            e))))))))

(defn- replace-template-element [dom [{:keys [paths selector]} r]]
  (reduce (update-el r selector) dom paths))

(defn render-template [{:keys [dom selectors]} replacement-fns]
  (with-meta
    (->> (map vector selectors replacement-fns)
         (reduce replace-template-element dom))
    (meta dom)))
