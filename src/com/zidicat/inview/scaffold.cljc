(ns com.zidicat.inview.scaffold
  (:require [com.zidicat.inview :as html #?@(:clj [:refer [def-view]] :cljs [:refer-macros [def-view]])]
            [com.zidicat.inview.selector :as selector]))

;;;; Forms

#_ (defmethod html/get-settings "inview/form-template" [s]
  (-> s
      (dissoc :file)
      (assoc :content [:form {:data-inview-template "form"}
                       [:fieldset {:data-inview-template "fields"}
                        [:div
                         {:data-inview-template "textfield"}
                         [:label {}]
                         [:input {:type "text"}]
                         [:span {:class "error", :data-inview-template "error"} "Error"]]
                        [:div {:data-inview-template "dropdown"}
                         [:label {}]
                         [:select {} [:option {}]]
                         [:span {:class "error", :data-inview-template "error"} "Error"]]]
                       [:fieldset {} [:input {:type "submit", :value "Cancel"}] [:input {:type "submit", :value "Save"}]]]
             :inline false)))

(defn insert-or-delete-error [error]
  (fn [dom]
    (cond (string? error)
          (html/transform dom [{:data-inview-template "error"}] (html/content error))

          (not error)
          (html/transform dom [{:data-inview-template "error"}] (html/delete))

          :else
          dom)))

(html/def-view textfield [nom value label error]
  ["inview/form-template" [{:data-inview-template "textfield"}]]
  [:input] (html/set-attrs :name nom :value value)
  [:label] (comp (html/set-attrs :for nom)
                 (html/content label))
  (insert-or-delete-error error))

(html/def-view password [nom value label error]
  ["inview/form-template" [{:data-inview-template "password"}]]
  [:input] (html/set-attrs :name nom :value value)
  [:label] (comp (html/set-attrs :for nom)
                 (html/content label))
  (insert-or-delete-error error))

(html/def-view dropdown [nom value label error options]
  ["inview/form-template" [{:data-inview-template "dropdown"}]]
  (let [f (fn [[k v]]
            (html/subform [:option] (comp (html/content v)
                                          (html/alter-if (constantly (= k value))
                                                         (html/set-attrs :selected "selected")))))]
    [:label] (comp (html/set-attrs :for nom)
                   (html/content label))
    [:select] (html/set-attrs :name nom)
    [:option] (html/clone-map f options)
    (insert-or-delete-error error)))

(html/def-view unchangeable-text [nom value label error]
  ["inview/form-template" [{:data-inview-template "unchangeable-text"}]]
  [:input] (html/set-attrs :name nom :value value)
  [:label] (comp (html/set-attrs :for nom)
                 (html/content label))
  (insert-or-delete-error error))

(html/def-view submit [nom value label]
  ["inview/form-template" [{:data-inview-template "submit"}]]
  [:input] (html/set-attrs :name nom :value value)
  [:label] (comp (html/set-attrs :for nom)
                 (html/content label)))

#_ (def render-field nil)
(defmulti render-field (fn [field _value _error] (first field)))

(defmethod render-field :default [_ _ _] nil)

(defmethod render-field :textfield [[_ nom label] value error]
  (textfield nom value label error))

(defmethod render-field :hidden [[_ nom] value _]
  [:input {:name nom :value value :type "hidden"}])

(defmethod render-field :password [[_ nom label] value error]
  (password nom value label error))

(defmethod render-field :dropdown [[_ nom label options] value error]
  (dropdown nom value label error options))

(defmethod render-field :unchangeable-text [[_ nom label] value error]
  (unchangeable-text nom value label error))

(defmethod render-field :submit [[_ nom label] _value _error]
  (submit nom label label))

(html/def-view form [ent errors-fn action fields]
  ["inview/form-template" [{:data-inview-template "form"}]]
  [{:data-inview-template "fields"}] (html/clone-map #(html/replace (render-field % (get ent (second %)) (when (ifn? errors-fn) (errors-fn (second %))))) fields)
  [:form] (html/set-attrs :action action :method "POST"))

#_ (def update-field-value* nil)
(defmulti update-field-value* (fn [dom x]
                                (if (sequential? dom)
                                  (recur (first dom) x)
                                  dom)))

(defmethod update-field-value* :default [dom _x]
  dom)

(defmethod update-field-value* :input [dom [_nom value]]
  (update dom 1 assoc :value value))

(defmethod update-field-value* :select [dom [_nom value]]
  (let [tf (html/subform [[:option {:value value}]] #(update % 1 assoc :selected "selected"))]
    (tf dom)))

(defmethod update-field-value* :label [[tag attr] [_nom value]]
  (into [tag (or attr {})] (if (sequential? value) value [value])))

(defmethod update-field-value* :button [dom [_nom value]]
  (update dom 1 assoc :value value))

(defn update-field-values [ent]
  (fn [dom]
    (reduce (fn [d [k v]]
              (let [f (html/subform [{:name (pr-str k)}] #(update-field-value* % [k v]))]
                (f d)))
            dom ent)))

(defn debug-form-fields! [f]
  (fn [dom]
    (doto dom
      (->> (tree-seq sequential? seq)
           (keep :name)
           (run! f)))))


(comment

  (let [dom [:form {:data-inview-template "form"}
             [:fieldset {:data-inview-template "fields"}
              [:div {:data-inview-template "textfield"}
               [:label {:for "test"} "label"]
               [:input {:type "text", :name ":test", :value "thing"}]
               [:span {:class "error", :data-inview-template "error"} "error"]]
              [:div {:data-inview-template "dropdown"}
               [:label {:for "test"} "label"]
               [:select {:name ":user/opt"}
                [:option {:value "fish"} "Fish!"]
                [:option {:value "dog"} "Dog!"]]
               [:span {:class "error", :data-inview-template "error"} "error"]]]
             [:fieldset {} [:input {:type "submit", :value "thing", :name "test"}] [:input {:type "submit", :value "thing", :name "test"}]]]]
    (->> dom
         (tree-seq sequential? seq)
         (keep :name))
    #_ ((html/subform [[:select {:name ":user/opt"}]] prn) dom)
    #_ ((update-field-values {:test "TESTING!!!"
                           :user/opt "fish"}) dom))



  )

;;;; Utils

(defn replace-content-by-class
  ([keywords]
   (replace-content-by-class keywords {}))
  ([keywords content-fns]
   (fn [m]
     (reduce (fn [f k]
               (let [v (if-let [cfn (get content-fns k)]
                         (cfn (get m k))
                         (str (get m k)))]
                 (comp f (html/subform [(keyword (str "." (name k)))] (html/content v))))) identity keywords))))

(defn replace-classes-matching-map-keys [m val-fns]
  (reduce (fn [f k]
            (let [vfn (val-fns k)
                  vfn (if (ifn? vfn) vfn str)]
              (comp f (html/subform [(keyword (str "." (name k)))] (html/content (vfn (get m k)))))))
          identity
          (keys m)))


