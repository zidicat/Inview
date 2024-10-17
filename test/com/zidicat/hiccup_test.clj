(ns com.zidicat.hiccup-test 
  (:require [clojure.test :refer [deftest testing is are] :as test]
            [com.zidicat.inview :as html]
            [com.zidicat.inview.render-as :as render]
            [hiccup.compiler :as hic]
            [taipei-404.html :as parse]
            [hickory.core :as hickory]
            [clojure.string :as string]))

;; TOOD think about this lib : https://github.com/cjohansen/lookup

(def template-conf {:strip-whitespace     true
                    :parser               :default
                    :file                 "test/com/zidicat/inview-test.html"
                    :template-search-path [".." "../libs/inview" "."]
                    :render               :default})

(defn make-doctype-metadata [hic]
  (if (= :!DOCTYPE (ffirst hic))
    (with-meta (->> hic (drop 1) (drop-while string?) first)
      {:com.zidicat.inview/doctype (into [] (map name) (keys (second (first hic))))})
    hic))

(html/def-view default-logged-in-user [user]
    template-conf
  [:.user] (html/content (:name user))
  [:form]  (html/add-class "fish")
  [:form]  (html/remove-class "ajaxform")
  #_ (comp html/render vec))

(defn parser [f]
  (nth (hickory/as-hiccup (hickory/parse (slurp f))) 2))

(html/def-view alternative-logged-in-user [user]
  {:file             "test/com/zidicat/inview-test.html"
   :parser           parser
   :strip-whitespace true
   :inline           true}
  [:.user] (html/content (:name user))
  [:form]  (html/add-class "fish")
  [:form]  (html/remove-class "ajaxform")
  #_ hic/render-html)

(deftest rendering-and-parsing
  (testing "parsing and rendering"
    (testing "html->hiccup and hiccup match the default parser and render fn"
      (let [user    {:name "Mr Bob Dabolina"}
            alt     (alternative-logged-in-user user)
            default (default-logged-in-user user)]
        (is (= default alt))
        (is (= (parse/html->hiccup (hic/render-html default)) (parse/html->hiccup (hic/render-html alt))))))))

(deftest tree-duce
  (testing "tree-duce"
    (let [user    {:name "Mr Bob Dabolina"}
          alt     (alternative-logged-in-user user)
          default (default-logged-in-user user)]
      (testing "different parsers produce the same hiccup"
        (is (= default alt)))
      (testing "can render to string"
        (is (= (into [[:!DOCTYPE {:html true}]] (parse/html->hiccup (hic/render-html default)))
               (-> (render/tree-duce (map identity) (render/render-string-rf) (render/str-render-settings) default)
                   parse/html->hiccup))))
      #_ (testing "fake js api"
        (is (= :test #_ (into ["<!DOCTYPE html>"] default)
               (render/tree-duce (map identity)
                                 conj
                                 {:empty-content []
                                  :tag-fn (fn [x]
                                            (if-let [nom (namespace x)]
                                              (keyword (str "js/document.createElementNS/" nom) (name x))
                                              (keyword "js/document.createElement" (name x))))
                                  :empty-attr (fn [tag r rf]
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
                                  :attr-xform (map (fn [x] x))}
                                 default)))))))







(comment

  (default-logged-in-user {:name "Mr Bob Dabolina"})

  (pop [1 2 3])

  (html/def-view default-snippet [user]
    (merge template-conf
           {:file     "test/com/zidicat/inview-test.html"
            :selector [:.logout]
            :parser   :default})
    [:.user] (html/content (:name user))
    html/render)

  (html/def-view alternative-snippet [user]
    (merge template-conf
           {:file             "test/com/zidicat/inview-test.html"
            :selector         [:.logout]
            :parser           parser
            :strip-whitespace false})
    [:.user] (html/content (:name user))
    [:form]  (html/add-class "fish")
    [:form]  (html/remove-class "ajaxform")
    render-hic)



  (default-logged-in-user {:name "Mr Bob Dabolina"})

  (let [hic [:html {} [:p {} "test"]]]
    (hic/compile-html hic))

  (require '[clojure.data :as data])
  (let [dom [:html {} [:div {:class "login"} [:hr] [:form {:class "fish", :method "POST", :action "/api/login"} [:fieldset {} [:label {:for "login-id"} "Login:"] [:input {:type "text", :name "login-id"}] [:label {:for "login-password"} "Password:"] [:input {:type "password", :name "login-password"}] [:input {:type "submit", :name "login", :value "Login"}]]]] [:div {:class "logout"} [:span {} "Logged in as " [:span {:class "user"} "Mr Bob Dabolina"]] [:form {:class "fish", :method "POST", :action "/api/logout"} [:input {:type "submit", :name "logout", :value "Logout"}]]]]
        times 5000]
    (if (= (hic/render-html dom)
           #_ (render/render dom)
           (html/render dom)
           (render/tree-duce (map identity) (render/render-string-rf) (render/str-render-settings) dom)
           (render/render-str dom))
      [(prn '-- times '------------------------------------------------------------------------------)
       (pr 'hiccup '>>) (time (dotimes [x times] (hic/render-html dom)))
       ;; (pr 'render '>>) (time (dotimes [x times] (render/render dom)))
       (pr 'html/render '>>) (time (dotimes [x times] (html/render dom)))
       (pr 'render-str '>>) (time (dotimes [x times] (render/render-str dom)))
       (pr 'tree-duce '>>) (time (dotimes [x times] (render/tree-duce (map identity) (render/render-string-rf) (render/str-render-settings) dom)))]
      (map println [(hic/render-html dom) (render/render-str dom)])))

    (let [dom [:html {} [:div {:class "login"} [:form {:class "fish", :method "POST", :action "/api/login"} [:fieldset {} [:label {:for "login-id"} "Login:"] [:input {:type "text", :name "login-id"}] [:label {:for "login-password"} "Password:"] [:input {:type "password", :name "login-password"}] [:input {:type "submit", :name "login", :value "Login"}]]]] [:div {:class "logout"} [:span {} "Logged in as " [:span {:class "user"} "Mr Bob Dabolina"]] [:form {:class "fish", :method "POST", :action "/api/logout"} [:input {:type "submit", :name "logout", :value "Logout"}]]]]]
      [(hic/render-html dom) (render/render-str dom)])

    "<html><div class=\"login\"><form action=\"/api/login\" class=\"fish\" method=\"POST\"><fieldset><label for=\"login-id\">Login:</label><input name=\"login-id\" type=\"text\" /><label for=\"login-password\">Password:</label><input name=\"login-password\" type=\"password\" /><input name=\"login\" type=\"submit\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Mr Bob Dabolina</span></span><form action=\"/api/logout\" class=\"fish\" method=\"POST\"><input name=\"logout\" type=\"submit\" value=\"Logout\" /></form></div></html>"
    "<html><div class=\"login\"><form class=\"fish\" method=\"POST\" action=\"/api/login\"><fieldset><label for=\"login-id\">Login:</label><input type=\"text\" name=\"login-id\" /><label for=\"login-password\">Password:</label><input type=\"password\" name=\"login-password\" /><input type=\"submit\" name=\"login\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Mr Bob Dabolina</span></span><form class=\"fish\" method=\"POST\" action=\"/api/logout\"><input type=\"submit\" name=\"logout\" value=\"Logout\" /></form></div></html>"

  
  
  (default-snippet {:name "Mr Bob Dabolina"})
  (alternative-snippet {:name "Mr Bob Dabolina"})
  )
