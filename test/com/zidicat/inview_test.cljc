(ns com.zidicat.inview-test
  (:require [#?@(:clj [clojure.test :refer] :cljs [cljs.test :refer-macros]) [deftest testing is are] :as test]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [com.zidicat.inview :as html #?@(:clj [:refer [def-view]] :cljs [:refer-macros [def-view]])]
            [com.zidicat.inview.render-as :as render]))

(defn template-conf []
  {:strip-whitespace     true
   :parser               :default
   :file                 "test/com/zidicat/inview-test.html"
   :template-search-path [".." "../libs/inview" "../../libs/inview" "." "../org-project/libs/inview"]
   :render               :default})

(defmethod html/get-settings "test/com/zidicat/inview-test.html" [s]
  (merge (template-conf) s))

(deftest loading
  #?(:clj
     (testing "reading html files"
       (testing "no selector"
         (is (= [:html {} "\n  "
                 [:head {} "\n  "] "\n  "
                 [:body {} "\n    "
                  [:div {:class "login"} "\n      "
                   [:form {:class "ajaxform", :method "POST", :action "/api/login"} "\n        "
                    [:fieldset {} "\n          "
                     [:label {:for "login-id"} "Login:"] "\n          "
                     [:input {:type "text", :name "login-id"}] "\n          "
                     [:label {:for "login-password"} "Password:"] "\n          "
                     [:input {:type "password", :name "login-password"}] "\n          "
                     [:input {:type "submit", :name "login", :value "Login"}] "\n        "] "\n      "] "\n    "] "\n    "
                  [:div {:class "logout"} "\n      "
                   [:span {} "Logged in as " [:span {:class "user"} "User"]] "\n      "
                   [:form {:class "ajaxform", :method "POST", :action "/api/logout"} "\n        "
                    [:input {:type "submit", :name "logout", :value "Logout"}] "\n      "] "\n    "] "\n  "] "\n"]
                (html/load-source (assoc (template-conf) :strip-whitespace false)))))
       (testing "with selector"
         (is (= [:div {:class "logout"} "\n      "
                 [:span {} "Logged in as " [:span {:class "user"} "User"]] "\n      "
                 [:form {:class "ajaxform", :method "POST", :action "/api/logout"} "\n        "
                  [:input {:type "submit", :name "logout", :value "Logout"}] "\n      "] "\n    "]
                (html/load-source (assoc (template-conf) :selector [:div.logout] :strip-whitespace false)))))
       (testing "defaults to stripping whitespace"
         (is (= [:div {:class "logout"}
                 [:span {} "Logged in as " [:span {:class "user"} "User"]]
                 [:form {:class "ajaxform", :method "POST", :action "/api/logout"}
                  [:input {:type "submit", :name "logout", :value "Logout"}]]]
                (html/load-source {:file                 "test/com/zidicat/inview-test.html"
                                   :selector             [:div.logout]
                                   :template-search-path (:template-search-path (template-conf))}))))
       (testing "trim whitespace explicitly"
         (is (= [:html {}
                 [:head {}]
                 [:body {}
                  [:div {:class "login"}
                   [:form {:class "ajaxform", :method "POST", :action "/api/login"}
                    [:fieldset {}
                     [:label {:for "login-id"} "Login:"]
                     [:input {:type "text", :name "login-id"}]
                     [:label {:for "login-password"} "Password:"]
                     [:input {:type "password", :name "login-password"}]
                     [:input {:type "submit", :name "login", :value "Login"}]]]]
                  [:div {:class "logout"}
                   [:span {} "Logged in as " [:span {:class "user"} "User"]]
                   [:form {:class "ajaxform", :method "POST", :action "/api/logout"}
                    [:input {:type "submit", :name "logout", :value "Logout"}]]]]]
                (html/load-source (template-conf))))))))

(deftest private-utils
  (testing "ensure-properties-exist"
    (is (= [:html {} [:div {:class "div"} [:span {} "text"]]]
           (html/ensure-attrs-exist [:html [:div {:class "div"} [:span "text"]]])))
    (is (= [:html {} [:div {:class "text div"} [:span {} "text"]]]
           (html/ensure-attrs-exist [:html [:div.text.div [:span "text"]]])))
    (is (= [[:DOCTYPE! {:html true}] [:html {} [:div {:class "div"} [:span {} "text"]]]]
           (html/ensure-attrs-exist [[:DOCTYPE! {:html true}] [:html [:div {:class "div"} [:span "text"]]]])))
    ;; TODO: do we need to support this?
    #_ (is (= [:html {} [:div {:class "div"} [:span {:id "id1"} "text"]]]
           (html/ensure-attrs-exist [:html [:div {:class "div"} [:span#id1 "text"]]])))))

(deftest rendering
  (testing "html rendering"
    (let [dom [:div {:class "logout"}
               [:span {} "Logged in as " [:span {:class "user"} "User"]]
               [:form {:class "ajaxform", :method "POST", :action "/api/logout"}
                [:input {:type "submit", :name "logout", :value "Logout"}]]]
          tem (html/template dom [:.user])]
      (is (= [:div          
              {:class "logout"}
              [:span {} "Logged in as " [:span {:class "user"} "FISH"]]
              [:form
               {:class "ajaxform", :method "POST", :action "/api/logout"}
               [:input {:type "submit", :name "logout", :value "Logout"}]]]
             (html/transform-template tem (html/content "FISH"))))
      (testing "string concat"
        (is (= "<div><span>test</span>test</div>"
               (html/render [:div {} [:span {} "test"] "test"]))))
      #?(:clj
         (testing "writing to a java writer"
           (is (= "<div><span class=\"user\">User<p>Hello fish</p></span></div>"
                  (str (render/tree-duce (map (fn [x]
                                                (if (and (sequential? x) (= :i18n (first x)))
                                                  ({:hello "Hello"} (second x) "content not found")
                                                  x)))
                                         (render/render-writer-rf (java.io.StringWriter.))
                                         (render/str-render-settings)
                                         [:div {} [:span {:class "user"} "User" [:p [:i18n :hello] " fish"]]]))))))
      (testing "transducer application before serialisation"
        (is (= "<div><span class=\"user\">User<p>Hello fish</p></span></div>"
               (render/tree-duce (map (fn [x]
                                        (if (and (sequential? x) (= :i18n (first x)))
                                          ({:hello "Hello"} (second x) "content not found")
                                          x)))
                                 (render/render-string-rf)
                                 (render/str-render-settings)
                                 [:div {} [:span {:class "user"} "User" [:p [:i18n :hello] " fish"]]]))))
      (testing "render tag with no content"
        (is (= "<div><span class=\"user\">test</span></div>"
               (html/render [:div {} [:span {:class "user"} "test"]]))))
      (testing "rendering a sequence of tags not just one tag"
        (is (= (str "<div class=\"nav\"><ol><li><a href=\"/home\">Home</a></li></ol></div>"
                    "<div class=\"nav\"><ol><li><a href=\"/link1\">" #?(:clj "1.0" :cljs "1") "</a></li></ol></div>"
                    "<div class=\"nav\"><ol><li><a href=\"/link2\">Link 2</a></li></ol></div>"
                    "<div class=\"nav\"><ol><li><a href=\"/link3\">:link-3</a></li></ol></div>")
               (html/transform
                [:div {:class "nav"} [:ol {} [:li {} [:a {} "Home"]]]]
                [:.nav] (html/clone-map #(comp (html/subform [:a] (html/content %1))
                                               (html/subform [:a] (html/set-attrs :href %2)))
                                        ["Home" 1.0M "Link 2" :link-3]
                                        ["/home" "/link1" "/link2" "/link3"])
                html/render))))

      (testing "escapes unsafe strings"
        (is (= "<p test=\"fish\">&lt;script&gt;alert('xss');&lt;/script&gt;</p>"
               (html/render [:p {(keyword "\"test\"") "'fish"} "<script>alert('xss');</script>"])))))))

(deftest selectors
  (testing "html selection"
    (testing "html 1"
      (let [dom [:div {:class "logout"}
                 [:span {:class "note"} "Logged in as " [:span {:class "user"} "User"]]
                 [:form {:id "logout-form", :method "POST", :action "/api/logout"}
                  [:input {:type "submit", :name "logout", :value "Logout"}]]
                 [:div {} [:span {:id "user" :class "cow fish dog"} [:p {} "Some" "text"]]]]]
        (are [s p]
            (= [{:selector s, :paths p}]
               (:selectors (html/template dom s)))
          [:form#logout-form]       [[3]]
          [:#logout-form]           [[3]]
          [:form]                   [[3]]
          [:span.user]              [[2 3]]
          [{:name "logout"}]        [[3 2]]
          [:span]                   [[2] [4 2]] ;; NOTE: does not search into matched els
          [[:span {:class "user"}]] [[2 3]]
          [[:span {:id "user"}]]    [[4 2]]
          [:span#user]              [[4 2]]
          [:span.cow]               [[4 2]]
          [:span.fish]              [[4 2]]
          [:span.dog]               [[4 2]]
          [{:class "cow fish dog"}] [[4 2]]
          [:div :span]              [[2] [4 2]]
          [:div :span :p]           [[4 2 2]])
        (testing "scrape"
          (are [s f p]
              (= p (html/select-from (html/template dom s) f))
            [:form#logout-form] :tag     [[:form]]
            [:span]             :attrs   [[{:class "note"} {:id "user", :class "cow fish dog"}]]
            [:.logout]          :content [[[[:span {:class "note"} "Logged in as " [:span {:class "user"} "User"]]
                                            [:form {:id "logout-form", :method "POST", :action "/api/logout"}
                                             [:input {:type "submit", :name "logout", :value "Logout"}]]
                                            [:div {}
                                             [:span {:id "user", :class "cow fish dog"}
                                              [:p {} "Some" "text"]]]]]]
            [:span.cow]         :all     [[[:span {:id "user", :class "cow fish dog"} [:p {} "Some" "text"]]]]))))
    (testing "html 2"
      (let [dom [:div
                 {:class "agenda"}
                 [:div {:id "thing"}
                  [:div {} [:p {} [:span {:class "percent-complete"}]]]
                  [:p {} "Some description"]
                  [:ol
                   {}
                   [:li
                    {:class "agenda-item"}
                    [:span {:class "title"} "Item"]
                    " ("
                    [:span {:class "state"} "TODO"]
                    ") "
                    [:form
                     {:class "state ajaxform inline-form", :method "POST"}
                     [:fieldset {} [:input {:type "submit", :name "event"}]]]
                    [:form
                     {:class "delete ajaxform inline-form", :method "POST"}
                     [:fieldset {} [:input {:type "submit", :name "event", :value "Delete"}]]]]]]
                 [:form
                  {:class "create ajaxform", :method "POST"}
                  [:fieldset {} [:input {:type "text", :name "title"}] [:input {:type "submit", :name "event"}]]]]]
        (are [s p]
            (= [{:selector s, :paths p}]
               (:selectors (html/template dom s)))
          [:.percent-complete] [[2 2 2 2]]
          [:.agenda-item]      [[2 4 2]]
          [:form.create]       [[3]])))))

(deftest transformations
  (testing "html transformations"
    (let [dom [:html {}
               [:ol {}
                [:li {:class "task"} "Things to do today"]]]
          tem (html/template dom [:li.task])]
      (testing "html/content"
        (testing "string"
          (is (= [:html {} [:ol {} [:li {:class "task"} "test"]]]
                 (html/transform-template tem (html/content "test")))))
        (testing "hiccup"
          (is (= [:html {} [:ol {} [:li {:class "task"} [:p {:class "test"} "test"]]]]
                 (html/transform-template tem (html/content [:p {:class "test"} "test"])))))
        (testing "number"
          (is (= [:html {} [:ol {} [:li {:class "task"} 12.34M]]]
                 (html/transform-template tem (html/content 12.34M))))))

      (testing "html/append"
        (is (= [:html {} [:ol {} [:li {:class "task"} "Things to do today" "test"]]]
               (html/transform-template tem (html/append "test")))))

      (testing "html/prepend"
        (is (= [:html {} [:ol {} [:li {:class "task"} "test" "Things to do today"]]]
               (html/transform-template tem (html/prepend "test")))))

      (testing "html/clone-map"
        (testing "many lists + html/content"
          (is (= [:html {} [:ol {}
                            [:li {:class "task modified light-background"} "test-0"]
                            [:li {:class "task modified dark-background"} "test-1"]
                            [:li {:class "task modified light-background"} "test-2"]
                            [:li {:class "task modified dark-background"} "test-3"]
                            [:li {:class "task modified light-background"} "test-4"]]]
                 (html/transform-template tem (html/clone-map #(comp (html/content (str %1 "-" %2))
                                                                     (html/add-class %3)
                                                                     (html/add-class "modified"))
                                                              (repeat "test") (range 5) (cycle ["light-background" "dark-background"]))))))

        (testing "html/content"
          (is (= [:html {} [:ol {}
                            [:li {:class "task"} "test-0"]
                            [:li {:class "task"} "test-1"]
                            [:li {:class "task"} "test-2"]]]
                 (html/transform-template tem (html/clone-map #(html/content (str "test-" %)) (range 3))))))

        (testing "html/clone-map + html/replace"
          (let [func (fn [x]
                       [:li {} [:span {} (str "test-" x)]])]
            (is (= [:html {} [:ol {}
                              [:li {} [:span {} "test-0"]]
                              [:li {} [:span {} "test-1"]]
                              [:li {} [:span {} "test-2"]]]]
                   (html/transform-template tem (html/clone-map #(html/replace (func %)) (range 3)))))))

        (testing "html/clone-map + html/replace splicing"
          (let [dom [:html {} [:div {} [:h1 {} "Test"] [:p {:class "para"} "Para"] [:div {} "Footer"]]]
                tem (html/template dom [:.para])
                func (fn [x] [:p {:class "para"} (str "test-" x)])]
            (is (= [:html {} [:div {}
                              [:h1 {} "Test"]
                              [:p {:class "para"} "test-0"]
                              [:p {:class "para"} "test-1"]
                              [:p {:class "para"} "test-2"]
                              [:div {} "Footer"]]]
                   #_ (html/transform-template tem (html/replace (func 0)))
                   (html/transform-template tem (html/clone-map #(html/replace (func %)) (range 3)))
                   )))))

      (testing "html/set-attrs"
        (is (= [:html {} [:ol {} [:li {:id "task-123" :class "task"} "Things to do today"]]]
               (html/transform-template tem (html/set-attrs :id "task-123")))))

      (testing "html/remove-attrs"
        (is (= [:html {} [:ol {} [:li {} "Things to do today"]]]
               (html/transform-template tem (html/remove-attrs :class)))))

      (testing "html/update-attr"
        (is (= [:html {} [:ol {} [:li {:class "fish"} "Things to do today"]]]
               (html/transform-template tem (html/update-attr :class (constantly "fish"))))))

      (testing "html/add-class"
        (is (= [:html {} [:ol {} [:li {:class "task fish"} "Things to do today"]]]
               (html/transform-template tem (html/add-class "fish")))))

      (testing "html/add-class but classes are vectors"
        (is (= [:html {} [:ol {} [:li {:class "task fish"} "Things to do today"]]]
               (html/transform-template (update-in tem [2 2 1 :class] vector) (html/add-class "fish")))))

      (testing "html/remove-class"
        (is (= [:html {} [:ol {} [:li {:class ""} "Things to do today"]]]
               (html/transform-template tem (html/remove-class "task")))))

      (testing "html/replace"
        (is (= [:html {} [:ol {} "fish"]]
               (html/transform-template tem (html/replace "fish")))))

      (testing "html/delete"
        (is (= [:html {} [:ol {} nil]]
               (html/transform-template tem (html/delete))))
        (testing "nil renders as empty string"
          (is (= (render/render-str [:html {} [:ol {}]])
                 (render/render-str (html/transform-template tem (html/delete)))))))
      
      (testing "html/wrap"
        (is (= [:html {} [:ol {} [:li {:class "task"} [:span {:class "test"} "Things to do today"]]]]
               (html/transform-template tem (html/wrap :span {:class "test"}))))
        (is (= [:html {} [:ol {} [:li {:class "task"} [:span {} "Things to do today"]]]]
               (html/transform-template tem (html/wrap :span)))))

      (testing "html/unwrap"
        (is (= [:html {} [:ol {} "Things to do today"]]
               (html/transform-template tem (html/unwrap)))))

      (testing "html/alter-if & html/has-class?"
        (is (= [:html {} [:ol {} [:li {:class "task"} "test"]]]
               (html/transform-template tem (html/alter-if (html/has-class? "task") (html/content "test")))))
        (is (= [:html {} [:ol {} [:li {:class "task"} "Things to do today"]]]
               (html/transform-template tem (html/alter-if (html/has-class? "fish") (html/content "test")))))
        (is (= [:html {} [:ol {} [:li {:id "id" :class "task"} "test"]]]
               (html/transform
                [:html {}
                 [:ol {}
                  [:li {:id "id" :class "task"} "Things to do today"]]]
                [:li.task] (html/alter-if (html/some-attr? {:id #{"id"}}) (html/content "test")))))))

    (testing "html/transform"
      (is (= [:html {}
              [:head {} "TITLE!"]
              [:body {}
               [:div {:class "header"} [:h1 {} "TITLE!"] [:div {:class "nav"} [:ol {} [:li {} [:a {:class "home"} "Home"]]]]]
               [:div {:class "main"} [:div {:class "content"}]]
               [:div {:class "footer"} [:p {} "© 2001 A Space Oddessy"]]]]
             (html/transform [:html {}
                              [:head {} [:title {} "Title"]]
                              [:body {}
                               [:div {:class "header"} [:h1 {} "Title"] [:div {:class "nav"} [:ol {} [:li {} [:a {:class "home"} "Home"]]]]]
                               [:div {:class "main"} [:div {:class "content"}]]
                               [:div {:class "footer"} [:p {} "© 2001 A Space Oddessy"]]]]
                             [:head] (html/content "TITLE!")
                             [:.header :h1] (html/content "TITLE!")))))))

(def-view logged-in-user [user]
  {:file   "test/com/zidicat/inview-test.html"
   :inline true}
  [:.user] (html/content (:name user))
  vec)

(def-view optional-let []
  {:strip-whitespace true
   :parser           :default
   :file             "test/com/zidicat/inview-test.html"
   :render           :default
   :inline           true}
  (let [user-name "Bob"]
    [:.user] (html/content user-name)
    vec))

(def-view chained-source [user]
  (logged-in-user user)
  [:.user] (html/append "!"))

(def-view string-content []
  {:content "<html><head></head><body><div class=\"login\"><form action=\"/api/login\" class=\"ajaxform\" method=\"POST\"><fieldset><label for=\"login-id\">Login:</label><input name=\"login-id\" type=\"text\" /><label for=\"login-password\">Password:</label><input name=\"login-password\" type=\"password\" /><input name=\"login\" type=\"submit\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Bob</span></span><form action=\"/api/logout\" class=\"ajaxform\" method=\"POST\"><input name=\"logout\" type=\"submit\" value=\"Logout\" /></form></div></body></html>"})

(deftest macro
  (testing "Macro"
    (testing "embeds parsed HTML"
      (let [expanded-macro (macroexpand
                            '(com.zidicat.inview/def-view logged-in-user [user]
                               {:strip-whitespace     true
                                :inline               true
                                :parser               :default
                                :file                 "test/com/zidicat/inview-test.html"
                                :template-search-path [".." "../libs/inview" "../../libs/inview" "." "../org-project/libs/inview"]
                                :render               :default}
                               [:.user] (com.zidicat.inview/content (:name user))
                               com.zidicat.inview/render))]
        (is (= :html (-> expanded-macro second second second second first)) (pr-str expanded-macro))
        (is (= #:com.zidicat.inview{:doctype ["html"]} (-> expanded-macro second second (nth 2))) (pr-str expanded-macro))))

    (testing "string content gets embedded"
      (let [expanded-macro (macroexpand-1 '(com.zidicat.inview/def-view string-content []
                                             {:content "<html><head></head><body><div class=\"login\"><form action=\"/api/login\" class=\"ajaxform\" method=\"POST\"><fieldset><label for=\"login-id\">Login:</label><input name=\"login-id\" type=\"text\" /><label for=\"login-password\">Password:</label><input name=\"login-password\" type=\"password\" /><input name=\"login\" type=\"submit\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Bob</span></span><form action=\"/api/logout\" class=\"ajaxform\" method=\"POST\"><input name=\"logout\" type=\"submit\" value=\"Logout\" /></form></div></body></html>"}))]
        (is (= true (-> {:content "<html></html>"} html/clean-settings html/get-settings :inline)))
        (is (= [:html {} "Something"] (-> [:html {} "Something"] html/clean-settings html/get-settings html/load-source)))
        (is (= :html (-> expanded-macro second second second second first)) (pr-str expanded-macro))))
    (let [expected-html "<html><head></head><body><div class=\"login\"><form action=\"/api/login\" class=\"ajaxform\" method=\"POST\"><fieldset><label for=\"login-id\">Login:</label><input name=\"login-id\" type=\"text\" /><label for=\"login-password\">Password:</label><input name=\"login-password\" type=\"password\" /><input name=\"login\" type=\"submit\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Bob</span></span><form action=\"/api/logout\" class=\"ajaxform\" method=\"POST\"><input name=\"logout\" type=\"submit\" value=\"Logout\" /></form></div></body></html>"]
      (testing "Runs and renders"
        (is (= expected-html (html/render (logged-in-user {:name "Bob"})))))
      (testing "Allows optional let"
        (is (= expected-html (html/render (optional-let)))))
      (testing "Allows chaining hiccup function calls"
        (let [expanded-macro (macroexpand-1 '(com.zidicat.inview/def-view chained-source [user]
                                               (logged-in-user user)
                                               [:.user] (html/append "!")))]
          (is (= ['logged-in-user 'user]
                 (-> expanded-macro
                     (nth 4)
                     second
                     second
                     second))))
        (is (nil? (meta (chained-source {:name "Bob"}))))
        (is (= (logged-in-user {:name ["Bob" "!"]}) (chained-source {:name "Bob"})))
        (is (= (string/replace expected-html #"Bob" "Bob!") (html/render (chained-source {:name "Bob"})))))

      (testing "a chained source"
        (is (= [:html {}
                [:head {}]
                [:body {}
                 [:div {:class "login"}
                  [:form {:class "ajaxform", :method "POST", :action "/api/login"}
                   [:fieldset {}
                    [:label {:for "login-id"} "Login:"]
                    [:input {:type "text", :name "login-id"}]
                    [:label {:for "login-password"} "Password:"]
                    [:input {:type "password", :name "login-password"}]
                    [:input {:type "submit", :name "login", :value "Login"}]]]]
                 [:div {:class "logout"}
                  [:span {} "Logged in as " [:span {:class "user"} "Bob" "!"]]
                  [:form {:class "ajaxform", :method "POST", :action "/api/logout"}
                   [:input {:type "submit", :name "logout", :value "Logout"}]]]]]
               (chained-source {:name "Bob"}))))
      
      (testing "String content"
        (is (nil? (meta (string-content))))
        (is (= (optional-let) (string-content)))
        (is (= expected-html (html/render (string-content))))))))

(def-view debug-view [user]
  {:file   "test/com/zidicat/inview-test.html"
   :inline true}
  [:.user] (html/content (:name user))
  [:.user] (html/append "!"))

(deftest debugging
  (testing "debugging"
    (let [data        (atom [])
          dbg         #(swap! data conj %)
          html        (binding [html/*debug-transform-fn* dbg] (debug-view {:name "Bob"}))
          [o & steps] @data]
      {:strip-whitespace true, :parser :default, :file "test/com/zidicat/inview-test.html", :template-search-path [".." "../libs/inview" "../../libs/inview" "." "../org-project/libs/inview"], :render :default, :inline true}
      {:strip-whitespace true, :template-search-path ["./" "../" "../page"], :inline true, :file "test/com/zidicat/inview-test.html"}
      (is (= "test/com/zidicat/inview-test.html" (:file (edn/read-string o))))
      (is (every? (comp vector? :after) steps))
      (is (vector? html)))))


(comment

  (require '[clojure.pprint :as ppr])
  (add-tap ppr/pprint)
  (binding [html/*debug-transform-fn* tap>]
    (html/transform
     [:div {:class "nav"} [:span {} [:a {} "Home"]]]
     [:.nav :a] (html/clone-map #(comp (html/content %1)
                                       (html/set-attrs :href %2)
                                       (html/add-class %3))
                                ["Home" "Link 1" "Link 2" "Link 3"]
                                ["/home" "/link1" "/link2" "/link3"]
                                (cycle ["light-background" "dark-background"]))))

  ;; {:before [:div {:class "nav"} [:span {} [:a {} "Home"]]],
  ;;  :selector [:.nav :a],
  ;;  :path [2 2],
  ;;  :matched-element [:a {} "Home"],
  ;;  :transformed-result
  ;;  [[:a {:class "light-background", :href "/home"} "Home"]
  ;;   [:a {:class "dark-background", :href "/link1"} "Link 1"]
  ;;   [:a {:class "light-background", :href "/link2"} "Link 2"]
  ;;   [:a {:class "dark-background", :href "/link3"} "Link 3"]],
  ;;  :replace-fn #function[com.zidicat.inview/clone-map/clone-map--16906],
  ;;  :after
  ;;  [:div
  ;;   {:class "nav"}
  ;;   [:span
  ;;    {}
  ;;    [:a {:class "light-background", :href "/home"} "Home"]
  ;;    [:a {:class "dark-background", :href "/link1"} "Link 1"]
  ;;    [:a {:class "light-background", :href "/link2"} "Link 2"]
  ;;    [:a {:class "dark-background", :href "/link3"} "Link 3"]]]}


  [:div {:class "nav"} [:span {} [:a {:class "light-background", :href "/home"} "Home"] [:a {:class "dark-background", :href "/link1"} "Link 1"] [:a {:class "light-background", :href "/link2"} "Link 2"] [:a {:class "dark-background", :href "/link3"} "Link 3"]]][:div {:class "nav"} [:span {} [:a {:class "light-background", :href "/home"} "Home"] [:a {:class "dark-background", :href "/link1"} "Link 1"] [:a {:class "light-background", :href "/link2"} "Link 2"] [:a {:class "dark-background", :href "/link3"} "Link 3"]]]

  (def-view example [user]
  {:strip-whitespace     true
   :parser               :default
   :file                 "test/com/zidicat/inview-test.html"
   :template-search-path [".." "../libs/inview" "../../libs/inview" "." "../org-project/libs/inview"]
   :render               :default}
  [:.user] (html/content (:name user))
  [:form]  (html/add-class "fish")
    [:form]  (html/remove-class "ajaxform"))

  (example {:name "Mr Bob Dabolina"})
  
  (require '[noahtheduke.fluent :as i18n])
  (let [sample-resource "
hello = Hello world!
welcome = Welcome, {$user}!
email-cnt = {$cnt ->
    [one] {$cnt} email
    *[other] {$cnt} emails
}"
        bundle (i18n/build "en" sample-resource)
        xform (map (fn [x]
                     (if (and (sequential? x) (= :i18n (first x)))
                       (apply i18n/format bundle (next x))
                       x)))]
    (->> [:div {} [:span {:class "greeting"} [:p [:i18n :hello]]]]
         (render/tree-duce xform (render/render-string-rf) (render/str-render-settings))))
  ;; =>
  "<div><span class=\"greeting\"><p>Hello world!</p></span></div>"
  
  

  )
