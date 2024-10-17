(ns com.zidicat.hiccup-test
  (:require [cljs.test :refer-macros [deftest testing is are] :as test]
            [clojure.string :as string]
            [clojure.edn :as edn]
            [com.zidicat.inview :as html :refer-macros [def-view]]
            [com.zidicat.inview.render-as :as render]))

;; NOTE: you need to know that cljs starts it's broweser on http://localhost:9000/ for `M-x cider-connect-sibling-cljs`

(def-view example [user]
  {:strip-whitespace     true
   :parser               :default
   :file                 "test/com/zidicat/inview-test.html"
   :template-search-path [".." "../libs/inview" "../../libs/inview" "." "../org-project/libs/inview"]
   :inline               true ;; NOTE: will need to be true in cljs to load the template from disk
   :render               :default}
  [:.user] (html/content (:name user))
  [:form]  (html/add-class "fish")
  [:form]  (html/remove-class "ajaxform"))

(deftest macro
  (let [user   {:name "Mr Bob Dabolina"}
        hiccup [:html {}
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
    (testing "runs in cljs"
      (is (= hiccup (example user))))))

(deftest tree-duce
  (let [user   {:name "Mr Bob Dabolina"}]
    (testing "runs in cljs"
      (is (= "<!DOCTYPE html><html><head></head><body><div class=\"login\"><form action=\"/api/login\" class=\"fish\" method=\"POST\"><fieldset><label for=\"login-id\">Login:</label><input name=\"login-id\" type=\"text\" /><label for=\"login-password\">Password:</label><input name=\"login-password\" type=\"password\" /><input name=\"login\" type=\"submit\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Mr Bob Dabolina</span></span><form action=\"/api/logout\" class=\"fish\" method=\"POST\"><input name=\"logout\" type=\"submit\" value=\"Logout\" /></form></div></body></html>"
             (render/tree-duce (map identity) (render/string-concat-rf) (example user)))))))

(comment



  (meta (example {:name "Mr Bob Dabolina"})) ;; => #:com.zidicat.inview{:doctype ["html"]}

  (render/tree-duce (map identity) (render/js-dom-rf)
                    (-> [:div {} [:h3 {:class "fish"} "test"]]
                        (with-meta {:com.zidicat.inview/doctype [:js]})))
  
  (js/document.getElementById "app")

  (->> [:div {} [:button {:class ["dog"] :onClick (fn [] (js/alert "here"))} "button"]]
       (render/tree-duce (map identity) (render/js-dom-rf) (render/js-dom-render-settings))
       (.appendChild (js/document.getElementById "app")))
  

  (.appendChild (js/document.createElement "div")
                (js/document.createTextNode "Hello, world"))

  )
