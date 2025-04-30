(ns com.zidicat.hiccup-test 
  (:require [clojure.test :refer [deftest testing is are] :as test]
            [com.zidicat.inview :as html]
            [com.zidicat.inview.render-as :as render]
            [hiccup.compiler :as hic]
            [taipei-404.html :as parse]
            [hickory.core :as hickory]
            [clojure.string :as string]
            [com.zidicat.inview.render-p :as render-p])
  (:import [java.io StringWriter]))

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
  [:form]  (html/remove-class "ajaxform"))

(defn parser [f]
  (nth (hickory/as-hiccup (hickory/parse (slurp f))) 2))

(html/def-view alternative-logged-in-user [user]
  {:file             "test/com/zidicat/inview-test.html"
   :parser           parser
   :strip-whitespace true
   :inline           true}
  [:.user] (html/content (:name user))
  [:form]  (html/add-class "fish")
  [:form]  (html/remove-class "ajaxform"))

(deftest rendering-and-parsing
  (testing "parsing and rendering"
    (testing "html->hiccup and hiccup match the default parser and render fn"
      (let [user    {:name "Mr Bob Dabolina"}
            alt     (alternative-logged-in-user user)
            default (default-logged-in-user user)]
        (is (= default alt))
        (is (= (parse/html->hiccup (hic/render-html default)) (parse/html->hiccup (hic/render-html alt))))))))

(deftest some-round-trips
  (testing "tree-duce"
    (let [user    {:name "Mr Bob Dabolina"}
          alt     (alternative-logged-in-user user)
          default (default-logged-in-user user)]
      (testing "different parsers produce the same hiccup"
        (is (= default alt)))
      (testing "can render to string"
        (is (= (into [[:!DOCTYPE {:html true}]] (parse/html->hiccup (hic/render-html default)))
               (-> default
                   render/render-str
                   parse/html->hiccup)))))))

(deftest tree-duce-p
  (let [user   {:name "Mr Bob Dabolina"}]
    (testing "runs in cljs"
      (let [expected "<!DOCTYPE html><html><head></head><body><div class=\"login\"><form action=\"/api/login\" class=\"fish\" method=\"POST\"><fieldset><label for=\"login-id\">Login:</label><input name=\"login-id\" type=\"text\" /><label for=\"login-password\">Password:</label><input name=\"login-password\" type=\"password\" /><input name=\"login\" type=\"submit\" value=\"Login\" /></fieldset></form></div><div class=\"logout\"><span>Logged in as <span class=\"user\">Mr Bob Dabolina</span></span><form action=\"/api/logout\" class=\"fish\" method=\"POST\"><input name=\"logout\" type=\"submit\" value=\"Logout\" /></form></div></body></html>"]
        (testing "string-tree-ducer"
          (is (= expected
                 (render-p/tree-duce (map identity) (render-p/string-tree-ducer) (default-logged-in-user user)))))
        (testing "string-writer-tree-ducer"
          (is (= expected
                 (render-p/tree-duce (map identity) (render-p/writer-tree-ducer (StringWriter.)) (default-logged-in-user user))))))
      (testing "noop-tree-ducer"
        (is (= (default-logged-in-user user)
               (render-p/tree-duce (map identity) (render-p/->NoopTreeDucer) (default-logged-in-user user))))
        (is (= #:com.zidicat.inview{:doctype ["html"]}
               (meta (render-p/tree-duce (map identity) (render-p/->NoopTreeDucer) (default-logged-in-user user)))))))))

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
                [:input {:type "submit", :name "logout", :value "Logout"}]]]]]
        user    {:name "Mr Bob Dabolina"}
        dom     (alternative-logged-in-user user)]
    (prn 'render-str)
    (time (render/render-str dom))
    (prn 'treeduce-p 'str-concat)
    (time (render-p/tree-duce (map identity) (render-p/string-tree-ducer) dom))
    (prn 'treeduce-p 'stringwriter)
    (time (render-p/tree-duce (map identity) (render-p/writer-tree-ducer (StringWriter.)) dom)))  

  )
