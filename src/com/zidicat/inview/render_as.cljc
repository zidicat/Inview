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

(def ^:private requires-separate-close-tag? (complement #{:area :base :br :col :embed :hr :img :input :link :meta :source :track :wbr :svg})) #_ #{:script :style :textarea :title :svg :template}

(defn- strip-quotes [s] (some-> s (string/replace #"['\"]" "")))
(defn- esc-entities [s] (some-> s (string/replace #"<" "&lt;") (string/replace #">" "&gt;")))

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
