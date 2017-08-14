(ns url-match.main
  (:gen-class))

(require
  '[cemerick.url]
  '[clojure.string :as str]
  '[clojure.tools.cli :refer [parse-opts]])

(defn- pattern-with-host [state pattern-part]
  (when-let [result (second (re-find #"^host\((.+)\)$" pattern-part))]
    (assoc state :host result)))

(defn- pattern-with-path [state pattern-part]
  (when-let [result (second (re-find #"^path\((.+)\)$" pattern-part))]
    (assoc state :path result)))

(defn- pattern-with-queryparam [state pattern-part]
  (when-let [result (re-find #"^queryparam\((.+)=\?(.+)\)$" pattern-part)]
    (assoc-in state [:queryparam (nth result 1)] (nth result 2))))

(defn- parse-pattern [state pattern-part]
  (first (filter some? (map #(apply %1 (vector state pattern-part))
    [pattern-with-host pattern-with-path pattern-with-queryparam]))))

(defn- merge-querty-params [query-param-pattern query-param-url]
  (let [pattern-key (keys query-param-pattern)
        value (map #(get query-param-url %1) pattern-key)
        result (filter #(some? (val %1)) (zipmap (vals query-param-pattern) value))]
    (when (= (count query-param-pattern) (count result))
      result)))

(defn- parse-path [path-url path-pattern]
  (let [path-bind (filter #(str/starts-with? %1 "?") (str/split path-pattern #"/"))
        path-regex (reduce #(str/replace %1 %2 "([^/]+)") path-pattern path-bind)
        parsed-path (re-find (re-pattern path-regex) path-url)]
        (when (some? parsed-path)
          (zipmap (map #(subs %1 1) path-bind) (subvec parsed-path 1)))))

(defn- merge-all-or-nil [& args]
  (when (empty? (filter nil? args))
    (apply merge args)))

(defn new-pattern [pattern-string]
  (reduce parse-pattern {} (map str/trim (str/split pattern-string #";"))))

(defn recognize [pattern url]
  (let [u (cemerick.url/url url)]
        (when (= (:host u) (:host pattern))
          (merge-all-or-nil
            (if-let [path (:path pattern)]
              (parse-path (:path u) path) {})
            (if-let [queryparam (:queryparam pattern)]
              (merge-querty-params queryparam (:query u)) {})))))

(def cli-options
  [["-u" "--url URL" "url"
    :default "http://dribble.com/aaa/status/123?offset=1&lol=aaa"]
   ["-p" "--pattern PATTERN" "pattern"
    :default "host(dribble.com); path(?user/status/?id); queryparam(offset=?offset); queryparam(lol=?kek)"]
   ["-h" "--help"]])

(defn -main [& args]
  (let [parsed-args (parse-opts args cli-options)
        help? (get-in parsed-args [:options :help])
        url (get-in parsed-args [:options :url])
        pattern (get-in parsed-args [:options :pattern])]
    (if help?
      (println (:summary parsed-args))
      (do
        (println "parsing url " url " with pattern " pattern)
        (println "result: "(recognize (new-pattern pattern) url))))))
