(ns dribbble-stats.main
  (:gen-class))

(require
  '[manifold.deferred :as d]
  '[byte-streams :as bs]
  '[cheshire.core :as cheshire]
  '[aleph.http :as http]
  '[clojure.core.async :as async :refer [>!! >! <!! <! poll! chan sliding-buffer go-loop put! timeout alts! close!]]
  '[taoensso.timbre :as timbre :refer [log info error spy trace]]
  '[taoensso.timbre.appenders.core :as appenders]
  '[clojure.tools.cli :refer [parse-opts]]
  '[cemerick.url :refer [url]])

(defn- find-next-page [resp]
	(if-some [link (-> resp :headers :link)]
		(->> link
		(re-find #"<([^><]*)>; rel=\"next\"")
		second)))

(defn- follower-request [user]
	(str "https://api.dribbble.com/v1/users/" user "/followers"))

(defn- request-with-access-token [req token]
  (let [url (url req)
        query (:query url)
        new-query (assoc query "per_page" "100" "access_token" token)]
    (str (assoc url :query new-query))))

(defn- create-shots-message [response]
	{:user (get-in response ["follower" "username"])
	 :url (get-in response ["follower" "shots_url"])})

(defn- create-likes-message [user shots-response]
  {:user user
   :url (get shots-response "likes_url")})

(defn- make-request [request]
  (trace "making request to " request)
  ( -> (d/let-flow [response (http/get request)
                    body (d/chain response :body bs/to-string cheshire/parse-string)
                    ratelimit-reset (d/chain response :headers #(get % "x-ratelimit-reset") bigint)]
                   (trace "Got result " body " for request " request)
                   {:status :ok :body body, :next (find-next-page response), :ratelimit-reset ratelimit-reset})
       (d/catch
         #(let [data (ex-data %)
                status (:status data)
                ratelimit-reset (get-in data [:headers "x-ratelimit-reset"])]
            (error % "Got exception for request " request)
            (if (= 429 status)
              {:status :timeout :ratelimit-reset (bigint ratelimit-reset)}
              {:status :unknown-exception :exception %})))))

(defn- timeout-handler [limit-chan source-chan response msg]
  (>!! limit-chan (:ratelimit-reset response))
  (>!! source-chan msg))

(defn- follower-ok-handler [log-chan result-chan source-chan response msg]
  (let [body (:body response)
        next (:next response)]
    (doseq [x body]
      (let [shot-message (create-shots-message x)]
        (>!! log-chan {:count 1 :status :started :url (:url shot-message)})
        (>!! result-chan (create-shots-message x))))
    (when (some? next)
      (>!! log-chan {:count 1 :status :started :url next})
      (>!! source-chan {:url next}))
    (>!! log-chan {:count -1 :status :finished :url (:url msg)})))

(defn- shot-ok-handler [log-chan result-chan source-chan response msg]
  (let [body (:body response)
        next (:next response)]
    (doseq [x body]
      (let [likes-message (create-likes-message (:user msg) x)]
        (>!! log-chan {:count 1 :status :started :url (:url likes-message)})
        (>!! result-chan likes-message)))
    (when (some? next)
      (>!! log-chan {:count 1 :status :started :url next})
      (>!! source-chan {:url next}))
    (>!! log-chan {:count -1 :status :finished :url (:url msg)})))

(defn- likes-ok-handler [log-chan result-chan source-chan response msg]
  (let [body (:body response)
        next (:next response)
        user (:user msg)]
    (>!! result-chan {:user user
                      :likers (into [] (map (fn[x] (get-in x ["user" "username"])) body))})
    (when (some? next)
      (>!! log-chan {:count 1 :status :started :url next})
      (>!! source-chan {:url next :user user}))
    (>!! log-chan {:count -1 :status :finished :url (:url msg)})))

(defn- worker [limit-chan ticker-chan source-chan result-chan log-chan ok-handler token]
  (go-loop []
           (let [[msg tick] (map <!! [source-chan ticker-chan])
                 recur? (empty? (filter nil? [msg tick]))]
             (when recur?
               (d/let-flow [response (make-request (request-with-access-token (:url msg) token))]
                           (case (:status response)
                             :ok (ok-handler log-chan result-chan source-chan response msg)
                             :timeout (timeout-handler limit-chan source-chan response msg )
                             :unknown-exception (>!! log-chan {:count -1 :status :error :url (:url msg)})))
               (recur)))))

(defn- start-finish-watcher [log-chan]
  (go-loop [count 0 initialized false]
           (if (and initialized (= 0 count))
             0
             (if-some [request (<! log-chan)]
                      (do
                        (info "Got status " (:status request) " for request " (:url request) ". Remaining " (+ count (:count request)))
                        (recur (+ count (:count request)) true))
                      count))))

(defn- sleep-mills [rate-limit-time]
  (- (* rate-limit-time 1000) (System/currentTimeMillis)))

(defn- start-ticker [limit-chan ticker-chan]
  (async/go-loop [last-rate-limit 0]
                 (let [sleep-mills (sleep-mills last-rate-limit)
                       [msg ch] (alts! [limit-chan (timeout sleep-mills)])
                       sheduled? (not= limit-chan ch)]
                   (when (or sheduled? (some? msg))
                     (let [new-rate-limit (if sheduled?
                                            (+ 60 (/ (System/currentTimeMillis) 1000))
                                            (max last-rate-limit msg))]
                       (when sheduled?
                         (info "sending ticks")
                         (dotimes [n 60] (>! ticker-chan 1)))
                       (recur new-rate-limit))))))

(defn- top-10-likers [likers]
  (take 10 (into
             (sorted-map-by
               #(compare [(get likers %2) %2]
                         [(get likers %1) %1]))
             likers)))

(defn get-top-likers [username token]
  (let [limit-chan (chan (sliding-buffer 10))
        ticker-chan (chan (sliding-buffer 60))
        follower-chan (chan 1000)
        shot-chan (chan 1000)
        like-chan (chan 1000)
        result-chan (chan)
        log-chan (chan)
        finish-watcher-chan (start-finish-watcher log-chan)
        url (follower-request username)]
    (worker limit-chan ticker-chan like-chan result-chan log-chan likes-ok-handler token)
    (worker limit-chan ticker-chan shot-chan like-chan log-chan shot-ok-handler token)
    (worker limit-chan ticker-chan follower-chan shot-chan log-chan follower-ok-handler token)
    (start-ticker limit-chan ticker-chan)
    (>!! log-chan {:count 1 :status :started :url url})
    (>!! follower-chan {:url url})
    (go-loop [result {}]
             (let [[msg ch] (alts! [finish-watcher-chan result-chan] :priority true)]
               (if (= ch finish-watcher-chan)
                 (do
                   (doseq [ch [limit-chan ticker-chan follower-chan shot-chan like-chan result-chan log-chan]]
                     (close! ch))
                   (top-10-likers result))

                 (recur (reduce #(if (contains? %1 %2)
                                   (assoc %1 %2 (inc (get %1 %2)))
                                   (assoc %1 %2 1))
                                result
                                (:likers msg))))))))

(def cli-options
  [["-u" "--user USER" "user"
    :default "zbhatti89"]
   ["-t" "--token TOKEN" "token"
    :default "f93e8179c62f25e0c6a814b4e9b0a090e0eda6081a4d23b0df50d9e0e64db455"]
   ["-v" "--verbose" "write all logs to console"]
   ["-h" "--help"]])

(defn -main [& args]
  (let [parsed-args (parse-opts args cli-options)
        help? (get-in parsed-args [:options :help])
        verbose? (get-in parsed-args [:options :verbose])
        user (get-in parsed-args [:options :user])
        token (get-in parsed-args [:options :token])]
    (when-not verbose?
      (timbre/set-config!
        {:appenders {:spit (appenders/spit-appender {:fname "log"})}
         :level :debug
         :output-fn timbre/default-output-fn}))
    (if help?
      (println (:summary parsed-args))
      (do
        (println "Getting top 10 likers for " user "'s followers. It can take some time. For help run: lein run -- -h")
        (println (<!! (get-top-likers user token)))))))
