(ns dribbble-stats.main-test
  (:require [clojure.test :refer :all]
            [dribbble-stats.main :refer :all]
            [clj-wiremock.core :refer :all]
            [clojure.java.io :as io]
            [clojure.core.async :as async :refer [>!! <!!]]))

(def wiremock-server (server))
(use-fixtures :once (fn [test-suite] (start wiremock-server) (test-suite) (stop wiremock-server)))
(use-fixtures :each (fn [test-to-run] (reset wiremock-server) (test-to-run)))

(let [followers-single-user-response (io/resource "followers-single-user-response.json")
         single-shot-response (io/resource "single-shot-response.json")
         likes-single-user (io/resource "likes-single-user.json")
         user "roman"
         liker-name "simplebits"
         token "1"]
  (deftest single-follower-test
    (with-redefs-fn {#'dribbble-stats.main/follower-request (fn [user] (str "http://localhost:8080/v1/users/" user "/followers"))}
      #(do
         (testing "return empty if followers request returns error"
           (is (empty? (<!! (get-top-likers user token)))))

         (stub {:request  {:method "GET" :url (str "/v1/users/" user "/followers?access_token=" token "&per_page=100")}
                :response {:status 200
                           :body (slurp followers-single-user-response)
                           :headers {:X-RateLimit-Reset "100"}}})

         (testing "return empty if couldn't reach any shots"
           (is (empty? (<!! (get-top-likers user token)))))

         (stub {:request  {:method "GET" :url (str "/v1/users/1/shots?access_token=" token "&per_page=100")}
                :response {:status 200
                           :body (slurp single-shot-response)
                           :headers {:X-RateLimit-Reset "100"}}})

         (testing "return empty if couldn't reach any likes"
           (is (empty? (<!! (get-top-likers user token)))))

         (stub {:request  {:method "GET" :url (str "/v1/shots/1/likes?access_token=" token "&per_page=100")}
                :response {:status 200
                           :body (slurp likes-single-user)
                           :headers {:X-RateLimit-Reset "100"}}})

         (testing "return result with single likes"
           (is (= [[liker-name 1]] (<!! (get-top-likers user token))))))))

  (deftest two-page-test
    (with-redefs-fn {#'dribbble-stats.main/follower-request (fn [user] (str "http://localhost:8080/v1/users/" user "/followers"))}
      #(do
        (stub {:request  {:method "GET" :url (str "/v1/users/" user "/followers?access_token=" token "&per_page=100")}
               :response {:status 200
                          :body (slurp followers-single-user-response)
                          :headers {:X-RateLimit-Reset "100"
                                    :link (str "<http://localhost:8080/v1/users/" user "/followers2>; rel=\"next\"")}}})

        (stub {:request  {:method "GET" :url (str "/v1/users/" user "/followers2?access_token=" token "&per_page=100")}
               :response {:status 200
                          :body (slurp followers-single-user-response)
                          :headers {:X-RateLimit-Reset "100"}}})

        (stub {:request  {:method "GET" :url (str "/v1/users/1/shots?access_token=" token "&per_page=100")}
                :response {:status 200
                           :body (slurp single-shot-response)
                           :headers {:X-RateLimit-Reset "100"
                                     :link (str "<http://localhost:8080/v1/users/1/shots2>; rel=\"next\"")}}})

        (stub {:request  {:method "GET" :url (str "/v1/users/1/shots2?access_token=" token "&per_page=100")}
                :response {:status 200
                           :body (slurp single-shot-response)
                           :headers {:X-RateLimit-Reset "100"}}})

        (stub {:request  {:method "GET" :url (str "/v1/shots/1/likes?access_token=" token "&per_page=100")}
               :response {:status 200
                          :body (slurp likes-single-user)
                          :headers {:X-RateLimit-Reset "100"
                                    :link (str "<http://localhost:8080/v1/shots/1/likes2>; rel=\"next\"")}}})

        (stub {:request  {:method "GET" :url (str "/v1/shots/1/likes2?access_token=" token "&per_page=100")}
               :response {:status 200
                          :body (slurp likes-single-user)
                          :headers {:X-RateLimit-Reset "100"}}})

        (testing "return result with single likes"
             (is (= [[liker-name 8]] (<!! (get-top-likers user token))))))))

  (deftest top-10-likers
    (testing "return empty if arg is empty"
      (is (empty? (#'dribbble-stats.main/top-10-likers {}))))

    (testing "return available sorted likers if count is less than 10"
      (is (= [[:a 20], [:b 10]] (#'dribbble-stats.main/top-10-likers {:b 10, :a 20}))))

    (testing "return 10 sorted likers"
      (let [likers (reduce #(assoc %1 %2 %2) {} (range 20))]
        (is (= [[19 19] [18 18] [17 17] [16 16] [15 15]
                [14 14] [13 13] [12 12] [11 11] [10 10]]
               (#'dribbble-stats.main/top-10-likers likers))))))

  (deftest next-page-parser
    (testing "return nil if arg is nil"
      (is (nil? (#'dribbble-stats.main/find-next-page nil))))

    (testing "return nil if format is invalid"
      (is (nil? (#'dribbble-stats.main/find-next-page {:headers {:link "aaaa"}}))))

    (testing "return next"
      (is (= "https://duckduckgo.com"
             (#'dribbble-stats.main/find-next-page
               {:headers {:link "<https://google.com>; rel=\"prev\", <https://duckduckgo.com>; rel=\"next\""}}))))))
