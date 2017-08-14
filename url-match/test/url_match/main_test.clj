(ns url-match.main-test
  (:require [clojure.test :refer :all]
            [url-match.main :refer :all]))

(deftest a-test
  (testing "recognize with path and queryparam"
  	(is (= {"user" "aaa", "id" "123", "offset" "1"} 
  		(recognize 
  			(new-pattern "host(dribble.com); path(?user/status/?id); queryparam(offset=?offset);") 
  			"http://dribble.com/aaa/status/123?offset=1"))))

  (testing "recognize with path and two queryparam names"
  	(is (= {"user" "aaa", "id" "123", "offset" "1", "kek" "aaa"} 
  		(recognize 
  			(new-pattern "host(dribble.com); path(?user/status/?id); queryparam(offset=?offset); queryparam(lol=?kek)") 
  			"http://dribble.com/aaa/status/123?offset=1&lol=aaa"))))

  (testing "nil if host doesn't match"
  	(is (= nil 
  		(recognize 
  			(new-pattern "host(google.com); path(?user/status/?id); queryparam(offset=?offset); queryparam(lol=?kek)") 
  			"http://dribble.com/aaa/status/123?offset=1&lol=aaa"))))

  (testing "nil if path mismatch"
  	(is (= nil
  		(recognize 
  			(new-pattern "host(dribble.com); path(?user/not-found/?id); queryparam(offset=?offset); queryparam(lol=?kek)") 
  			"http://dribble.com/aaa/status/123?offset=1&lol=aaa"))))

  (testing "nil if queryparam mismatch"
  	(is (= nil
  		(recognize 
  			(new-pattern "host(dribble.com); path(?user/status/?id); queryparam(lol2=?kek)") 
  			"http://dribble.com/aaa/status/123?offset=1&lol=aaa"))))

  (testing "recognize without path"
  	(is (= {"offset" "1", "kek" "aaa"} 
  		(recognize 
  			(new-pattern "host(dribble.com); queryparam(offset=?offset); queryparam(lol=?kek)") 
  			"http://dribble.com/aaa/status/123?offset=1&lol=aaa"))))

  (testing "recognize without queryparam"
  	(is (= {"user" "aaa", "id" "123"} 
  		(recognize 
  			(new-pattern "host(dribble.com); path(?user/status/?id)") 
  			"http://dribble.com/aaa/status/123?offset=1"))))

  (testing "return empty map if pattern doesn't have path and queryparam"
  	(is (= {} 
  		(recognize 
  			(new-pattern "host(dribble.com)") 
  			"http://dribble.com/aaa/status/123?offset=1&lol=aaa"))))

  )
