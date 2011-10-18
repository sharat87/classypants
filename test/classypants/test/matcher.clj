(ns classypants.test.matcher
  (:use [classypants.matcher])
  (:use [clojure.test]))

(deftest test-split-on
  (is (= ['(:a :b) '(:d :e)]
         (split-on :c [:a :b :c :d :e])))
  (is (= ['() '(:d :e)]
         (split-on :c [:c :d :e])))
  (is (= ['(:a :b) '()]
         (split-on :c [:a :b :c]))))

(deftest test-in?
  (is (true? (in? :b [:a :b :c])))
  (is (false? (in? :b [])))
  (is (false? (in? 2 [:a :b :c]))))

(deftest test-get-spec
  (is (= [:a]
         (get-spec ":a"))))

(deftest test-digest-search-spec
  (is (= '(classypants.matcher/matches? "search-term")
         (digest-search-spec 'search-term)))
  (is (= '(classypants.matcher/matches? "search-term")
         (digest-search-spec '[search-term])))
  (is (= '(and (classypants.matcher/matches? "term1") (classypants.matcher/matches? "term2"))
         (digest-search-spec '[term1 :and term2])))
  (is (= '(or (classypants.matcher/matches? "term1") (classypants.matcher/matches? "term2"))
         (digest-search-spec '[term1 term2])))
  (is (= '(or (classypants.matcher/matches? "term1") (classypants.matcher/matches? "term2"))
         (digest-search-spec '[term1 :or term2]))))

(deftest test-paint-match-status
  (is (:matches? (first (paint-match-status (parse-search-str "search-term") [{:path "ha search-term"}])))))
