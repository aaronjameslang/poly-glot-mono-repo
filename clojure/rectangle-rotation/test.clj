(ns kata.test
  (:require [clojure.test :refer :all]))

;; Load the kata.clj file explicitly
(load-file "kata.clj")

(defn tester [a b exp]
  (testing (str "(kata/rectangle-rotation " a " " b ")")
    (is (= (kata/rectangle-rotation a b) exp))))

(deftest basic-tests
  (tester 6 4 23)
  (tester 30 2 65)
  (tester 8 6 49)
  (tester 16 20 333))

;; Run tests when file is executed directly
(clojure.test/run-tests)
