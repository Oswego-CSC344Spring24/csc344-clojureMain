(ns clojuremain.core-test
  (:require [clojure.test :refer :all]
            [clojuremain.core :refer :all]))

(deftest evalexp-testp1
  (testing evalexp
    (is (= (evalexp '(and x (or x (and y (not z)))) '{x false, z true}) 'false))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{x true, z false}) 'true))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{x true, z true}) 'true))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{x false, z false}) 'false))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{x false, y true}) 'false))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{x true, y false}) 'true))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{x true, y true}) 'true))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{x false, y false}) 'false))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{z false, y true}) 'x))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{z true, y false}) 'x))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{z true, y true}) 'x))
    (is (= (evalexp '(and x (or x (and y (not z)))) '{z false, y false}) 'x))
    ))


(deftest evalexp-testp2
  (testing evalexp
    (is (= (evalexp '(and (and z false) (or x true false)) '{x false, z true}) 'false))
    (is (= (evalexp '(and (and z false) (or x true false)) '{x true, z false}) 'false))
    (is (= (evalexp '(and (and z false) (or x true false)) '{x true, z true}) 'false))
    (is (= (evalexp '(and (and z false) (or x true false)) '{x false, z false}) 'false))

    ))

(deftest evalexp-testp3
  (testing evalexp
    (is (= (evalexp '(or true a) '{x false}) 'true))
    (is (= (evalexp '(or true a) '{x true})  'true) )

    )

    )
