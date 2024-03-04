(ns clojuremain.core)
(defn nor [x & xs]
  (if (empty? xs)
    (if x (not x) true)
    (apply nor (cons (if x (not x) true) xs))))

(defn simplify [expr]
  (cond
    ;; nor false -> true
    (= expr '(nor false)) true
    ;; nor true -> false
    (= expr '(nor true)) false
    ;; nor (nor x) -> x
    (and (= (first expr) 'nor) (= (count (rest expr)) 1) (= (first (rest expr)) 'nor)) (second (rest expr))
    ;; nor (nor (nor x)) -> (nor x)
    (and (= (first expr) 'nor) (= (count (rest expr)) 1) (= (first (rest expr)) 'nor)) (rest (rest expr))
    ;; nor x x -> (nor x)
    (and (= (first expr) 'nor) (= (count (rest expr)) 2) (= (first (rest expr)) (second (rest expr)))) (list 'nor (first (rest expr)))
    ;; nor x y -> (nor x y)
    (= (count expr) 2) expr
    ;; nor x true -> false
    (and (= (first expr) 'nor) (= (count (rest expr)) 2) (= (second expr) 'true)) false
    ;; nor x false -> (nor x)
    (and (= (first expr) 'nor) (= (count (rest expr)) 2) (= (second expr) 'false)) (list 'nor (first (rest expr)))
    ;; nor x y false -> (nor x y)
    (and (= (first expr) 'nor) (= (count (rest expr)) 3) (= (last expr) 'false)) (butlast expr)
    ;; nor x false false -> (nor x)
    (and (= (first expr) 'nor) (= (count (rest expr)) 3) (= (second expr) 'false) (= (last expr) 'false)) (list 'nor (first (rest expr)))
    ;; nor false false false -> true
    (and (= (first expr) 'nor) (= (count (rest expr)) 3) (= (first expr) 'false) (= (second expr) 'false) (= (last expr) 'false)) true
    ;; nor x y true -> false
    (and (= (first expr) 'nor) (= (count (rest expr)) 3) (= (last expr) 'true)) false
    ;; Default case: return original expression
    :else expr))

(defn nor-convert [expr]
  (cond
    ;; Base case: If expr is a variable or constant, return it
    (not (coll? expr)) expr
    ;; Recursive case: Convert sub-expressions recursively
    :else (cons 'nor (map nor-convert expr))))

(defn deep-substitute [bindings expr]
  (if (coll? expr)
    (mapv #(deep-substitute bindings %) expr)
    (or (bindings expr) expr)))

(defn evalexp [exp bindings]
  (simplify (nor-convert (deep-substitute bindings exp))))

(defn test-cases []
  (let [test-cases [(list 'nor 'false) ; -> true
                    (list 'nor 'true)  ; -> false
                    (list 'nor (list 'nor 'x))  ; -> x
                    (list 'nor (list 'nor (list 'nor 'x)))  ; -> (nor x)
                    (list 'nor (list 'nor (list 'nor (list 'nor 'x))))  ; -> x
                    (list 'nor 'x 'x)  ; -> (nor x)
                    (list 'nor 'x 'x 'x)  ; -> (nor x)
                    (list 'nor 'x 'y)  ; -> (nor x y)
                    (list 'nor 'x 'true)  ; -> false
                    (list 'nor 'x 'false)  ; -> (nor x)
                    (list 'nor 'false 'false)  ; -> true
                    (list 'nor 'x 'y 'false)  ; -> (nor x y)
                    (list 'nor 'x 'false 'false)  ; -> (nor x)
                    (list 'nor 'false 'false 'false)  ; -> true
                    (list 'nor 'x 'y 'true)  ; -> false
                    (list 'nor 'x 'y 'z)]  ; -> (nor x y z)
        expected-results [true
                          false
                          'x
                          (list 'nor 'x)
                          'x
                          (list 'nor 'x)
                          (list 'nor 'x 'y)
                          false
                          (list 'nor 'x)
                          true
                          (list 'nor 'x 'y)
                          (list 'nor 'x)
                          true
                          false
                          (list 'nor 'x 'y 'z)]]
    (map #(= (evalexp % {}) %2) test-cases expected-results)))

(test-cases)
