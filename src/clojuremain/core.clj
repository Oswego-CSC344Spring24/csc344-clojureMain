(ns clojuremain.core)
(defn nor [x & ys]
  (if ys
    `(nor ~x (nor ~@ys))
    `(nor ~x)))

(defn nor-convert [expr]
  (cond
    (true? expr) 'false
    (false? expr) 'true
    (and (list? expr) (= 'not (first expr))) (second expr)
    (and (list? expr) (= 'and (first expr))) (apply nor (map nor-convert (rest expr)))
    (and (list? expr) (= 'or (first expr))) (apply nor (map #(list 'not %) (rest expr)))
    :else expr))

(defn simplify [expr]
  (cond
    (and (list? expr) (= 'nor (first expr))) (if (= 1 (count (rest expr)))
                                               (simplify (first (rest expr)))
                                               (cons 'nor (map simplify (rest expr))))
    :else expr))

(defn bind-values [bindings expr]
  (let [lookup (fn [x] (get bindings x x))]
    (if (seq? expr)
      (mapv (fn [x] (bind-values bindings x)) expr)
      (lookup expr))))

(defn evalexp [exp bindings]
  (-> exp
      (bind-values bindings)
      nor-convert
      simplify))

;; Example usage
(def p1 '(and x (or x (and y (not z)))))
(def p2 '(and (and z false) (or x true false)))
(def p3 '(or true a))

(defn test []
  (println "p1: " (evalexp p1 '{x false, z true}))
  (println "p2: " (evalexp p2 '{x true, z false}))
  (println "p3: " (evalexp p3 '{a true})))

(test)

(defn evalexp [exp bindings]
  (simplify (nor-convert (bind-values bindings exp))))
