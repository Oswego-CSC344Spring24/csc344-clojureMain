(ns clojuremain.core)

(defn evalexp [exp bindings]
  (simplify (nor-convert (bind-values bindings exp))))