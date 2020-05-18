# reductio

Small library to partially evaluate a subset of Clojure given a set of bindings.

## Usage:

```clojure
(require '[dev.mksm.reductio :refer [deval]])

(def code '(+ a (+ b (+ c d))))

(deval code {})
;; => the same `code` value because no bindings were substituted

(deval code {'a 1})
;; => `(+ 1 (+ b (+ c d)))`, `a` being substituted for `1`

(deval code {'+ + 'c 1 'd 2})
;; => `(+ a (+ b 3))`, `(+ c d)` substituted for it's value
```
