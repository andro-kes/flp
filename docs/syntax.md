# Syntax Reference

## Ternary operator `?:`

**Form:**
```
cond ? thenExpr : elseExpr
```

Desugars to `(if cond thenExpr elseExpr)`.

**Example:**
```
true ? 1 : 2
```
Evaluates to `1`.

```
false ? 100 : 200
```
Evaluates to `200`.

---

## Raise operator `!`

**Form:**
```
! expr
```

Evaluates `expr` and raises a runtime `LangError` with the evaluated value as the error payload.

**Example:**
```
! 42
```
Raises `LangError { Message = "Raised: VNumber 42" }`.

---

## Arrow loop `->`

**Form:**
```
(-> cond body)
```

Executes `body` in a loop while `cond` evaluates to `true`.

**Example:**
```lisp
(letrec loop (n)
  (-> (< n 5)
    (begin
      (set n (+ n 1))
      n))
  (loop 0))
```

---

## Traditional `if`

**Form:**
```
(if cond thenExpr elseExpr)
```

**Example:**
```
(if true 1 2)
```

---

## Lambda and application

```lisp
(lambda (x) (+ x 1))
((lambda (x) (+ x 1)) 10)
```

---

## Let and letrec

```lisp
(let x 42 x)
(letrec fact (n) (if (= n 0) 1 (* n (fact (- n 1)))) (fact 5))
```
