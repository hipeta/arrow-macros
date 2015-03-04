# Arrow-macros

Arrow-macros provides clojure-like arrow macros (ex. ->, ->>) and diamond wands in swiss-arrows (https://github.com/rplevy/swiss-arrows).

## Examples

```
(-> 1 (+ 2 3) #'1+ 1+ (lambda (x) (+ x 1)) (1+)) ; => 10
```

```
(->> (list 1 2 3 4 5)
  (mapcar #'1+)
  (reduce #'+)) => 20
```

```
(-<> "abcdefghijklmnopqrstuvwxyz"
  (ppcre:scan-to-strings "j.*q" <>)
  (sort #'string>)
  (string-upcase :end 1)) => "Qponmlkj"
```

```
(-> (get-some-webpage-uri id)
  drakma:http-request
  babel:octets-to-string
  json->list)
```

## APIs

### Clojure-like macros

These macro functions are equivalent to clojure's ones.

- ->
- ->>
- some->
- some->>
- as->
- cond->
- cond->>

### Diamond wands

These wonderful macros are developed by clojure's library swiss-arrows (https://github.com/rplevy/swiss-arrows).
These macro functions are equivalent to them too.

- -<>
- -<>>
- some-<>
- some-<>>

## Installation

```
(ql:quickload :arrow-macros)
```

## License

Arrow-macros is under the MIT License, see LICENSE file.
