# Arrow-macros

Arrow-macros provides clojure-like arrow macros (ex. ->, ->>) and diamond wands in swiss-arrows (https://github.com/rplevy/swiss-arrows).

[![Build Status](https://travis-ci.org/hipeta/arrow-macros.svg)](https://travis-ci.org/hipeta/arrow-macros)

## Examples

```
(-> 1 (+ 2 3) #'1+ 1+ (lambda (x) (+ x 1)) (1+)) ; => 10
```

```
(->> (list 1 2 3 4 5)
  (mapcar #'1+)
  (reduce #'+)) ; => 20
```

```
(-<> "abcdefghijklmnopqrstuvwxyz"
  (ppcre:scan-to-strings "j.*q" <>)
  (sort #'string>)
  (string-upcase :end 1)) ; => "Qponmlkj"
```

```
(-> (get-some-webpage-uri id)
  drakma:http-request
  babel:octets-to-string
  json->list)
```

## Installation

```
(ql:quickload :arrow-macros)
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

These wonderful macros are developed in clojure's library swiss-arrows (https://github.com/rplevy/swiss-arrows).
These macro functions are almost equivalent to them too.

- -<>
- -<>>
- some-<>
- some-<>>


```
(-<> 1 1+ #'1+ (1+) (lambda (x) (1+ x)))  ; => 5
```

```
(-<> 1
  (+ 2)     ; => (+ 1 2)
  (+ 3 <>)  ; => (+ 3 (+ 1 2))
  (+ 4)     ; => (+ (+ 3 (+ 1 2) 4)  => 10
```

```
(-<>> 1
  (+ 2)     ; => (+ 2 1)
  (+ <> 3)  ; => (+ (+ 2 1) 3)
  (+ 4)     ; => (+ 4 (+ (+ 2 1) 3)) => 10
```

Arrow-macros can also write nested diamond-wands like:

```
(-<>> (list 1 2 3)
  (mapcar #'1+)
  (-<> <> (sort #'>))
  (mapcar #'1+))       ; => (5 4 3)
```

```
(-<> 1 (+ <> <>) (-<> (+ <> <> <>)))  ; => 6
```

Be aware that the inner diamond-wand can refer to outer diamond symbol <> only in initial form.

#### Side effect in diamond wand and <!> symbol

Diamond wand can write multiple <> symbols in each expression.

```
(let ((x 0))
  (list (-<> x
          incf
          (+ <> <>)
          (+ <> <>))   ; => (+ (+ (incf x) (incf x)) (+ (incf x) (incf x))) => 10
        x))            ; => (10 4)
        
```

This means some expressions in diamond wands can be evaluated for 2 or more times.
If you use expressions which have side effects in diamond wands, there is a risk that might happen what you don't intend.
One solution is you don't use diamond wands for expressions containing side effects.
Another solution is using <!> symbol in diamond wands.
This may be a unique feature in this library. <!> symbol ensure previous expression evaluted only once.

```
(let ((x 0))
  (list (-<> x
          incf <!>
          (+ <> <>)
          (+ <> <>))
        x))            ; => (4 1)

(-<> (drakma:http-request <some-address>) <!>
  (some-converter <> :encode (detect-encoding-of <>)))  ; => this sends http request only once

```
Original library (https://github.com/rplevy/swiss-arrows) doesn't have this feature. I need this for my local small project so add it. It doesn't change the original diamond wand behavior but if you find any problems or improvements, please report me.


## Notes

(Nov 26, 2020)

For author's misunderstading, diamond wand specification had been uncorrect (see issue <a href='https://github.com/hipeta/arrow-macros/issues/3'>#3</a>).
It has been fixed and the changes can make this library independence from code walker so arrow-macros is now smaller and slimer than old version.


(June 2015)

Build verified on sbcl, ccl, abcl, allegro, clisp.
But diamond-wand tests fail on abcl, allegro, clisp.
Failed on ecl while loading hu.dwim.walker.
Not tested on cmucl.
(contributed by [guicho271828](https://github.com/guicho271828), thanks!)

(May 7, 2015)

On sbcl for windows, arrow-macros can be built now.

<del>
<p>(April 9, 2015)</p>

<p>If you use sbcl on windows, this library could not be built on your system.
Because this library uses hu.dwim.util (via hu.dwim.walker) on current version (see issue <a href='https://github.com/hipeta/arrow-macros/issues/1'>#1</a>) and the quicklisp dist update in April 2015 has not included hu.dwim.util latest updates.
This would be resolved in the next month quicklisp dist update, probably.
But if you want to use this library on sbcl for windows immediately, version 0.2.1 is available. (This version has issue <a href='https://github.com/hipeta/arrow-macros/issues/1'>#1</a> problem, but it is edge case, I think)</p>
</del>

## License

Arrow-macros is under the MIT License, see LICENSE file.
