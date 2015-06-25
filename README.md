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

These wonderful macros are developed in clojure's library swiss-arrows (https://github.com/rplevy/swiss-arrows).
These macro functions are equivalent to them too.

- -<>
- -<>>
- some-<>
- some-<>>

## Installation

```
(ql:quickload :arrow-macros)
```

## Notes

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
