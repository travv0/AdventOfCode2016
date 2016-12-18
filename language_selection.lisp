0(defparameter *languages* '("C"
                            "Java"
                            "Python"
                            "C++"
                            "R"
                            "C#"
                            "PHP"
                            "JavaScript"
                            "Ruby"
                            "Go"
                            "Swift"
                            "Assembly"
                            "Scala"
                            "Perl"
                            "Shell"
                            "Lua"
                            "Haskell"
                            "Rust"
                            "Fortran"
                            "Pascal"
                            "D"
                            "Lisp"
                            "Julia"
                            "Erlang"
                            "Prolog"
                            "Clojure"
                            "Ada"
                            "Cobol"
                            "Scheme"
                            "J"
                            "TCL"
                            "OCaml"
                            "Forth"))

(defun shuffle (list)
  (let ((len (length list)))
    (loop repeat len do
      (rotatef
       (nth (random len) list)
       (nth (random len) list))
          finally
             (return list))))

(defun range (min max &optional (step 1))
  (loop for i from min below max by step collect i))

(defun pick-languages (list number)
  (map 'list (lambda (x y) (cons x y)) (range 1 (1+ number)) (subseq (shuffle list) 0 (1+ number))))

;; =>
;; ((1 . "Haskell") (2 . "JavaScript") (3 . "C") (4 . "Lisp") (5 . "Rust")
;;                  (6 . "PHP") (7 . "Python") (8 . "Ruby") (9 . "Shell") (10 . "Lua")
;;                  (11 . "Erlang") (12 . "Java") (13 . "Prolog") (14 . "Go") (15 . "OCaml")
;;                  (16 . "Scala") (17 . "C++") (18 . "R") (19 . "Swift") (20 . "Assembly")
;;                  (21 . "Ada") (22 . "Pascal") (23 . "Clojure") (24 . "Fortran") (25 . "Cobol")
