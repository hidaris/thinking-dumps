(module lang

    ;; grammar for the LET language

    (lib "eopl.ss" "eopl")

  (require "drscheme-init.rkt")

  (provide (all-defined-out))

  ;;;;;;;;;;;;;;;; grammatical specification ;;;;;;;;;;;;;;;;

  (define the-lexical-spec
    '((whitespace (whitespace) skip)
      (comment ("%" (arbno (not #\newline))) skip)
      (identifier
       (letter (arbno (or letter digit "_" "-" "?")))
       symbol)
      (number (digit (arbno digit)) number)
      (number ("-" digit (arbno digit)) number)
      ))

  (define the-grammar
    '((program (expression) a-program)
      (program (bool-exp) a-bool-program)
      (program (effect) a-effect-program)

      ;; union type for expression and bool-exp
      (Uexpbool (expression) Uexp-exp)
      (Uexpbool (bool-exp) Ubool-exp)

      (effect ("print" "(" Uexpbool ")") print-exp)

      (expression (number) const-exp)
      (expression
        ("-" "(" expression "," expression ")")
        diff-exp)
      ;; 3-7
      (expression
        ("+" "(" expression "," expression ")")
        plus-exp)

      (expression
        ("*" "(" expression "," expression ")")
        mult-exp)

      (expression
        ("/" "(" expression "," expression ")")
        div-exp)

      (expression
        ("if" bool-exp "then" expression "else" expression)
        if-exp)

      (expression (identifier) var-exp)

      (expression
        ("let" (arbno identifier "=" expression) "in" expression)
        let-exp)

      (expression
        ("let*" (arbno identifier "=" expression) "in" expression)
        let*-exp)

      (expression
        ("unpack" (arbno identifier) "=" expression "in" expression)
        unpack-exp)

      ;; add minus by 3-6
      (expression
        ("minus" "(" expression ")")
        minus-exp)

      ;; add 3-8
      (bool-exp
       ("zero?" "(" expression ")")
       zero?-exp)

      (bool-exp
       ("equal?" "(" expression "," expression ")")
       equal?-exp)

      (bool-exp
       ("greater?" "(" expression "," expression ")")
       greater?-exp)

      (bool-exp
       ("less?" "(" expression "," expression ")")
       less?-exp)

      ;; add 3-9
      (expression
        ("cons" "(" expression "," expression ")")
        cons-exp)

      (expression
        ("car" "(" expression ")")
        car-exp)

      (expression
        ("cdr" "(" expression ")")
        cdr-exp)

      (bool-exp
       ("null?" "(" expression ")")
       null?-exp)

      (expression
        ("emptylist")
        emptylist-exp)

      ;; add 3-10
      (expression
        ("list" "(" (separated-list expression ",") ")")
        list-exp)

      ;; add by 3-12
      (expression
        ("cond" (arbno bool-exp "==>" expression) "end")
        cond-exp)

      ))

  ;;;;;;;;;;;;;;;; sllgen boilerplate ;;;;;;;;;;;;;;;;

  (sllgen:make-define-datatypes the-lexical-spec the-grammar)

  (define show-the-datatypes
    (lambda () (sllgen:list-define-datatypes the-lexical-spec the-grammar)))

  (define scan&parse
    (sllgen:make-string-parser the-lexical-spec the-grammar))

  (define just-scan
    (sllgen:make-string-scanner the-lexical-spec the-grammar))

  )
