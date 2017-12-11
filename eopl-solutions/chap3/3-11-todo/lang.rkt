(module lang (lib "eopl.ss" "eopl")
  ;; grammar for the LET language

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
      (nullary-op ("emptylist") string)
      (unary-op ((or "zero?" "minus" "car" "cdr" "null?")) string)
      (binary-op ((or "+" "-" "*" "/" "equal?" "greater?" "less?" "cons")) string)
      (n-ary-op ("list") string)
      ))

  (define the-grammar
    '((program (expression) a-program)

      (expression (number) const-exp)
      (expression
       (binary-op "(" expression "," expression ")")
       binary-exp)

      (expression
       (unary-op "(" expression ")")
       unary-exp)

      (expression
       ("if" expression "then" expression "else" expression)
       if-exp)

      (expression (identifier) var-exp)

      (expression
       ("let" identifier "=" expression "in" expression)
       let-exp)

      (expression
       (nullary-op) nullary-exp)

      (expression
       (n-ary-op "(" (separated-list expression ",") ")")
       n-ary-exp)
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
