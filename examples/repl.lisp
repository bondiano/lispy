(define repl (lambda ()
  (begin
    (define input (read))

    (if (= input 'quit)
        (begin
          (print "Goodbye!")
          nil)
        (begin
          (print (eval input))
          (repl))))))

(print "Welcome to the Lispy REPL!")
(print "Enter an expression (or 'quit' to exit):")

(repl)
