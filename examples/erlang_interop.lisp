; Call Erlang's lists:reverse function
(print "Reverse list:")
(print (erlang-call @lists @reverse (list 1 2 3 4 5)))

; Call Erlang's erlang:length function
(print "\nList length:")
(print (erlang-call @erlang @length (list "a" "b" "c")))

; Call Erlang's lists:sum function
(print "\nSum of numbers:")
(print (erlang-call @lists @sum (list 10 20 30 40)))

; Convert atom to string
(print "\nAtom to binary:")
(print (erlang-call @erlang @atom_to_binary @hello))

; Check if value is an atom
(print "\nIs atom check:")
(print (erlang-call @erlang @is_atom @test))
(print (erlang-call @erlang @is_atom "test"))
