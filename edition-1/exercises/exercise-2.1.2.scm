;; <datum-+> ::= <datum> | <datum> <datum-+>
;; <datum-*> ::= <null> | <datum><datum-*>
;; <list> ::= (<datum-*>)
;; <dotted-datum> ::= (<datum-+> . <datum>)
;; <vector> ::= #(<datum-*>)
;; <datum> ::= <number> | <symbol> | <boolean> | <string> | <list> | <dotted-datum> | <vector>

;; <list>
;; => (<datum-*>)
;; => (<datum> <datum-*>)
;; => (<datum> <datum-*>)
;; => (<boolean> <datum-*>)
;; => (#t <datum-*>)
;; => (#t <datum> <datum-*>)
;; => (#t <dotted-datum> <datum-*>)
;; => (#t (<datum-+> . <datum>) <datum-*>)
;; => (#t (<datum> . <datum>) <datum-*>)
;; => (#t (foo . <datum>) <datum-*>)
;; => (#t (foo . <list>) <datum-*>)
;; => (#t (foo . (<datum-*>)) <datum-*>)
;; => (#t (foo . (<null>)) <datum-*>)
;; => (#t (foo . ()) <datum-*>)
;; => (#t (foo . ()) <datum> <datum-*>)
;; => (#t (foo . ()) <number> <datum-*>)
;; => (#t (foo . ()) 3 <datum-*>)
;; => (#t (foo . ()) 3 <null>)
;; => (#t (foo . ()) 3)