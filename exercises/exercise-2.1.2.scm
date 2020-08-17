;; <list> ::= () | (<datum> . <list>)
;; <multi-datum> ::= <datum> | <datum> <multi-datum>
;; <dotted-datum> ::= (<multi-datum> . <datum>)
;; <vector> ::= #() | #(<datum> <vector>)
;; <datum> ::= <number> | <symbol> | <boolean> | <string> | <list> | <dotted-datum> | <vector>

;; <list>
;; => (<datum> . <list>)
;; => (<boolean> . <list>)
;; => (#t . <list>)
;; => (#t . (<datum> . <list>))
;; => (#t . (<dotted-datum> . <list>))
;; => (#t . ((<multi-datum> . <datum>) . <list>))
;; => (#t . ((<datum> . <datum>) . <list>))
;; => (#t . ((foo . <datum>) . <list>))
;; => (#t . ((foo . <list>) . <list>))
;; => (#t . ((foo . ()) . <list>))
;; => (#t . ((foo . ()) . (<datum> . <list>)))
;; => (#t . ((foo . ()) . (3 . <list>)))
;; => (#t . ((foo . ()) . (3 . ()))); 