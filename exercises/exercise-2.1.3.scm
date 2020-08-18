;; <datum-+> ::= <datum> | <datum> <datum-+>
;; <datum-*> ::= <null> | <datum><datum-*>
;; <list> ::= (<datum-*>)
;; <dotted-datum> ::= (<datum-+> . <datum>)
;; <vector> ::= #(<datum-*>)
;; <datum> ::= <number> | <symbol> | <boolean> | <string> | <list> | <dotted-datum> | <vector>

;; (a "mixed" #(bag (of . data)))

;; <list>
;; => (<datum-*>)
;; => (<datum> <datum> <datum> <datum-*>)
;; => (<datum> <datum> <datum> <null>)
;; => (<symbol> <datum> <datum>)
;; => (a <datum> <datum>)
;; => (a <string> <datum>)
;; => (a "mixed" <datum>)
;; => (a "mixed" <vector>)
;; => (a "mixed" #(<datum-*>))
;; => (a "mixed" #(<datum> <datum-*>))
;; => (a "mixed" #(<symbol> <datum-*>))
;; => (a "mixed" #(bag <datum-*>))
;; => (a "mixed" #(bag <dotted-datum><datum-*>))
;; => (a "mixed" #(bag <dotted-datum><null>))
;; => (a "mixed" #(bag <dotted-datum>))
;; => (a "mixed" #(bag (<datum-+> . <datum>)))
;; => (a "mixed" #(bag (<datum> . <datum>)))
;; => (a "mixed" #(bag (<symbol> . <datum>)))
;; => (a "mixed" #(bag (of . <datum>)))
;; => (a "mixed" #(bag (of . <symbol>)))
;; => (a "mixed" #(bag (of . data)))
