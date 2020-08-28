;; In Javascript

"use strict"
(x) => (y) => ((x) => x(y))(x)

(z) => ((a,b,c) => a((a) => a + c, b))(f,x) => f(z(x))

;; The scope of the bindings are same in ES6 strict mode as that of Scheme