(ert-deftest signature-for-simple-args ()
  (should (string= (robe-signature '("C" t "foo") '(("req" "a") ("req" "b")))
                   "C#foo(a, b)")))

(ert-deftest signature-for-rest ()
  (should (string= (robe-signature '("C" t "foo") '(("rest" "items")))
                   "C#foo(items...)")))

(ert-deftest signature-for-block ()
  (should (string= (robe-signature '("C" t "foo") '(("block" "pred")))
                   "C#foo(&pred)")))

(ert-deftest signature-for-nameless-args ()
  (should (string= (robe-signature '("C" t "foo")
                                   '(("req") ("req") ("rest") ("block")))
                   "C#foo(arg1, arg2, args..., &block)")))

(ert-deftest signature-for-class-method ()
  (should (string= (robe-signature '("A::B" nil "bar") nil) "A::B.bar()")))
