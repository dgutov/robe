(require 'ert-x)

(ert-deftest signature-for-simple-args ()
  (should (string= (robe-signature '("C" t "foo") '(("req" "a") ("req" "b")))
                   "C#foo(a, b)")))

(ert-deftest signature-for-rest ()
  (should (string= (robe-signature '("C" t "foo") '(("rest" "items")))
                   "C#foo(items...)")))

(ert-deftest signature-for-block ()
  (should (string= (robe-signature '("C" t "foo") '(("block" "pred")))
                   "C#foo(&pred)")))

(ert-deftest signature-for-opt ()
  (should (string= (robe-signature '("C" t "foo") '(("req" "a") ("opt" "b")))
                   "C#foo(a, [b])")))

(ert-deftest signature-for-nameless-args ()
  (should (string= (robe-signature '("C" t "foo")
                                   '(("req") ("req") ("rest") ("block")))
                   "C#foo(arg1, arg2, args..., &block)")))

(ert-deftest signature-for-class-method ()
  (should (string= (robe-signature '("A::B" nil "bar") nil) "A::B.bar()")))

(ert-deftest signature-font-lock ()
  (should (equal-including-properties
           (robe-signature '("A::B" t "foo") '(("req" "a") ("rest" "b")))
           (ert-propertized-string
            `(face ,font-lock-type-face) "A" nil "::"
            `(face ,font-lock-type-face) "B" nil "#"
            `(face ,font-lock-function-name-face) "foo" nil "("
            `(face ,robe-em-face) "a" nil ", "
            `(face ,robe-em-face) "b..." nil ")"))))

(ert-deftest signature-current-arg-in-bold ()
  (let* ((sig (robe-signature '("C" nil "foo")
                              '(("req" "a") ("req" "b") ("req" "c")) 2))
         (value (get-text-property 9 'face sig)))
    (should (consp value))
    (should (memq 'bold value))))

(ert-deftest signature-rest-arg-in-bold ()
  (let* ((sig (robe-signature '("C" nil "foo") '(("req" "a") ("rest" "b")) 4))
         (value (get-text-property 9 'face sig)))
    (should (consp value))
    (should (memq 'bold value))))

(ert-deftest call-at-point-simple ()
  (should (equal (with-temp-buffer
                   (insert "foo")
                   (robe-call-at-point))
                 '("foo"))))

(ert-deftest call-at-point-keyword ()
  (should (null (with-temp-buffer
                   (insert (propertize "foo" 'face font-lock-keyword-face))
                   (robe-call-at-point)))))

(ert-deftest call-at-point-function-call ()
  (should (equal (with-temp-buffer
                   (insert "foo(bar")
                   (robe-call-at-point))
                 '("foo" . 1))))

(ert-deftest call-at-point-function-definition ()
  (should (null (with-temp-buffer
                  (insert (ert-propertized-string
                           `(face ,font-lock-function-name-face) "foo"
                           nil "(bar"))
                   (robe-call-at-point)))))

(ert-deftest call-at-point-complex ()
  (should (equal (with-temp-buffer
                   (insert "bar(foo, baz(t, p), \"A,Z\", ")
                   (robe-call-at-point))
                 '("bar" . 4))))

(ert-deftest call-at-point-nested ()
  (should (equal (with-temp-buffer
                   (insert "bar(foo, baz(t, p")
                   (robe-call-at-point))
                 '("baz" . 2))))

(ert-deftest call-at-point-just-before-comma ()
  (should (equal (with-temp-buffer
                   (insert "bar(foo,")
                   (forward-char -1)
                   (robe-call-at-point))
                 '("bar" . 1))))

(ert-deftest call-at-point-just-after-paren ()
  (should (equal (with-temp-buffer
                   (insert "bar(")
                   (robe-call-at-point))
                 '("bar" . 1))))
