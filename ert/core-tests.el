(require 'ert-x)
(require 'robe)

(ert-deftest signature-for-simple-args ()
  (should (string= (robe-signature '("C" t "foo" (("req" "a") ("req" "b"))))
                   "C#foo(a, b)")))

(ert-deftest signature-for-rest ()
  (should (string= (robe-signature '("C" t "foo" (("rest" "items"))))
                   "C#foo(items...)")))

(ert-deftest signature-for-block ()
  (should (string= (robe-signature '("C" t "foo" (("block" "pred"))))
                   "C#foo(&pred)")))

(ert-deftest signature-for-opt ()
  (should (string= (robe-signature '("C" t "foo" (("req" "a") ("opt" "b"))))
                   "C#foo(a, [b])")))

(ert-deftest signature-for-nameless-args ()
  (should (string= (robe-signature '("C" t "foo"
                                     (("req") ("req") ("rest") ("block"))))
                   "C#foo(arg1, arg2, args..., &block)")))

(ert-deftest signature-for-class-method ()
  (should (string= (robe-signature '("A::B" nil "bar" nil)) "A::B.bar")))

(ert-deftest signature-font-lock ()
  (should (equal-including-properties
           (robe-signature '("A::B" t "foo" (("req" "a") ("rest" "b"))))
           (ert-propertized-string
            `(face ,font-lock-type-face) "A" nil "::"
            `(face ,font-lock-type-face) "B" nil "#"
            `(face ,font-lock-function-name-face) "foo" nil "("
            `(face ,robe-em-face) "a" nil ", "
            `(face ,robe-em-face) "b..." nil ")"))))

(ert-deftest signature-current-arg-in-bold ()
  (let* ((sig (robe-signature '("C" nil "foo"
                                (("req" "a") ("req" "b") ("req" "c"))) 2))
         (value (get-text-property 9 'face sig)))
    (should (consp value))
    (should (memq 'bold value))))

(ert-deftest signature-rest-arg-in-bold ()
  (let* ((sig (robe-signature '("C" nil "foo" (("req" "a") ("rest" "b"))) 4))
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

(ert-deftest call-at-point-parenless-first-arg ()
  (should (equal (with-temp-buffer
                   (insert "bar foo")
                   (robe-call-at-point))
                 '("bar" . 1))))

(ert-deftest call-at-point-parenless-complex-arg ()
  (should (equal (with-temp-buffer
                   (insert "bar foo, tee(1, 2), pea")
                   (robe-call-at-point))
                 '("bar" . 3))))

(ert-deftest call-at-point-parenless-at-paren ()
  (should (equal (with-temp-buffer
                   (insert "bar {a: 1, b: 2}")
                   (search-backward "{")
                   (robe-call-at-point))
                 '("bar" . 1))))

(ert-deftest call-at-point-parenless-semicolon ()
  (should (equal (with-temp-buffer
                   (insert "bar; foo")
                   (robe-call-at-point))
                 '("foo"))))

(ert-deftest call-at-point-parenless-assign ()
  (should (equal (with-temp-buffer
                   (insert "bar = foo")
                   (robe-call-at-point))
                 '("foo"))))

(ert-deftest call-at-point-parenless-newline ()
  (should (equal (with-temp-buffer
                   (insert "bar\nfoo")
                   (robe-call-at-point))
                 '("foo"))))

(ert-deftest call-at-point-parenless-newline-and-comma ()
  (should (equal (with-temp-buffer
                   (insert "bar foo,\n  baz")
                   (robe-call-at-point))
                 '("bar" . 2))))

(ert-deftest call-at-point-parenless-operator ()
  (should (equal (with-temp-buffer
                   (insert "bar 1 + 1,\n  baz")
                   (ruby-mode)
                   (robe-call-at-point))
                 '("bar" . 2))))

(ert-deftest call-at-point-parenless-js-hash ()
  (should (equal (with-temp-buffer
                   (insert "bar :foo, a: 1, b: 2")
                   (ruby-mode)
                   (robe-call-at-point))
                 ;; should be 2, but meh
                 '("bar" . 3))))

(ert-deftest call-at-point-parenless-hash-rocket ()
  (should (equal (with-temp-buffer
                   (insert "bar :foo, :a => 1, :b => 2")
                   (ruby-mode)
                   (robe-call-at-point))
                 ;; should be 2, but meh
                 '("bar" . 3))))

(ert-deftest call-at-point-inside-a-string ()
  (should (equal (with-temp-buffer
                   (insert "bar :foo, \"baz 1, (2 + 3)\"")
                   (ruby-mode)
                   (search-backward "3")
                   (robe-call-at-point))
                 '("bar" . 2))))

(ert-deftest call-context-simple ()
  (let ((ctx (with-temp-buffer
               (insert "class C
  def foo
    bar.tee
  end
end")
               (ruby-mode)
               (search-backward "tee")
               (robe-call-context))))
    (should (equal "bar" (car ctx)))
    (should (equal "C" (nth 1 ctx)))
    (should (null (nth 2 ctx)))
    (should (equal '("C" t "foo") (nth 3 ctx)))))

(ert-deftest call-context-self ()
  (should (null (with-temp-buffer
                  (insert "self.qux")
                  (car (robe-call-context))))))

(ert-deftest call-context-self-with-spaces ()
  (should (null (with-temp-buffer
                  (insert "self\n  .qux")
                  (car (robe-call-context))))))

(ert-deftest call-context-complex ()
  (should (equal "!" (with-temp-buffer
                       (insert "(bar + tee).qux")
                       (car (robe-call-context))))))
