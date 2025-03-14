(require 'ert-x)
(require 'robe)

(ert-deftest signature-for-simple-args ()
  (should (string= (robe-signature '("C" t "foo" (("req" "a") ("req" "b"))))
                   "C#foo(a, b)")))

(ert-deftest signature-for-rest ()
  (should (string= (robe-signature '("C" t "foo" (("rest" "items"))))
                   "C#foo(*items)")))

(ert-deftest signature-for-keyrest ()
  (should (string= (robe-signature '("C" t "foo" (("keyrest" "params"))))
                   "C#foo(**params)")))

(ert-deftest signature-for-block ()
  (should (string= (robe-signature '("C" t "foo" (("block" "pred"))))
                   "C#foo(&pred)")))

(ert-deftest signature-for-opt ()
  (should (string= (robe-signature '("C" t "foo" (("req" "a") ("opt" "b"))))
                   "C#foo(a, [b])")))

(ert-deftest signature-for-nameless-args ()
  (should (string= (robe-signature '("C" t "foo"
                                     (("req") ("req") ("rest") ("block"))))
                   "C#foo(arg1, arg2, *args, &block)")))

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
            `(face ,robe-em-face) "*b" nil ")"))))

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

(ert-deftest signature-tripledot-arg-in-bold ()
  (let* ((sig (robe-signature '("C" t "foo" (("rest" "*") ("block" "&"))) 4))
         (value (get-text-property 6 'face sig)))
    (should (equal "C#foo(...)" sig))
    (should (consp value))
    (should (memq 'bold value))))

(ert-deftest signature-tripledot-with-leading-arg ()
  (let* ((sig (robe-signature '("C" t "foo" (("req" "a") ("opt" "b")
                                             ("rest" "*") ("block" "&")))
                              4)))
    (should (equal "C#foo(a, [b], ...)" sig))))

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

(ert-deftest call-context-after-string ()
  (with-temp-buffer
    (insert "\"\"\.tee")
    (should (equal '("String" nil t)
                   (butlast (robe-call-context) 2)))))

(ert-deftest call-context-after-hash ()
  (with-temp-buffer
    (ruby-mode)
    (insert "{a: 1}.ea")
    (should (equal '("Hash" nil t)
                   (butlast (robe-call-context) 2)))))

(ert-deftest call-context-after-array ()
  (with-temp-buffer
    (ruby-mode)
    (insert "[a, b].ea")
    (should (equal '("Array" nil t)
                   (butlast (robe-call-context) 2)))
    (insert "\nfoo[2].ea")
    (should (equal '("!" nil nil)
                   (butlast (robe-call-context) 2)))
    (insert "\n%w[].ea")
    (should (equal '("Array" nil t)
                   (butlast (robe-call-context) 2)))))

(ert-deftest call-context-after-percent-literal ()
  (with-temp-buffer
    (ruby-mode)
    (insert "\n%w[].ea")
    (should (equal '("Array" nil t)
                   (butlast (robe-call-context) 2)))))

(ert-deftest call-context-after-block ()
  (with-temp-buffer
    (ruby-mode)
    (insert "class C
  def foo
    bar {}.qux")
    (save-excursion (insert "\n\n"))
    (should (equal '("!" "C" nil)
                   (butlast (robe-call-context) 2)))))

(ert-deftest call-context-after-dot-new ()
  (with-temp-buffer
    (insert "M::Foo
  .new(bar: tee)
  .qux")
    (should (equal '("M::Foo" nil t)
                   (butlast (robe-call-context) 2)))))

(ert-deftest call-context-with-local-var ()
  (with-temp-buffer
    (insert "class C
  def foo
    tee = 'abc'
    tee.to_")
    (ruby-mode)
    (should (equal '("String" "C" t)
                   (butlast (robe-call-context) 2)))))

(ert-deftest call-context-inside-class-body ()
  (with-temp-buffer
    (insert "class C
  do_weird_stuff do
  end

  def foo
  end

  ")
    (ruby-mode)
    (should (equal '(nil "C" nil)
                   (butlast (robe-call-context) 2)))))

(ert-deftest call-context-with-dsl ()
  (with-temp-buffer
    (insert "class FooApi
  get do
    ")
    (ruby-mode)
    (should (equal '(nil nil nil)
                   (butlast (robe-call-context) 2)))))

(ert-deftest jump-to-var ()
  (with-temp-buffer
    (insert "def foo\n  abc = 1\n  abc")
    (robe-jump nil)
    (should (looking-at "abc ="))))

(ert-deftest jump-to-var-not-method-call ()
  (with-temp-buffer
    (insert "def foo\n  abc = 1\n  t.abc")
    (cl-letf (((symbol-function 'robe-start) (lambda () (error "Not found"))))
      (should (equal
               (nth 1 (should-error (robe-jump nil)))
               "Not found")))))

(ert-deftest jump-to-var-not-method-call-2 ()
  (with-temp-buffer
    (insert "def foo\n  abc = abc()")
    (forward-char -3)
    (cl-letf (((symbol-function 'robe-start) (lambda () (error "Not found"))))
      (should (equal
               (nth 1 (should-error (robe-jump nil)))
               "Not found")))))

(ert-deftest const-here-p-nested ()
  (with-temp-buffer
    (insert "module Foo
  class ApplicationRecord < ActiveRecord::Base
    primary_abstract_class
  end
end")
    (should (robe--const-here-p "Foo::ApplicationRecord"))
    (should-not (robe--const-here-p "ApplicationRecord"))))

(ert-deftest const-here-p-toplevel ()
  (with-temp-buffer
    (insert "class ApplicationRecord < ActiveRecord::Base
  primary_abstract_class
end")
    (should (robe--const-here-p "ApplicationRecord"))
    (should-not (robe--const-here-p "Foo::ApplicationRecord"))))
