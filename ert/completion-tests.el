(require 'ert-x)
(require 'robe)

(ert-deftest complete-instance-variables-collects-assignments ()
  (with-temp-buffer
    (insert "
class A
  def foo
    @f1 = 1
    @f2 = @f3
  end
end")
    (ruby-mode)
    (should (equal '("@f2" "@f1")
                   (robe-complete--instance-variables)))))

(ert-deftest complete-instance-variables-ignores-comments ()
  (with-temp-buffer
    (insert "
class A
  def foo
    # @f1 = 1
    @f2 = @f3
  end
end")
    (ruby-mode)
    (should (equal '("@f2")
                   (robe-complete--instance-variables)))))

(ert-deftest complete-local-variables-collects-assignments-in-method ()
  (with-temp-buffer
    (insert "
class A
  def foo
    abc = 1
  end

  def bar
    tee = 4 + qux
    xyz = 5 # def bar fake
    ")
    (should (equal '("xyz" "tee")
                   (robe-complete--local-variables "bar")))))

(ert-deftest complete-local-variables-in-class-method ()
  (with-temp-buffer
    (insert "
class A
  def A.foo(ccc)
    abc = 1
    ")
    (should (equal '("abc" "ccc")
                   (robe-complete--local-variables "foo")))))

(ert-deftest complete-local-variables-works-outside-of-methods ()
  (with-temp-buffer
    (insert "
abc = 1
tee = 4 + qux
")
    (should (equal '("tee" "abc")
                   (robe-complete--local-variables nil)))))

(ert-deftest complete-local-variables-includes-method-arguments ()
  (insert "
class A
  def foo(a1, a2: b2, a3:, *rest)
    xyz = 5
    ")
    (should (equal '("xyz" "rest" "a3" "a2" "a1")
                   (robe-complete--local-variables "foo"))))

(ert-deftest complete-local-variables-includes-block-arguments ()
  (insert "
class A
  def foo
    [a1, a2].blarg do |b, z, (x, c)|
    ")
    (should (equal '("c" "x" "z" "b")
                   (robe-complete--local-variables "foo"))))
