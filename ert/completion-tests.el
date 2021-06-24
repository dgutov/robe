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
                   (mapcar #'robe--variable-name
                           (robe-complete--instance-variables))))))

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
                   (mapcar #'robe--variable-name
                           (robe-complete--instance-variables))))))

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
                   (mapcar #'robe--variable-name
                           (robe-complete--local-variables "bar"))))))

(ert-deftest complete-local-variables-in-class-method ()
  (with-temp-buffer
    (insert "
class A
  def A.foo(ccc)
    abc = 1
    ")
    (should (equal '("abc" "ccc")
                   (mapcar #'robe--variable-name
                           (robe-complete--local-variables "foo"))))))

(ert-deftest complete-local-variables-works-outside-of-methods ()
  (with-temp-buffer
    (insert "
abc = 1
tee = 4 + qux
")
    (should (equal '("tee" "abc")
                   (mapcar #'robe--variable-name
                           (robe-complete--local-variables nil))))))

(ert-deftest complete-local-variables-includes-method-arguments ()
  (insert "
class A
  def foo(a1, a2: b2, a3:, *rest, &block)
    xyz = 5
    ")
    (should (equal '("xyz" "block" "rest" "a3" "a2" "a1")
                   (mapcar #'robe--variable-name
                           (robe-complete--local-variables "foo")))))

(ert-deftest complete-local-variables-includes-block-arguments ()
  (insert "
class A
  def foo
    [a1, a2].blarg do |b, z, (x, c)|
    ")
    (should (equal '("c" "x" "z" "b")
                   (mapcar #'robe--variable-name
                           (robe-complete--local-variables "foo")))))

(ert-deftest complete-local-variables-includes-blocks-with-parenless-calls ()
  (insert "
class A
  def foo
    tee \"abc\" do |a, b|
")
  (should (equal '("b" "a")
                 (mapcar #'robe--variable-name
                           (robe-complete--local-variables "foo")))))

(ert-deftest complete-local-variables-after-paren ()
  (insert "
class A
  def foo
    if (qux = 4)
      return true
    ")
  (should (equal '("qux")
                 (mapcar #'robe--variable-name
                         (robe-complete--local-variables "foo")))))

(ert-deftest complete-local-variables-skips-ones-at-and-after-point ()
  (insert "
class A
  def foo
    bar = 1; tee = 2; qux = 3
    ")
  (search-backward " = 2")
  (should (equal '("bar")
                 (mapcar #'robe--variable-name
                         (robe-complete--local-variables "foo")))))

(ert-deftest complete-local-variables-multiple-assignment ()
  (with-temp-buffer
    (insert "
class A
  def foo
    qux, tee = foobar(45)
    ")
    (ruby-mode)
    (should (equal '("tee" "qux")
                   (mapcar #'robe--variable-name
                           (robe-complete--local-variables "foo"))))))

(ert-deftest complete-local-variables-detect-types ()
  (with-temp-buffer
    (insert "
class A
  def foo
    bar = Array.new.first
    qux ||= 'abc'
    tee = Array.new
    ")
    (ruby-mode)
    (let ((vars (robe-complete--local-variables "foo")))
      (should (equal (robe--variable-name (nth 0 vars)) "tee"))
      (should (equal (robe--variable-type (nth 0 vars)) "Array"))
      (should (equal (robe--variable-name (nth 1 vars)) "qux"))
      (should (equal (robe--variable-type (nth 1 vars)) "String"))
      (should (equal (robe--variable-name (nth 2 vars)) "bar"))
      (should (null (robe--variable-type (nth 2 vars)))))))

(ert-deftest complete-rspec-bindings ()
  (with-temp-buffer
    (insert "RSpec.describe SomeClass do
  let(:foo) { [1, 2, 3] }
  let(:bar) { tee }
  let(:qux) { StringIO.new }

  it 'foo bar' do
    ")
    (ruby-mode)
    (let ((vars (robe-complete--rspec-bindings)))
      (should (equal (robe--variable-name (nth 0 vars)) "qux"))
      (should (equal (robe--variable-type (nth 0 vars)) "StringIO"))
      (should (equal (robe--variable-name (nth 1 vars)) "bar"))
      (should (null (robe--variable-type (nth 1 vars))))
      (should (equal (robe--variable-name (nth 2 vars)) "foo"))
      (should (equal (robe--variable-type (nth 2 vars)) "Array")))))

(ert-deftest complete-rspec-bindings-skips-over-nested-scopes ()
  (with-temp-buffer
    (insert "RSpec.describe SomeClass do
  let(:foo) { [1, 2, 3] }

  describe 'stuff' do
    let(:bar) { tee }
    let(:qux) { StringIO.new }
  end

  it 'foo bar' do
    ")
    (ruby-mode)
    (let ((vars (robe-complete--rspec-bindings)))
      (should (not (cdr vars)))
      (should (equal (robe--variable-name (nth 0 vars)) "foo")))))
