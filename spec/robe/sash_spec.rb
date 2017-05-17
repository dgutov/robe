require 'spec_helper'
require 'support/mocks'
require 'robe/sash'

describe Robe::Sash do
  klass = described_class

  context "#modules" do
    it "returns module names" do
      mock_space = BlindVisor.new(*%w(A B C).map { |n| OpenStruct.new(__name__: n) })
      expect(klass.new(mock_space).modules).to eq %w(A B C)
    end
  end

  context "#class_locations" do
    it "shows location when class has methods" do
      k = klass.new(double(resolve_context: Class.new { def foo; end }))
      expect(k.class_locations(nil, nil)).to eq([__FILE__])
    end

    it "shows no location for class without methods" do
      k = klass.new(double(resolve_context: Class.new))
      expect(k.class_locations(nil, nil)).to be_empty
    end
  end

  context "#targets" do
    context "value is a module" do
      let(:m) do
        Module.new do
          def foo; end
          private
          def baz; end
          class << self
            alias_method :inspect, :name
            def oom; end
            private
            def tee; end
          end
        end
      end

      before { stub_const("M", m) }

      let(:k) { klass.new }

      subject { k.targets("M")[1..-1] }

      specify { expect(k.targets("M")[0]).to eq("M") }

      it { should include_spec("M#foo") }
      it { should include_spec("M#baz") }
      it { should include_spec("M.oom") }
      it { expect(subject.select { |(_, _, m)| m == :tee }).to be_empty }
    end

    it "looks at the class if the value is not a module" do
      expect(klass.new.targets("Math::E")).to include_spec("Float#ceil")
    end
  end

  context "#find_method" do
    let(:k) { klass.new }

    it { expect(k.find_method(String, true, :gsub).name).to eq(:gsub) }
    it { expect(k.find_method(String, nil, :freeze).name).to eq(:freeze) }
  end

  context "#find_method_owner" do
    let(:k) { klass.new }

    it { expect(k.find_method_owner(File, nil, :open)).to eq(IO.singleton_class)}
    it { expect(k.find_method_owner(String, true, :split)).to eq(String)}
    it { expect(k.find_method_owner(Class.new, nil, :boo)).to be_nil}
  end

  context "#method_spec" do
    let(:k) { klass.new }

    it "works on String#gsub" do
      match = if RUBY_ENGINE == "rbx"
                start_with("String", true, :gsub)
              else
                eq(["String", true, :gsub, [[:rest]]])
              end
      expect(k.method_spec(String.instance_method(:gsub))).to match
    end

    it "includes method location" do
      m = Module.new { def foo; end }
      expect(k.method_spec(m.instance_method(:foo))[4..5])
        .to eq([__FILE__, __LINE__ - 2])
    end

    it "includes method parameters" do
      m = Module.new { def foo(a, *b, &c); end }
      expect(k.method_spec(m.instance_method(:foo)))
        .to eq([nil, true, :foo, [[:req, :a], [:rest, :b], [:block, :c]],
                __FILE__, anything])
    end

    it "ignores overridden name method" do
      # Celluloid::Actor in celluloid <~ 0.15
      # https://github.com/celluloid/celluloid/issues/354

      m = Module.new do
        def self.name
          raise TooCoolForSchoolError
        end

        def self.__name__
          "baa"
        end

        def qux
        end
      end

      expect(k.method_spec(m.instance_method(:qux)))
        .to eq(["baa", true, :qux, [], __FILE__, anything])
    end

    context "eigenclass" do
      let(:c) do
        Class.new do
          class << self
            def foo; end
          end
        end
      end

      it "substitutes eigenclass with the actual class name" do
        stub_const("M::C", c)
        expect(k.method_spec(c.singleton_class.instance_method(:foo))[0])
          .to eq("M::C")
      end

      it "skips anonymous one" do
        expect(k.method_spec(c.singleton_class.instance_method(:foo))[0])
          .to be_nil
      end

      it "recognizes ActiveRecord classes" do
        arc = Class.new do
          class << self
            def bar; end
            def inspect
              "Record(id: integer)"
            end
          end
        end

        stub_const("Record", arc)

        expect(k.method_spec(arc.singleton_class.instance_method(:bar))[0]).to eq("Record")
      end
    end

    context "anonymous owner" do
      let(:m) { Module.new { def foo; end} }

      it "returns nil first element" do
        expect(k.method_spec(m.instance_method(:foo))[0]).to be_nil
      end

      it "substitutes anonymous module with including class name" do
        stub_const("C", Class.new.send(:include, m) )
        spec = k.method_spec(m.instance_method(:foo))
        expect(spec[0]).to eq("C")
        expect(spec[1]).to eq(true)
      end

      it "substitutes anonymous modules with extending module name" do
        stub_const("M", Module.new.send(:extend, m) )
        spec = k.method_spec(m.instance_method(:foo))
        expect(spec[0]).to eq("M")
        expect(spec[1]).to eq(nil)
      end
    end
  end

  context "#doc_for" do
    it "returns doc hash for instance method" do
      k = klass.new
      hash = k.doc_for("Set", true, "replace")
      expect(hash[:docstring]).to start_with("Replaces the contents")
    end

    it "returns doc hash for module method" do
      k = klass.new
      hash = k.doc_for("Set", nil, "[]")
      expect(hash[:docstring]).to start_with("Creates a new set containing")
    end
  end

  context "#method_targets" do
    it "returns empty array when not found" do
      k = klass.new(ScopedVisor.new)
      expect(k.method_targets("a", "b", "c", true, nil, nil)).to be_empty
    end

    context "examples" do
      let(:k) { klass.new }

      it "returns class method candidate" do
        expect(k.method_targets("open", "File", nil, nil, nil, nil))
          .to have_one_spec("IO.open")
      end

      it "returns the method on Class" do
        expect(k.method_targets("superclass", "Object", nil, nil, nil, nil))
          .to have_one_spec("Class#superclass")
      end

      it "returns the non-overridden method" do
        targets = k.method_targets("new", "Object", nil, nil, nil, nil)
        expect(targets).to include_spec("BasicObject#initialize")
        expect(targets).not_to include_spec("Class#new")
      end

      it "returns #new overridden in the given class" do
        c = Class.new do
          def self.new
          end
        end

        stub_const("C", c)

        targets = k.method_targets("new", "C", nil, nil, nil, nil)
        expect(targets).to include_spec("C.new")
        expect(targets).not_to include_spec("Class#new")
        expect(targets).not_to include_spec("BasicObject#initialize")
      end

      it "doesn't return overridden method" do
        expect(k.method_targets("to_s", "Hash", nil, true, nil, nil))
          .to have_one_spec("Hash#to_s")
      end

      context "unknown target" do
        it "returns String method candidate" do
          expect(k.method_targets("split", "s", nil, true, nil, nil))
            .to include_spec("String#split")
        end

        it "does not return wrong candidates" do
          candidates = k.method_targets("split", "s", nil, true, nil, nil)
          expect(candidates).to be_all { |c| c[2] == :split }
        end
      end

      it "returns no candidates for target when conservative" do
        expect(k.method_targets("split", nil, nil, true, nil, true))
          .to be_empty
      end

      it "returns single instance method from superclass" do
        expect(k.method_targets("map", nil, "Array", true, true, nil))
          .to have_one_spec("Enumerable#map")
      end

      it "returns single method from target class" do
        expect(k.method_targets("map", nil, "Array", true, nil, nil))
          .to have_one_spec("Array#map")
      end

      it "checks for instance Kernel methods when the target is a module" do
        # Not 100% accurate: the including class may derive from BasicObject
        stub_const("M", Module.new)
        expect(k.method_targets("puts", nil, "M", true, nil, true))
          .to have_one_spec("Kernel#puts")
      end

      it "checks private Kernel methods when no primary candidates" do
        k = klass.new(BlindVisor.new)
        expect(k.method_targets("puts", nil, nil, true, nil, nil))
          .to have_one_spec("Kernel#puts")
      end

      it "sorts results list" do
        extend ScannerHelper

        a = named_module("A", "a", "b", "c", "d")
        b = named_module("A::B", "a", "b", "c", "d")
        c = new_module("a", "b", "c", "d")
        k = klass.new(ScopedVisor.new(*[b, c, a].shuffle))
        expect(k.method_targets("a", nil, nil, true, nil, nil).map(&:first))
          .to eq(["A", "A::B", nil])
      end
    end
  end

  context "#complete_method" do
    let(:k) { klass.new }

    it "completes instance methods" do
      expect(k.complete_method("gs", nil, nil, true))
        .to include_spec("String#gsub", "String#gsub!")
    end

    context "class methods" do
      let(:k) { klass.new(ScopedVisor.new(Class, {"Object" => Object})) }

      it "completes public" do
        expect(k.complete_method("su", nil, nil, nil))
          .to include_spec("Class#superclass")
      end

      it "no private methods with explicit target" do
        expect(k.complete_method("attr", "Object", nil, nil))
          .not_to include_spec("Module#attr_reader")
      end

      it "no private methods with no target at all" do
        expect(k.complete_method("attr", "Object", nil, nil))
          .not_to include_spec("Module#attr_reader")
      end

      it "completes private methods with implicit target" do
        expect(k.complete_method("attr", nil, "Object", nil))
          .to include_spec("Module#attr_reader", "Module#attr_writer")
      end
    end
  end

  context "#complete_const" do
    let(:m) do
      Module.new do
        self::ACONST = 1

        module self::AMOD; end
        module self::BMOD
          module self::C; end
        end

        class self::ACLS; end
      end
    end
    let(:v) { ScopedVisor.new({"Test" => m})}
    let(:k) { klass.new(v) }

    context "sandboxed" do
      it "completes all constants" do
        expect(k.complete_const("Test::A", nil))
          .to match_array(%w(Test::ACONST Test::AMOD Test::ACLS))
      end

      it "requires names to begin with prefix" do
        expect(k.complete_const("Test::MOD", nil)).to be_empty
      end

      it "completes the constant in the nesting" do
        expect(k.complete_const("A", "Test")).to include("ACONST")
      end
    end

    it "completes with bigger nesting" do
      expect(k.complete_const("BMOD::C", "Test")).to eq(["BMOD::C"])
    end

    it "completes global constants" do
      expect(k.complete_const("Ob", nil)).to include("Object", "ObjectSpace")
    end

    it "completes the constants in all containing scopes" do
      k = klass.new
      expect(k.complete_const("C", "Encoding"))
        .to include("Converter", "Class", "Complex")
    end

    it "uses the full access path from the request" do
      k = klass.new
      expect(k.complete_const("Object::File::S", nil)).to include("Object::File::Stat")
    end

    it "keeps the global qualifier" do
      k = klass.new
      expect(k.complete_const("::Obj", nil)).to match_array(["::Object", "::ObjectSpace"])
    end
  end

  context "#load_path" do
    it 'returns an appropriate value' do
      expect(klass.new.load_path).to eq($:)
    end
  end

  it { expect(klass.new.ping).to be_true }

  RSpec::Matchers.define :include_spec do |*specs|
    match do |candidates|
      actual = candidates.map { |mod, instance, sym|
        MethodSpec.to_str(mod, instance, sym)
      }
      RSpec::Matchers::BuiltIn::Include.new(*specs).matches?(actual)
    end
  end

  RSpec::Matchers.define :have_one_spec do |spec|
    match do |candidates|
      candidates.length == 1 and
        MethodSpec.new(spec) == candidates[0]
    end
  end

  class MethodSpec
    def initialize(str)
      @str = str
    end

    def self.to_str(mod, inst, sym)
      "#{mod}#{inst ? '#' : '.'}#{sym}"
    end

    def ==(other)
      other.is_a?(Array) && other.length > 2 &&
        self.class.to_str(*other[0..2]) == @str
    end
  end
end
