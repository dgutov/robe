require 'spec_helper'
require 'support/mocks'
require 'robe/sash'
require 'robe/sash/pry_doc_fallback'

describe Robe::Sash do
  klass = described_class

  context "#modules" do
    it "returns module names" do
      mock_space = BlindVisor.new(*%w(A B C).map { |n| OpenStruct.new(name: n) })
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
            def name
              "M"
            end
            alias_method :inspect, :name
            def oom; end
            private
            def tee; end
          end
        end
      end
      let(:k) { klass.new(double(resolve_const: m)) }
      subject { k.targets(nil)[1..-1] }

      specify { expect(k.targets(nil)[0]).to eq("M") }

      it { should include(["M", :instance, :foo, __FILE__, anything]) }
      it { should include(["M", :instance, :baz, __FILE__, anything]) }
      it { should include(["M", :module, :oom, __FILE__, anything]) }
      it { expect(subject.select { |(_, _, m)| m == :tee }).to be_empty }
    end

    it "looks at the class if the value is not a module" do
      expect(klass.new.targets("Math::E")).to include(["Float", :instance, :ceil])
    end
  end

  context "#find_method" do
    let(:k) { klass.new }

    it { expect(k.find_method(String, :instance, :gsub).name).to eq(:gsub) }
    it { expect(k.find_method(String, :module, :freeze).name).to eq(:freeze) }
  end

  context "#find_method_owner" do
    let(:k) { klass.new }

    it { expect(k.find_method_owner(File, :module, :open)).to eq(IO.singleton_class)}
    it { expect(k.find_method_owner(String, :instance, :split)).to eq(String)}
    it { expect(k.find_method_owner(Class.new, :module, :boo)).to be_nil}
  end

  context "#method_info" do
    let(:k) { klass.new }

    it "works on String#gsub" do
      expect(k.method_info(String.instance_method(:gsub)))
        .to eq(["String", :instance, :gsub])
    end

    it "includes method location" do
      m = Module.new { def foo; end }
      expect(k.method_info(m.instance_method(:foo)))
        .to eq([nil, :instance, :foo, __FILE__, __LINE__ - 2])
    end

    it "substitutes eigenclass with the actual class name" do
      c = Class.new do
        class << self
          def foo; end
        end
      end
      stub_const("M::C", c)
      expect(k.method_info(c.singleton_class.instance_method(:foo)))
        .to eq(["M::C", :module, :foo, __FILE__, anything])
    end

    context "anonymous owner" do
      let(:m) { Module.new { def foo; end} }

      it "returns nil first element" do
        expect(k.method_info(m.instance_method(:foo)))
          .to eq([nil, :instance, :foo, __FILE__, anything])
      end

      it "substitutes anonymous module with including class name" do
        stub_const("C", Class.new.send(:include, m) )
        expect(k.method_info(m.instance_method(:foo)))
          .to eq(["C", :instance, :foo, __FILE__, anything])
      end
    end
  end

  context "#doc_for" do
    it "returns docstring, parameters and source" do
      c = Class.new do
        # Some words.
        def quux(a, *b, &c); end
      end
      v = ScopedVisor.new({"C" => c})
      k = klass.new(v).extend(Robe::Sash::PryDocFallback) # YARD chokes on specs
      expect(k.doc_for("C", "instance", "quux"))
        .to eq({docstring: "Some words.",
                parameters: [[:req, :a], [:rest, :b], [:block, :c]],
                source: "def quux(a, *b, &c); end\n",
                aliases: []})
    end

    it "shows docs for built-in classes" do
      k = klass.new(Robe::Visor.new)
      hash = k.doc_for("Hash", "instance", "update")
      expect(hash[:docstring]).to include("Adds the contents")
      expect(hash[:parameters]).to eq([[:req]])
      expect(hash[:source]).to include("rb_hash_foreach")
      expect(hash[:aliases]).to eq([:merge!])
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
          .to eq([["IO", :module, :open]])
      end

      it "returns the method on Class" do
        expect(k.method_targets("allocate", "Object", nil, nil, nil, nil))
          .to eq([["Class", :instance, :allocate]])
      end

      it "returns the non-overridden method" do
        expect(k.method_targets("initialize", "Object", nil, true, nil, nil))
          .to include(["BasicObject", :instance, :initialize])
      end

      it "doesn't return overridden method" do
        expect(k.method_targets("to_s", "Hash", nil, true, nil, nil))
          .to eq([["Hash", :instance, :to_s]])
      end

      context "unknown target" do
        it "returns String method candidate" do
          expect(k.method_targets("split", "s", nil, true, nil, nil))
            .to include(["String", :instance, :split])
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
          .to eq([["Enumerable", :instance, :map]])
      end

      it "returns single method from target class" do
        expect(k.method_targets("map", nil, "Array", true, nil, nil))
          .to eq([["Array", :instance, :map]])
      end

      it "checks for instance Kernel methods when the target is a module" do
        # Not 100% accurate: the including class may derive from BasicObject
        stub_const("M", Module.new)
        expect(k.method_targets("puts", nil, "M", true, nil, true))
          .to eq([["Kernel", :instance, :puts]])
      end

      it "checks private Kernel methods when no primary candidates" do
        k = klass.new(BlindVisor.new)
        expect(k.method_targets("puts", nil, nil, true, nil, nil))
          .to eq([["Kernel", :instance, :puts]])
      end

      it "sorts results list" do
        extend ScannerHelper

        a = named_module("A", "a", "b", "c", "d")
        b = named_module("A::B", "a", "b", "c", "d")
        c = new_module("a", "b", "c", "d")
        k = klass.new(ScopedVisor.new(*[b, c, a].shuffle))
        expect(k.method_targets("a", nil, nil, true, nil, nil).map { |(m)| m })
          .to eq(["A", "A::B", nil])
      end
    end
  end

  context "#complete_method" do
    let(:k) { klass.new }

    it "completes instance methods" do
      expect(k.complete_method("gs", nil, nil, true).map(&:first))
        .to include(:gsub, :gsub!)
    end

    context "class methods" do
      let(:k) { klass.new(ScopedVisor.new(Class, {"Object" => Object})) }

      it "completes public" do
        expect(k.complete_method("su", nil, nil, nil).map(&:first))
          .to include(:superclass)
      end

      it "no private methods with explicit target" do
        expect(k.complete_method("attr", "Object", nil, nil).map(&:first))
          .not_to include(:attr_reader)
      end

      it "no private methods with no target at all" do
        expect(k.complete_method("attr", "Object", nil, nil).map(&:first))
          .not_to include(:attr_reader)
      end

      it "completes private methods with implicit target" do
        expect(k.complete_method("attr", nil, "Object", nil).map(&:first))
          .to include(:attr_reader, :attr_writer)
      end
    end
  end

  context "#complete_const" do
    let(:m) do
      Module.new do
        def self.name
          "Test"
        end

        self::ACONST = 1

        module self::AMOD; end
        module self::BMOD
          def self.name
            "BMOD"
          end

          module self::C; end
        end

        class self::ACLS; end
      end
    end
    let(:v) { ScopedVisor.new({"Test" => m})}
    let(:k) { klass.new(v) }

    context "sandboxed" do
      it "completes all constants" do
        expect(k.complete_const("Test::A"))
          .to eq(%w(Test::ACONST Test::AMOD Test::ACLS))
      end

      it "requires names to begin with prefix" do
        expect(k.complete_const("Test::MOD")).to be_empty
      end
    end

    it "completes with bigger nesting" do
      expect(k.complete_const("Test::BMOD::C")).to eq(["BMOD::C"])
    end

    it "completes global constants" do
      expect(k.complete_const("Ob")).to include("Object", "ObjectSpace")
    end
  end

  it { expect(klass.new.ping).to be_true }
end
