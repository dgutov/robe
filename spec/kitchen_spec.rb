require 'spec_helper'
require 'robe/kitchen'

describe Robe::Kitchen do
  klass = described_class

  context ".resolve_const" do
    it "resolves ARGF.class" do
      expect(klass.resolve_const("ARGF.class")).to be(ARGF.class)
    end

    it "silently fails on nil" do
      expect(klass.resolve_const(nil)).to be_nil
    end

    it "resolves simple constant" do
      expect(klass.resolve_const("Object")).to be(Object)
    end

    it "resolves explicit global scope" do
      expect(klass.resolve_const("::String")).to be(String)
    end

    it "resolves nested class" do
      expect(klass.resolve_const("File::Stat")).to be(File::Stat)
    end

    it "swallows NameError" do
      expect(klass.resolve_const("Foo::Bar")).to be_nil
    end
  end

  context ".resolve_context" do
    it "defaults to the module if no name" do
      expect(klass.resolve_context(nil, "File")).to be(File)
    end

    it "accepts explicit scope qualifier" do
      expect(klass.resolve_context("::String", "File")).to be(String)
    end

    it "sees nested constants" do
      expect(klass.resolve_context("Stat", "File")).to be(File::Stat)
    end

    it "sees constants in containing scopes" do
      expect(klass.resolve_context("Stat", "File::Constants")).to be(File::Stat)
    end
  end

  context "#modules" do
    it "returns module names" do
      mock_space = MockSpace.new(*%w(A B C).map { |n| OpenStruct.new(name: n) })
      expect(klass.new(mock_space).modules).to eq %w(A B C)
    end
  end

  context "#class_locations" do
    it "shows location when class has methods" do
      k = klass.new
      k.stub(:resolve_context).and_return(Class.new { def foo; end })
      expect(k.class_locations(nil, nil)).to eq([__FILE__])
    end

    it "shows no location for class without methods" do
      k = klass.new
      k.stub(:resolve_context).and_return(Class.new)
      expect(k.class_locations(nil, nil)).to be_empty
    end
  end

  context "#targets" do
    let(:m) do
      Module.new do
        def foo; end
        private
        def baz; end
        class << self
          def oom; end
          private
          def tee; end
        end
      end.tap { |m| m.stub(:name).and_return("M") }
    end
    let(:k) { klass.new.tap { |k| k.stub(:resolve_const).and_return(m) } }
    subject { k.targets(nil)[1..-1] }

    specify { expect(k.targets(nil)[0]).to eq("M") }

    it { should include(["M", :instance, :foo, __FILE__, __LINE__ - 15]) }
    it { should include(["M", :instance, :baz, __FILE__, __LINE__ - 14]) }
    it { should include(["M", :module, :oom, __FILE__, __LINE__ - 13]) }
    it { expect(subject.select { |(_, _, m)| m == :tee }).to be_empty }
  end

  context "#find_method" do
    let(:k) { klass.new }

    it { expect(k.find_method(String, :instance, :gsub).name).to eq(:gsub) }
    it { expect(k.find_method(String, :module, :freeze).name).to eq(:freeze) }
  end

  context "#method_info" do
    let(:k) { klass.new }

    it { expect(k.method_info(String, :instance, :gsub))
         .to eq(["String", :instance, :gsub]) }

    it "includes method location" do
      m = Module.new { def foo; end }
      expect(k.method_info(m, :instance, :foo))
        .to eq([nil, :instance, :foo, __FILE__, __LINE__ - 2])
    end
  end

  context "#method_targets" do
    it "returns empty array when not found" do
      k = klass.new(MockSpace.new)
      klass.should_receive(:resolve_context).with("b", "c").and_return(nil)
      expect(k.method_targets("a", "b", "c", true, nil, nil)).to be_empty
    end

    context "examples" do
      let(:k) { klass.new }

      it "returns class method candidate" do
        expect(k.method_targets("open", "File", nil, nil, nil, nil))
          .to eq([["IO", :module, :open]])
      end

      it "returns the constructor" do
        expect(k.method_targets("initialize", "Object", nil, nil, nil, nil))
          .to include(["Class", :module, :initialize])
      end

      it "doesn't return overridden method" do
        expect(k.method_targets("to_s", "Hash", nil, true, nil, nil))
          .to eq([["Hash", :instance, :to_s]])
      end

      it "returns instance method candidate" do
        expect(k.method_targets("split", "s", nil, true, nil, nil))
          .to include(["String", :instance, :split])
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

      it "checks private Kernel methods when no primary candidates" do
        k = klass.new(MockSpace.new)
        expect(k.method_targets("puts", nil, nil, true, nil, nil))
          .to eq([["Kernel", :instance, :puts]])
      end

      it "sorts results last" do
        extend ScannerHelper

        a = named_module("A", "a", "b", "c", "d")
        b = named_module("A::B", "a", "b", "c", "d")
        c = new_module("a", "b", "c", "d")
        d = named_module("B", "a", "b", "c", "d")
        ospace = MockSpace.new(b, c, d, a)
        expect(k.method_targets("a", nil, nil, true, nil, nil).map { |(m)| m })
          .to eq(["B", "A", "A::B", nil])
      end
    end
  end
end
