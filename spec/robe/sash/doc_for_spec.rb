require 'robe/sash/doc_for'
require 'uri'
require 'active_support/core_ext/kernel' # https://github.com/pry/pry-doc/issues/16

describe Robe::Sash::DocFor do
  klass = described_class

  it "returns hash for a basic class" do
    c = Class.new do
      # Some words.
      def quux(a, *b, &c); end
    end

    k = klass.new(c.instance_method(:quux))
    expect(k.format).to eq({docstring: "Some words.\n",
                            source: "def quux(a, *b, &c); end\n",
                            aliases: [],
                            visibility: :public})
  end

  it "shows docs for built-in classes" do
    hash = klass.new(Hash.instance_method(:update)).format
    expect(hash[:aliases]).to eq([:merge!])
    if RUBY_ENGINE == "ruby"
      expect(hash[:docstring]).to include("Adds the contents")
      expect(hash[:source]).to include("rb_hash_foreach")
    else
      expect(hash[:docstring]).to eq("")
      expect(hash[:source]).to start_with("def merge!(other)\n")
    end
  end

  it "shows docs for stdlib classes" do
    hash = klass.new(URI.method(:parse)).format
    expect(hash[:docstring]).to include("one of the URI's subclasses")
  end

  it "returns private visibility for Kernel#puts" do
    expect(klass.new(Kernel.instance_method(:puts)).format[:visibility])
      .to eq(:private)
  end

  it "returns protected visibility" do
    method = Class.new.class_exec do
      protected
      def foo; end
      instance_method(:foo)
    end
    expect(klass.new(method).format[:visibility]).to be(:protected)
  end

  it "mentions pry-doc when relevant" do
    val = Pry.config.has_pry_doc
    Pry.config.has_pry_doc = false
    struct = described_class.method_struct(String.instance_method(:gsub))
    Pry.config.has_pry_doc = val

    if RUBY_ENGINE == "ruby"
      expect(struct.source).to be_nil
      if RUBY_VERSION < "2.0.0"
        expect(struct.docstring).to match(/pry-doc/)
      else
        expect(struct.docstring).to eq("")
        pending("https://github.com/pry/pry/issues/956")
      end
    else
      expect(struct.docstring).to eq("")
      expect(struct.source).to start_with("def gsub(")
    end
  end

  context "native methods" do
    let(:c) { described_class }

    context "String#gsub info fields" do
      let(:struct) { c.method_struct(String.instance_method(:gsub)) }

      if RUBY_ENGINE == "ruby"
        it { expect(struct.docstring).to start_with("Returns")}
        it { expect(struct.source).to start_with("static VALUE") }
      else
        it { expect(struct.docstring).to eq("") }
        it { expect(struct.source).to start_with("def gsub(") }
      end
    end

    context "Array#map has an alias" do
      let(:struct) { c.method_struct(Array.instance_method(:map)) }
      it { expect(struct.aliases).to eq([:collect])}
    end

    context "Set#include? has an alias" do
      let(:struct) { c.method_struct(Set.instance_method(:include?)) }
      it { expect(struct.aliases).to eq([:member?])}
    end

    context "Know the appropriate amount about Kernel#is_a?" do
      let(:struct) { c.method_struct(Kernel.instance_method(:is_a?)) }

      it { expect(struct.visibility).to be_nil }

      if RUBY_ENGINE == "ruby"
        it { expect(struct.docstring).to be_empty }
        [:aliases, :source].each do |prop|
          it { expect(struct.send prop).to be_nil }
        end
      else
        it { expect(struct.docstring).to include("class or superclass") }
        it { expect(struct.aliases).to eq([:kind_of?]) }
        it { expect(struct.source).to start_with("def kind_of?(") }
      end
    end
  end

  context "pure methods" do
    let(:c) { described_class }

    context "method quux defined" do
      # First line,
      # second line.
      def quux(n)
      end

      it "has the docstring" do
        expect(c.method_struct(method(:quux)).docstring).to eq("First line,\n" +
                                                               "second line.\n")
      end

      it "has the source" do
        expect(c.method_struct(method(:quux)).source).to eq("def quux(n)\nend\n")
      end

      it "has no aliases" do
        expect(c.method_struct(method(:quux)).aliases).to eq([])
      end
    end

    it "should return the source for one-line methods" do
      def xuuq(); end
      expect(c.method_struct(method(:xuuq)).source).to eq("def xuuq(); end\n")
    end

    it "should return empty docstring when none" do
      def xuuq(m)
      end

      struct = c.method_struct(method(:xuuq))
      expect(struct.docstring).to eq("")
      expect(struct.source).not_to be_empty
    end
  end
end
