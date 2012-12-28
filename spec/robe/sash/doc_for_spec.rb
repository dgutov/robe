require 'robe/sash/doc_for'
require 'robe/sash/pry_doc_fallback'

describe Robe::Sash::DocFor do
  klass = described_class

  it "returns hash for a basic class" do
    c = Class.new do
      # Some words.
      def quux(a, *b, &c); end
    end
    # YARD chokes on specs
    k = klass.new(c.instance_method(:quux)).extend(Robe::Sash::PryDocFallback)
    expect(k.format).to eq({docstring: "Some words.",
                            source: "def quux(a, *b, &c); end\n",
                            aliases: [],
                            visibility: :public})
  end

  it "shows docs for built-in classes" do
    hash = klass.new(Hash.instance_method(:update)).format
    expect(hash[:docstring]).to include("Adds the contents")
    expect(hash[:source]).to include("rb_hash_foreach")
    expect(hash[:aliases]).to eq([:merge!])
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
end
