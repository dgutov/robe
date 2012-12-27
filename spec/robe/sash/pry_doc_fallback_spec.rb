require 'spec_helper'
require 'robe/sash/pry_doc_fallback'

describe Robe::Sash::PryDocFallback do
  # Misleading comment.
  let(:o) { Object.new.extend(described_class) }

  context "method quux defined" do
    # First line,
    # second line.
    def quux(n)
    end

    it "has the docstring" do
      expect(o.method_struct(method(:quux)).docstring).to eq("First line,\n" +
                                                             "second line.")
    end

    it "has the source" do
      expect(o.method_struct(method(:quux)).source).to eq("def quux(n)\nend\n")
    end

    it "has no aliases" do
      expect(o.method_struct(method(:quux)).aliases).to be_nil
    end
  end

  it "should return the source for one-line methods" do
    def xuuq(); end
    expect(o.method_struct(method(:xuuq)).source).to eq("def xuuq(); end\n")
  end

  it "should return empty docstring when none" do
    def xuuq(m)
    end

    struct = o.method_struct(method(:xuuq))
    expect(struct.docstring).to eq("")
    expect(struct.source).not_to be_empty
  end

  it "should mention pry-doc when method has no location" do
    struct = o.method_struct(String.instance_method(:gsub))
    expect(struct.docstring).to match(/pry-doc/)
    expect(struct.source).to be_nil
  end
end
