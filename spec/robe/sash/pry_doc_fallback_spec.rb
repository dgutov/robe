require 'spec_helper'
require 'robe/sash/pry_doc_fallback'

describe Robe::Sash::PryDocFallback do
  # Misleading comment.
  let(:o) { Object.new.extend(described_class) }

  it "should return docstring" do
    # First line,
    # second line.
    def quux(n)
    end

    expect(o.method_struct(method(:quux)).docstring).to eq("First line,\n" +
                                                           "second line.")
  end

  it "should return empty docstring when none" do
    def xuuq(m)
    end

    expect(o.method_struct(method(:xuuq)).docstring).to eq("")
  end

  it "should mention pry-doc when method has no location" do
    expect(o.method_struct(String.instance_method(:gsub)).docstring)
      .to match(/pry-doc/)
  end
end
