require 'spec_helper'
require 'robe/kitchen/pry_doc_fallback'

describe Robe::Kitchen::PryDocFallback do
  # Misleading comment.
  let(:o) { Object.new.extend(described_class) }

  it "should return docstring" do
    # First line,
    # second line.
    def quux(n)
      42
    end

    expect(o.method_struct(method(:quux)).docstring).to eq("First line,\n" +
                                                           "second line.")
  end

  it "should return empty docstring when none" do
    def xuuq(m)
      24
    end

    expect(o.method_struct(method(:xuuq)).docstring).to eq("")
  end
end
