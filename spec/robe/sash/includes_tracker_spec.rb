require 'spec_helper'
require 'robe/sash/includes_tracker'

describe Robe::Sash::IncludesTracker do
  klass = described_class

  let(:m) { Module.new { def foo; end } }

  context "after change in environment" do
    before do
      expect(klass.method_owner_and_inst(m)).to eq([nil, true])
    end

    it "detects an included module" do
      n = Module.new
      stub_const("N", n)
      n.send(:include, m)
      expect(klass.method_owner_and_inst(m)).to eq(["N", true])
    end

    it "detects an extended module" do
      n = Module.new
      stub_const("N", n)
      n.send(:extend, m)
      expect(klass.method_owner_and_inst(m)).to eq(["N", nil])
    end
  end
end
