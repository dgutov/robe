require 'spec_helper'
require 'robe/scanners'

describe Robe::MethodScanner do
  include ScannerHelper

  klass = described_class

  let(:a) { new_module(:f, :b, :b, :t) }
  let(:b) { new_module(:bar, :foo, :baz, :tee) }
  let(:c) { new_module(:foo, :bar, :baz, :tee) }
  let(:d) { new_module(:foo, :bar, :tee, :baz) }
  let(:modules) { [a, b, c, d] }

  it "completes instance methods" do
    scanner = klass.new(:f, false)
    scanner.scan(modules, true, false)
    expect(scanner.candidates).to eq [:f, :foo, :foo]
  end

  it "completes module methods" do
    scanner = klass.new(:b, false)
    scanner.scan(modules, false, true)
    expect(scanner.candidates).to eq [:b, :bar, :bar]
  end

  it "completes private instance methods" do
    scanner = klass.new(:baz, true)
    scanner.scan(modules, true, false)
    expect(scanner.candidates).to eq [:baz, :baz]
  end

  it "doesn't complete private module methods" do
    scanner = klass.new(:tee, true)
    scanner.scan(modules, false, true)
    expect(scanner.candidates).to be_empty
  end

  it "completes nothing when shouldn't" do
    scanner  = klass.new(:foo, true)
    scanner.scan(modules, false, false)
    expect(scanner.candidates).to be_empty
  end
end
