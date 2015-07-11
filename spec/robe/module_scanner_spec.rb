require 'spec_helper'
require 'robe/scanners'

describe Robe::ModuleScanner do
  include ScannerHelper

  klass = described_class

  let(:a) { new_module(:foo, :bar, :baz, :tee) }
  let(:b) { new_module(:bar, :foo, :baz, :tee) }
  let(:c) { new_module(:foo, :bar, :tee, :baz) }
  let(:d) { new_module(:bar, :foo, :tee, :baz) }
  let(:e) { Module.new }
  let(:modules) { [a, b, c, d, e ] }

  it "finds instance methods" do
    scanner = klass.new(:foo, false)
    scanner.scan(modules, true, false)
    expect(scanner.candidates).to eq [a, c].map { |s| s.instance_method(:foo) }
  end

  it "finds module methods" do
    scanner = klass.new(:foo, false)
    scanner.scan(modules, false, true)
    expect(scanner.candidates).to eq [b, d].map { |s| s.method(:foo).unbind }
  end

  it "find private instance methods" do
    scanner = klass.new(:baz, true)
    scanner.scan(modules, true, false)
    expect(scanner.candidates).to eq [a, b].map { |s| s.instance_method(:baz) }
  end

  it "finds private module methods" do
    scanner = klass.new(:tee, true)
    scanner.scan(modules, false, true)
    expect(scanner.candidates).to eq [a, b].map { |s| s.method(:tee).unbind }
  end

  it "finds nothing when shouldn't" do
    scanner = klass.new(:foo, true)
    scanner.scan(modules, false, false)
    expect(scanner.candidates).to eq []
  end

  it "finds both types when should" do
    scanner = klass.new(:foo, false)
    scanner.scan(modules, true, true)
    expect(scanner.candidates).to eq([a.instance_method(:foo),
        b.method(:foo).unbind, c.instance_method(:foo), d.method(:foo).unbind])
  end

  it "doesn't include methods from Module in module methods" do
    scanner = klass.new(:attr, true)
    scanner.scan([Module.new], true, true)
    expect(scanner.candidates).to be_empty
  end

  it "doesn't pay attention to overridden [private_]instance_methods" do
    tiff = new_module(:foo, :bar, :bar, :foo)

    tiff = Module.new do
      def self.instance_methods(_ign)
        [:fool, :me, :once]
      end

      def self.private_instance_methods(_ign)
        [:fool, :me, :twice]
      end
    end

    scanner = klass.new(:fool, true)
    scanner.scan([tiff], true, true)
    expect(scanner.candidates).to be_empty
  end
end
