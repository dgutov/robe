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
end
