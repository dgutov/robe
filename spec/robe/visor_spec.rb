require 'spec_helper'
require 'robe/visor'

describe Robe::Visor do
  let(:v) { described_class.new }

  context ".resolve_const" do
    it "resolves ARGF.class" do
      expect(v.resolve_const("ARGF.class")).to be(ARGF.class)
    end

    it "silently fails on nil" do
      expect(v.resolve_const(nil)).to be_nil
    end

    it "resolves simple constant" do
      expect(v.resolve_const("Object")).to be(Object)
    end

    it "resolves explicit global scope" do
      expect(v.resolve_const("::String")).to be(String)
    end

    it "resolves nested class" do
      expect(v.resolve_const("File::Stat")).to be(File::Stat)
    end

    it "swallows NameError" do
      expect(v.resolve_const("Foo::Bar")).to be_nil
    end
  end

  context ".resolve_context" do
    it "defaults to the module if no name" do
      expect(v.resolve_context(nil, "File")).to be(File)
    end

    it "accepts explicit scope qualifier" do
      expect(v.resolve_context("::String", "File")).to be(String)
    end

    it "sees nested constants" do
      expect(v.resolve_context("Stat", "File")).to be(File::Stat)
    end

    it "sees constants in containing scopes" do
      expect(v.resolve_context("Stat", "File::Constants")).to be(File::Stat)
    end
  end

  context "#guess_target_type" do
    it "resolves simple class" do
      expect(v.guess_target_type("String", nil, true)).to eq [String, true]
    end

    it "resolves nested class" do
      expect(v.guess_target_type("Stat", "File", nil)).to eq [File::Stat, nil]
    end

    it "resolves constant to its type" do
      expect(v.guess_target_type("E", "Math", nil)).to eq [Float, true]
    end
  end
end
