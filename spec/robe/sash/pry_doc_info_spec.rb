require 'spec_helper'
require 'robe/sash/pry_doc_info'

describe Robe::Sash::PryDocInfo do
  let(:o) { Object.new.extend(described_class) }

  context "String#gsub info fields" do
    let(:struct) { o.method_struct(String.instance_method(:gsub)) }
    it { expect(struct.docstring).to start_with("Returns")}
    it { expect(struct.source).to start_with("static VALUE") }
  end

  context "Array#map has an alias" do
    let(:struct) { o.method_struct(Array.instance_method(:map)) }
    it { expect(struct.aliases).to eq([:collect])}
  end

  context "Ignore non-overload tags" do
    let(:struct) { o.method_struct(Set.instance_method(:include?)) }
    it { expect(struct.aliases).to eq([])}
  end

  context "Know nothing about Kernel#is_a?" do
    let(:struct) { o.method_struct(Kernel.instance_method(:is_a?)) }
    it { expect(struct.docstring).to be_empty }
    [:aliases, :source, :visibility].each do |prop|
      it { expect(struct.send prop).to be_nil }
    end
  end
end
