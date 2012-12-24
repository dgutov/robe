require 'spec_helper'
require 'robe/sash/pry_doc_info'

describe Robe::Sash::PryDocInfo do
  let(:o) { Object.new.extend(described_class) }

  context "String#gsub info fields" do
    let(:struct) { o.method_struct(String.instance_method(:gsub)) }
    it { expect(struct.docstring).to start_with("Returns")}
    it { expect(struct.source).to start_with("static VALUE") }
  end
end
