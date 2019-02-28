require 'robe/sash/const_locations'
require 'spec_helper'

describe Robe::Sash::ConstLocations do
  it "filters locations to include said module's definition" do
    require 'fixtures/sample_class'

    k = described_class.new(double(resolve_context: SampleClass))
    expect(k.all(nil, nil)).to match([ending_with('fixtures/sample_class.rb')])
  end
end
