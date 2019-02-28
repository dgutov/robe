require 'robe/sash/const_locations'
require 'robe/visor'
require 'spec_helper'

describe Robe::Sash::ConstLocations do
  it "filters locations to include said module's definition" do
    require 'fixtures/sample_class'

    k = described_class.new(Robe::Visor.new)
    expect(k.all('SampleClass', nil)).to match([ending_with('fixtures/sample_class.rb')])
  end

  it "shows no location for class without methods" do
    k = described_class.new(Robe::Visor.new)
    expect(k.all('ArgumentError', nil)).to be_empty
  end
end
