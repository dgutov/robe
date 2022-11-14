require 'robe/sash/const_locations'
require 'robe/visor'
require 'spec_helper'

describe Robe::Sash::ConstLocations do
  it "filters locations to include said module's definition" do
    require 'fixtures/sample_class'

    k = described_class.new(Robe::Visor.new)
    expect(k.all('SampleClass', nil)[:files]).to match([ending_with('fixtures/sample_class.rb')])
  end

  it 'shows location for class with only private methods' do
    require 'fixtures/sample_private_class'
    require 'fixtures/sample_module'

    k = described_class.new(Robe::Visor.new)
    expect(k.all('SamplePrivateClass', nil)[:files]).to match([ending_with('fixtures/sample_private_class.rb')])
  end

  it 'shows location for class without methods' do
    require 'fixtures/sample_methodless_class'
    require 'fixtures/sample_methodless_subclass'

    k = described_class.new(Robe::Visor.new)
    expect(k.all('SampleMethodlessClass', nil)[:files]).to match([ending_with('/sample_methodless_class.rb')])
  end

  it 'replaces value constants with parent modules' do
    require 'fixtures/sample_class'

    k = described_class.new(Robe::Visor.new)
    expect(k.all('SampleClass::SILLY_CONSTANT', nil)[:files])
      .to match([ending_with('fixtures/sample_class.rb')])
  end

  it 'no location for non-existing constant' do
    require 'fixtures/sample_class'

    k = described_class.new(Robe::Visor.new)
    expect(k.all('SampleClass::BUZZ_BUZZ', nil)[:files]).to eq([])
  end
end
