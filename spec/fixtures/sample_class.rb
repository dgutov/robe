# frozen_string_literal: true

require 'fixtures/sample_module'

class SampleClass
  include SampleModule

  SILLY_CONSTANT = 3

  def foo; end
end
