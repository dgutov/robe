# frozen_string_literal: true

module SampleModule
  def self.included(other)
    other.module_eval do
      def bar; end
    end
  end

  class SamplePrivateClass
  end
end
