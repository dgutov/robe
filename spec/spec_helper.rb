require 'pry'

RSpec.configure do |config|
  config.expect_with :rspec do |c|
    c.syntax = :expect
  end
end

module ScannerHelper
  def new_module(imethod, method, private_imethod, private_method)
    Module.new do
      eval <<-EOS
        def #{imethod}; end
        private
        def #{private_imethod}; end
        class << self
          def #{method}; end
          private
          def #{private_method}; end
        end
      EOS
    end
  end
end

class MockSpace
  def initialize(*modules)
    @modules = modules
  end

  def each_object(type)
    @modules.select { |m| m.is_a?(type) }.each { |m| yield m if block_given? }
  end
end
