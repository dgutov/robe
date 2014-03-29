require 'pry'

begin
  require 'simplecov'
  SimpleCov.start if RSpec.configuration.files_to_run.size > 1
rescue LoadError
  puts "simplecov not loaded"
end

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

  def named_module(name, *args)
    new_module(*args).tap do |m|
      m.instance_eval "def __name__; \"#{name}\" end"
    end
  end
end
