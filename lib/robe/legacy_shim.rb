
if RUBY_VERSION[/^1.8/]

  require 'rubygems'
  require 'backports'

  RUBY_ENGINE = "ruby" unless defined?(RUBY_ENGINE)
  
  class Object
    def singleton_class
      class << self; self; end
    end

    def public_send(name, *args)
      unless public_methods.include?(name.to_s)
        raise NoMethodError.new("undefined method `#{name}' for \"#{self.inspect}\":#{self.class}")
      end
      send(name, *args)
    end
  end

  class Module
    def singleton_class
      class << self; self; end
    end
  end
end
