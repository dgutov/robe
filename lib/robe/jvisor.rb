require 'robe/visor'

module Robe
  class JVisor < Visor
    def each_object(mod)
      # JRuby has weird Module's (Java package objects, etc)
      ObjectSpace.each_object(mod).select { |m| m.respond_to? :instance_methods }
    end
  end
end
