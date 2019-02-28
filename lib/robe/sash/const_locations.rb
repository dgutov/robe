require 'robe/core_ext'

module Robe
  class Sash
    class ConstLocations
      attr_reader :visor

      def initialize(visor)
        @visor = visor
      end

      def all(name, mod)
        locations = {}

        if (obj = visor.resolve_context(name, mod)) and obj.is_a?(Module)
          methods = obj.methods(false).map { |m| obj.method(m) } +
                    obj.__instance_methods__(false).map { |m| obj.instance_method(m) }

          methods.each do |m|
            if (loc = m.source_location)
              path = loc[0]
              locations[path] ||= 0
              locations[path] += 1
            end
          end
        end

        if defined?(Class.class_attribute) && Class != obj
          locations.delete(Class.method(:class_attribute).source_location[0])
        end

        filter_locations_by_module(
          locations.keys.sort { |k1, k2| -(locations[k1] <=> locations[k2]) },
          obj
        )
      end

      private

      def filter_locations_by_module(files, obj)
        return files if obj.nil?
        obj_local_name = obj.name[/(\A|::)([^:]+)\z/, 2]
        re = /^[ \t]*(class|module) *(.*?::)?#{obj_local_name}\b/
        filtered = files.select { |file| File.read(file).match(re) }
        return files unless filtered.any?
        filtered
      end
    end
  end
end
