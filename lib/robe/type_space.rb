require 'robe/sash'

module Robe
  class TypeSpace
    attr_reader :visor, :target_type, :instance

    def initialize(visor, target, mod, instance, superc)
      @visor = visor
      @superc = superc
      @target_type, @instance = visor.guess_target_type(target, mod, instance)
    end

    def scan_with(scanner)
      return unless obj = target_type
      modules = obj.ancestors - [obj]
      modules -= obj.included_modules unless instance
      modules +=
        visor.each_object(obj.singleton_class).to_a unless @superc

      if instance
        if defined? ActiveSupport::Concern and obj.is_a?(ActiveSupport::Concern)
          deps = obj.instance_variable_get("@_dependencies")
          modules += deps if deps
        end
      end

      scanner.scan(modules, instance, !instance)
      scanner.scan(obj.singleton_class.ancestors, true, false) unless instance
    end
  end
end
