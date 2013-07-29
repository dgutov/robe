require 'robe/sash'

module Robe
  class TypeSpace
    attr_reader :visor, :target_type, :instance

    def initialize(visor, target, mod, instance, superc)
      @visor = visor
      @instance = instance
      @superc = superc
      guess_target_type(target, mod)
    end

    def scan_with(scanner)
      return unless obj = target_type
      modules = obj.ancestors
      modules -= obj.included_modules unless instance

      if @superc
        modules -= [obj]
      else
        modules += visor.descendants(obj).to_a
      end

      modules.push(Kernel) if instance && !obj.is_a?(Class)

      if instance
        if defined? ActiveSupport::Concern and obj.is_a?(ActiveSupport::Concern)
          deps = obj.instance_variable_get("@_dependencies")
          modules += deps if deps
        end
      end

      scanner.scan(modules, instance, !instance)
      scanner.scan(obj.singleton_class.ancestors, true, false) unless instance
    end

    private

    def guess_target_type(target, mod)
      @target_type = visor.resolve_context(target, mod)
      if @target_type && !(@target_type.is_a? Module)
        @target_type, @instance = @target_type.class, true
      end
    end
  end
end
