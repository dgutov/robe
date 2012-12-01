module Robe
  class TypeSpace
    attr_reader :object_space, :target_type, :instance

    def initialize(object_space, target, mod, instance, superc)
      @object_space = object_space
      @target = target
      @mod = mod
      @instance = instance
      @superc = superc
      guess_target_type
    end

    def scan_with(scanner)
      return unless obj = target_type
      modules = obj.ancestors - [obj]
      modules -= obj.included_modules unless instance
      modules +=
        object_space.each_object(obj.singleton_class).to_a unless @superc

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

    def guess_target_type
      begin
        @target_type = Kitchen.resolve_context(@target, @mod)
        unless @target_type.is_a? Module
          @target_type, @instance = @target_type.class, true
        end
      end rescue nil
    end
  end
end
