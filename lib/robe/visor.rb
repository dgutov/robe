require 'forwardable'

module Robe
  class Visor
    extend Forwardable

    delegate each_object: ObjectSpace

    def resolve_context(name, mod)
      return resolve_const(mod) unless name
      unless name =~ /\A::/
        nesting = mod ? mod.split("::") : []
        while nesting.any?
          if obj = resolve_const((nesting + [name]).join("::"))
            return obj
          else
            nesting.pop
          end
        end
      end
      resolve_const(name)
    end

    def resolve_const(name)
      return nil unless name
      return ARGF.class if name == "ARGF.class"
      nesting = name.split("::")
      nesting.shift if nesting[0].empty?
      begin
        nesting.reduce(Object, :const_get)
      rescue NameError
      end
    end

    def guess_target_type(target, mod, instance)
      target_type = resolve_context(target, mod)
      if target_type && !(target_type.is_a? Module)
        target_type, instance = target_type.class, true
      end
      [target_type, instance]
    end
  end
end
