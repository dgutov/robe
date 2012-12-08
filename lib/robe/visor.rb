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
        resolve_path_in(Object, nesting)
      rescue NameError
      end
    end

    def resolve_path_in(base, nesting)
      nesting.reduce(base, :const_get)
    end
  end
end
