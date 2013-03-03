require 'forwardable'

module Robe
  class Visor
    extend Forwardable

    delegate each_object: ObjectSpace

    def resolve_context(name, mod)
      return resolve_const(mod) unless name
      unless name =~ /\A::/
        nesting = mod ? mod.split("::") : []
        resolve_path_elems(nesting).reverse.each do |elem|
          begin
            return elem.const_get(name)
          rescue NameError
          end
        end
      end
      resolve_const(name)
    end

    def resolve_const(name)
      resolve_path(name).last
    end

    def resolve_path(name)
      return [] unless name
      return [ARGF.class] if name == "ARGF.class"
      nesting = name.split("::")
      nesting.shift if nesting[0].empty?
      begin
        resolve_path_elems(nesting)
      rescue NameError
        []
      end
    end

    def resolve_path_elems(nesting, init = Object)
      c = init; ary = []
      nesting.each do |name|
        ary << (c = c.const_get(name))
      end
      ary
    end
  end
end
