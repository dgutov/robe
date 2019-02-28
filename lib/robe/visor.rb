require 'forwardable'
require 'robe/core_ext'

module Robe
  class Visor
    extend Forwardable

    def each_object(*args)
      ObjectSpace.each_object(*args).reject { |m| m.__singleton_class__? }
    end

    # Returns descendants and (modules only) also includers.
    def descendants(cls)
      ObjectSpace.each_object(Class).select { |c| c < cls }
    end

    def resolve_context(name, mod)
      resolve_context_path(name, mod).last
    end

    def resolve_context_path(name, mod)
      return resolve_path(mod) unless name

      unless name =~ /\A::/
        nesting = mod ? mod.split("::") : []

        resolve_path_elems(nesting, Object).reverse.each do |elem|
          path = resolve_path(name, elem)
          return path if path.any?
        end
      end

      resolve_path(name)
    end

    def resolve_const(name)
      resolve_path(name).last
    end

    def resolve_path(name, init = Object)
      return [] unless name
      return [ARGF.class] if name == "ARGF.class"
      if %w(IO::readable IO::writable).include?(name)
        return [StringIO.included_modules.find { |m| m.name == name }]
      end
      nesting = name.split("::")
      nesting.shift if nesting[0].empty?
      resolve_path_elems(nesting, init)
    end

    def resolve_path_elems(nesting, init)
      c = init; ary = []

      begin
        nesting.each do |name|
          ary << (c = c.const_get(name))
        end

        ary
      rescue NameError
        []
      end
    end
  end
end
