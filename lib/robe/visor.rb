# frozen_string_literal: true

require 'forwardable'
require 'robe/core_ext'

module Robe
  class Visor
    extend Forwardable

    SearchError = Class.new(StandardError)

    def each_object(*args)
      ObjectSpace.each_object(*args).reject(&:singleton_class?)
    end

    # Returns descendants and (modules only) also includers.
    def descendants(cls)
      ObjectSpace.each_object(Class).select { |c| c < cls }
    end

    def resolve_context(name, mod)
      return resolve_const(mod) unless name

      unless name =~ /\A::/
        nesting = mod ? mod.split('::') : []
        resolve_path_elems(nesting, true).reverse.each do |elem|
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
      return [ARGF.class] if name == 'ARGF.class'
      if %w[IO::readable IO::writable].include?(name)
        return [StringIO.included_modules.find { |m| m.name == name }]
      end

      nesting = name.split('::')
      nesting.shift if nesting[0].empty?
      resolve_path_elems(nesting)
    end

    def resolve_path_elems(nesting, lax = false, init = Object)
      c = init
      ary = []

      begin
        nesting.each do |name|
          ary << (c = c.const_get(name))
        end

        ary
      rescue NameError
        lax ? ary : raise(SearchError)
      end
    end
  end
end
