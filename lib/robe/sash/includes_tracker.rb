require 'weakref'
require 'set'
require 'robe/core_ext'

module Robe
  class Sash
    class IncludesTracker
      def self.method_owner_and_inst(owner)
        includers = maybe_scan

        mod, inst = includers[owner].first

        if mod
          [mod.__name__, inst]
        else
          [nil, true]
        end
      end

      def self.reset!
        @@hosts = nil
      end

      private

      def self.maybe_scan
        includers = @@hosts

        unless includers
          @@hosts = includers = Hash.new { |h, k| h[k] = Set.new }

          ObjectSpace.each_object(Module) do |cl|
            cl.included_modules.each { |mod| includers[mod] << [cl, true] }
            sc = cl.__singleton_class__
            sc.included_modules.each { |mod| includers[mod] << [cl, nil] }
          end
        end

        includers
      end

      if Module.respond_to?(:prepend)
        @@invalidator = Module.new do
          define_method(:include) do |*others|
            IncludesTracker.reset!
            super(*others)
          end

          define_method(:extend) do |others|
            IncludesTracker.reset!
            super(*others)
          end
        end

        Module.send(:prepend, @@invalidator)
      else
        Module.class_eval do
          alias_method :__orig_include, :include
          alias_method :__orig_extend, :extend

          # Cannot hook into this method without :prepend.
          def include(*others)
            IncludesTracker.reset!
            __orig_include(*others)
          end

          def extend(*others)
            IncludesTracker.reset!
            __orig_extend(*others)
          end
        end
      end
    end
  end
end
