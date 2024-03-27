# frozen_string_literal: true

require 'robe/core_ext'

module Robe
  class Sash
    class IncludesTracker
      class << self
        attr_accessor :vm_stats
        attr_accessor :hosts

        def method_owner_and_inst(owner, name_cache)
          rescan_maybe

          mod, inst = hosts[owner].first

          return [nil, true] unless mod

          [name_cache[mod], inst]
        end

        def rescan_maybe
          return hosts if vm_stats == current_vm_stats

          self.hosts = Hash.new { |h, k| h[k] = [] }
          self.vm_stats = current_vm_stats

          ObjectSpace.each_object(Module) do |mod|
            next unless mod.respond_to?(:included_modules)
            next if mod.singleton_class?

            mod.included_modules.each { |imod| hosts[imod] << [mod, true] }
            sc = mod.__singleton_class__
            sc.included_modules.each { |imod| hosts[imod] << [mod, nil] }
          end

          hosts
        end

        def current_vm_stats
          if RUBY_ENGINE == 'jruby'
            JRuby::Util.cache_stats
          else
            RubyVM.stat
          end
        end
      end
    end
  end
end
