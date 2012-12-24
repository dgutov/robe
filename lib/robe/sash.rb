require 'robe/type_space'
require 'robe/scanners'
require 'robe/visor'

module Robe
  class Sash
    begin
      require 'robe/sash/pry_doc_info'
      include PryDocInfo
    rescue LoadError
      require 'robe/sash/pry_doc_fallback'
      include PryDocFallback
    end

    attr_reader :visor

    def initialize(visor = Visor.new)
      @visor = visor
    end

    def class_locations(name, mod)
      locations = {}
      if (obj = visor.resolve_context(name, mod)) and obj.is_a? Module
        methods = obj.methods(false).map { |m| obj.method(m) } +
          obj.instance_methods(false).map { |m| obj.instance_method(m) }
        methods.each do |m|
          if loc = m.source_location
            path = loc[0]
            locations[path] ||= 0
            locations[path] += 1
          end
        end
      end
      if defined? Class.class_attribute and Class != obj
        locations.delete Class.method(:class_attribute).source_location[0]
      end
      locations.keys.sort { |k1, k2| -(locations[k1] <=> locations[k2]) }
    end

    def modules
      visor.each_object(Module).map { |c| c.name }.tap { |ms| ms.compact! }
    end

    def targets(obj)
      obj = visor.resolve_const(obj)
      if obj.is_a? Module
        module_methods = obj.methods.map { |m| method_info(obj.method(m)) }
        instance_methods = (obj.instance_methods +
                            obj.private_instance_methods(false))
          .map { |m| method_info(obj.instance_method(m)) }
        [obj.name] + module_methods + instance_methods
      else
        self.targets(obj.class.to_s)
      end
    end

    def find_method(mod, type, sym)
      mod.send(type == :instance ? :instance_method : :method, sym)
    end

    def find_method_owner(mod, type, sym)
      begin
        find_method(mod, type, sym).owner
      rescue NameError
      end
    end

    def method_info(method)
      owner = method.owner
      if Class == owner || owner.ancestors.first == owner
        type = :instance
        name = method_owner_name(owner)
      else
        type = :module # defined in an eigenclass
        name = owner.to_s[/Class:(.*)>\Z/, 1]
      end
      [name, type, method.name] + method.source_location.to_a
    end

    def method_owner_name(owner)
      owner.name or
        unless owner.is_a?(Class)
          klass = ObjectSpace.each_object(Class).find { |c| c.include?(owner) }
          klass && klass.name
        end
    end

    def doc_for(mod, type, sym)
      mod = visor.resolve_const(mod)
      method = find_method(mod, type.to_sym, sym.to_sym)
      info = method_struct(method)
      {docstring: info.docstring,
       source: info.source,
       aliases: info.aliases,
       parameters: method.parameters}
    end

    def method_targets(name, target, mod, instance, superc, conservative)
      sym = name.to_sym
      space = TypeSpace.new(visor, target, mod, instance, superc)
      special_method = sym == :initialize || superc
      scanner = ModuleScanner.new(sym, special_method || !target)

      space.scan_with(scanner)

      if (targets = scanner.candidates).any?
        owner = find_method_owner(space.target_type, instance && :instance, sym)
        if owner
          targets.reject! do |method|
            !(method.owner <= owner) &&
              targets.find { |other| other.owner < method.owner }
          end
        end
      elsif (target || !conservative) && !special_method
        unless target
          scanner.scan_methods(Kernel, :private_instance_methods)
        end
        scanner.check_private = false
        scanner.scan(visor.each_object(Module), true, true)
      end

      scanner.candidates.map { |method| method_info(method) }
        .sort_by { |(mname)| mname ? mname.scan(/::/).length : 99 }
    end

    def complete_method(prefix, target, mod, instance)
      space = TypeSpace.new(visor, target, mod, instance, nil)
      scanner = MethodScanner.new(prefix, !target)

      space.scan_with(scanner)

      if scanner.candidates.empty?
        scanner.check_private = false
        scanner.scan(visor.each_object(Module), true, true)
      end

      scanner.candidates
    end

    def complete_const(prefix)
      colons = prefix.rindex("::")
      if !colons || colons == 0
        base, base_name = Object, ""
      else
        base = visor.resolve_const(prefix[0..colons - 1])
        base_name = base.name + "::"
      end
      tail = colons ? prefix[colons + 2..-1] : prefix
      base.constants.grep(/^#{Regexp.escape(tail)}/)
        .map { |c| "#{base_name}#{c}" }
    end

    def rails_refresh
      reload!
      Rails.application.eager_load!
    end

    def ping
      true
    end
  end
end
