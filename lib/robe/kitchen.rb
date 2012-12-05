require "robe/type_space"
require "robe/scanners"

module Robe
  class Kitchen
    begin
      require "pry"
      require "pry-doc"
      require "robe/kitchen/pry_doc_info"
      include PryDocInfo
    rescue LoadError
      require "robe/kitchen/pry_doc_fallback"
      include PryDocFallback
    end

    extend Forwardable

    delegate [:resolve_const, :resolve_context] => "self.class"

    attr_reader :object_space

    def initialize(object_space = ObjectSpace)
      @object_space = object_space
    end

    def class_locations(name, mod)
      locations = {}
      if (obj = resolve_context(name, mod)) and obj.is_a? Module
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
      object_space.each_object(Module).map { |c| c.name }.tap { |ms| ms.compact! }
    end

    def targets(obj)
      obj = resolve_const(obj)
      if obj.is_a? Module
        module_methods = obj.methods.map { |m| method_info(obj, :module, m) }
        instance_methods = (obj.instance_methods +
                            obj.private_instance_methods(false))
          .map { |m| method_info(obj, :instance, m) }
        # XXX: Filter out methods defined only in Object and Module?
        [obj.name] + module_methods + instance_methods
      else
        self.targets(obj.class.to_s)
      end
    end

    def find_method(mod, type, sym)
      mod.send(type == :instance ? :instance_method : :method, sym)
    end

    def method_info(mod, type, sym)
      # FIXME: For anonymous modules, substitute the name of the class.
      # As long as it's just for ActiveModel::AttributeMethods, not worth it.
      [mod.name, type, sym] + find_method(mod, type, sym).source_location.to_a
    end

    def doc_for(mod, type, sym)
      mod = resolve_const(mod)
      method = find_method(mod, type.to_sym, sym.to_sym)
      info = method_struct(method)
      {comment: info ? info.docstring : "",
       signature: signature(mod, type.to_sym, sym)}
    end

    def signature(mod, type, sym)
      sig = "#{mod.name}#{type == :instance ? '#' : '.'}#{sym}("
      dummy = "arg0"
      parameters = find_method(mod, type, sym).parameters
      parameters.each_with_index do |(kind, name), n|
        unless name
          case kind
          when :rest
            name = :args
          when :block
            name = :block
          else
            name = dummy.succ!
          end
        end
        if kind == :opt
          sig << "["
          sig << ", " if n > 0
          sig << name.to_s << "]"
        else
          sig << ", " if n > 0
          sig << {req: "%s", rest: "%s...", block: "&%s"}[kind] % name
        end
      end
      sig << ")"
    end

    def self.resolve_context(name, mod)
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

    def self.resolve_const(name)
      return nil unless name
      return ARGF.class if name == "ARGF.class"
      nesting = name.split("::")
      nesting.shift if nesting[0].empty?
      begin
        nesting.reduce(Object, :const_get)
      rescue NameError
      end
    end

    def method_targets(method, target, mod, instance, superc, conservative)
      sym = method.to_sym
      space = TypeSpace.new(object_space, target, mod, instance, superc)
      special_method = sym == :initialize || superc
      scanner = ModuleScanner.new(sym, special_method || !target)

      space.scan_with(scanner)

      if (targets = scanner.candidates).any?
        targets.reject! do |(m, _)|
          !(m <= space.target_type) && targets.find { |(t, _)| t < m }
        end
      elsif (target || !conservative) && !special_method
        unless target
          scanner.scan_methods(Object, :private_instance_methods, :instance)
        end
        scanner.check_private = false
        scanner.scan(object_space.each_object(Module), true, true)
      end

      scanner.candidates
        .sort_by { |(m)| m.name ? m.name.scan(/::/).length : 99 }
        .map { |(m, type)| method_info(m, type, sym) }
    end

    def complete_method(prefix, target, mod, instance)
      space = TypeSpace.new(object_space, target, mod, instance, nil)
      scanner = MethodScanner.new(prefix, !target)

      space.scan_with(scanner)

      if scanner.candidates.empty?
        scanner.check_private = false
        scanner.scan(object_space.each_object(Module), true, true)
      end

      scanner.candidates
    end

    def complete_const(prefix)
      colons = prefix.rindex("::")
      if !colons || colons == 0
        base, base_name = Object, ""
      else
        base = resolve_const(prefix[0..colons - 1])
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
