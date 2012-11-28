require "webrick"
require "json"

module Zossima
  class Handler < WEBrick::HTTPServlet::AbstractServlet
    def do_GET(req, res)
      _, endpoint, *args = req.path.split("/").map {|s| s == "_" ? nil : s }
      value = Zossima.send(endpoint.to_sym, *args)
      res["Content-Type"] = "application/json"
      res.status = 200
      res.body = value.to_json
      raise WEBrick::HTTPStatus::OK
    end
  end

  def self.class_locations(name, mod)
    locations = {}
    if (obj = resolve_context(name, mod) rescue nil) and obj.is_a? Module
      methods = obj.methods(false).map{|m|obj.method(m)} +
        obj.instance_methods(false).map{|m|obj.instance_method(m)}
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
    locations.keys.sort {|k1, k2| -(locations[k1] <=> locations[k2])}
  end

  def self.modules
    ObjectSpace.each_object(Module).map{|c| c.name }.compact!
  end

  def self.targets(obj)
    obj = resolve_const(obj)
    if obj.is_a? Module
      module_methods = obj.methods.map{|m| method_info(obj, :module, m)}
      instance_methods = (obj.instance_methods +
                          obj.private_instance_methods(false))
        .map{|m| method_info(obj, :instance, m)}
      # XXX: Filter out methods defined only in Object and Module?
      [obj.name] + module_methods + instance_methods
    else
      self.targets(obj.class.to_s)
    end
  end

  def self.find_method(mod, type, sym)
    mod.send(type == :instance ? :instance_method : :method, sym)
  end

  def self.method_info(mod, type, sym)
    # FIXME: For anonymous modules, substitute the name of the class.
    # But as long as it's just for ActiveModel::AttributeMethods, not worth it.
    [mod.name, type, sym] + find_method(mod, type, sym).source_location.to_a
  end

  def self.doc_for(mod, type, sym)
    mod = resolve_const(mod)
    method = find_method(mod, type.to_sym, sym.to_sym)
    begin
      require "pry"
      require "pry-doc"
      YARD::Registry.send :thread_local_store=, Thread.main[:__yard_registry__]
      ym = Pry::MethodInfo.info_for(method)
      comment = ym ? ym.docstring : ""
    rescue LoadError
      if method.source_location
        buf = []
        loc, n = method.source_location
        File.open(loc) do |f|
          f.each_line.with_index do |line, index|
            break if index == n - 1
            case line
            when /\A[ \t]*#( (?<text>.*))?/
              buf << $~[:text]
            when /\A[ \t]*[^#]/
              buf.clear
            end
          end
        end
        comment = buf.join("\n")
      else
        comment = "Docstring not found. For core classes, try installing 'pry-doc'."
      end
    end
    {comment: comment,
     signature: signature(mod, type.to_sym, sym)}
  end

  def self.signature(mod, type, sym)
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
        if obj = resolve_const((nesting + [name]).join("::")) rescue nil
          return obj
        else
          nesting.pop
        end
      end
    end
    resolve_const(name)
  end

  def self.resolve_const(name)
    return ARGF.class if name == "ARGF.class"
    nesting = name.split("::")
    nesting.shift if nesting[0].empty?
    nesting.reduce(Object, :const_get)
  end

  def self.method_targets(method, target, mod, instance, superc, conservative)
    sym = method.to_sym
    space = TypeSpace.new(target, mod, instance, superc)
    special_method = sym == :initialize || superc
    scanner = ModuleScanner.new(sym, special_method || !target)

    space.scan_with(scanner)

    if (targets = scanner.candidates).any?
      targets.reject! do |(m, _)|
        !(m <= space.target_type) && targets.find {|(t, _)| t < m}
      end
    elsif (target || !conservative) && !special_method
      scanner.check_private = false
      scanner.scan(ObjectSpace.each_object(Module), true, true)
    end

    scanner.candidates.map {|(m, type)| method_info(m, type, sym)}
  end

  def self.complete_method(prefix, target, mod, instance)
    space = TypeSpace.new(target, mod, instance, nil)
    scanner = MethodScanner.new(prefix, !target)

    space.scan_with(scanner)

    if scanner.candidates.empty?
      scanner.check_private = false
      scanner.scan(ObjectSpace.each_object(Module), true, true)
    end

    scanner.candidates
  end

  def self.complete_const(prefix)
    colons = prefix.rindex("::")
    if !colons || colons == 0
      base, base_name = Object, ""
    else
      base = resolve_const(prefix[0..colons - 1])
      base_name = base.name + "::"
    end
    tail = colons ? prefix[colons + 2..-1] : prefix
    base.constants.grep(/^#{Regexp.escape(tail)}/).map {|c| "#{base_name}#{c}"}
  end

  def self.rails_refresh
    reload!
    Rails.application.eager_load!
  end

  def self.start(port)
    @server ||= WEBrick::HTTPServer.new({:Port => port}).tap do |s|
      ['INT', 'TERM'].each {|signal| trap(signal) {s.shutdown; @server = nil} }
      s.mount("/", Handler)
      Thread.new { s.start }
    end
    nil # too noisy in inf-ruby otherwise
  end

  class TypeSpace
    attr_reader :target_type, :instance

    def initialize(target, mod, instance, superc)
      @target = target
      @mod = mod
      @instance = instance
      @superc = superc
      guess_target_type
    end

    def scan_with(scanner)
      obj = target_type
      modules = obj.ancestors - [obj]
      modules -= obj.included_modules unless instance
      modules +=
        ObjectSpace.each_object(obj.singleton_class).to_a unless @superc

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
        @target_type = Zossima.resolve_context(@target, @mod)
        unless @target_type.is_a? Module
          @target_type, @instance = @target_type.class, true
        end
      end rescue nil

      @target_type ||= Object
    end
  end

  class Scanner
    attr_accessor :check_private
    attr_reader :candidates

    def initialize(sym, check_private)
      @candidates = []
      @sym = sym
      @check_private = check_private
    end
  end

  class ModuleScanner < Scanner
    def scan(modules, check_instance, check_module)
      modules.each do |m|
        if check_module
          scan_methods(m, :methods, :module)
          scan_methods(m, :private_methods, :module) if check_private
        end
        if check_instance
          scan_methods(m, :instance_methods, :instance)
          scan_methods(m, :private_instance_methods, :instance) if check_private
        end
      end
    end

    private

    def scan_methods(mod, method, type)
      candidates << [mod, type] if mod.send(method, false).include?(@sym)
    end
  end

  class MethodScanner < Scanner
    def initialize(*args)
      super
      @re = /^#{Regexp.escape(@sym || "")}/
    end

    def scan(modules, check_instance, check_module)
      modules.each do |m|
        methods = []
        methods += m.instance_methods(false) if check_instance
        methods += m.methods(false) if check_module
        methods += m.private_instance_methods(false) if (check_instance &&
                                                         check_private)
        candidates.concat(methods.grep(@re))
      end
    end
  end
end
