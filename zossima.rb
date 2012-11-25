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
    if (obj = resolve_target(name, mod) rescue nil) and obj.is_a? Module
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
    obj = eval(obj)
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
    method = find_method(eval(mod), type.to_sym, sym.to_sym)
    begin
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
        comment = "Definition not found. For core classes, try installing 'pry-doc'."
      end
    end
    {comment: comment,
     signature: signature(mod, type.to_sym, sym)}
  end

  def self.signature(mod, type, sym)
    sig = "#{mod}#{type == :instance ? '#' : '.'}#{sym}("
    dummy = "arg0"
    find_method(eval(mod), type, sym).parameters.each_with_index do |(kind, name), n|
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

  def self.resolve_target(name, mod)
    return eval(mod) unless name
    return eval(name) if name =~ /\A::/
    nesting = mod ? mod.split("::") : []
    while nesting.any?
      if obj = begin eval((nesting + [name]).join("::"))
               rescue NameError
               rescue SyntaxError
               end
        return obj
      else
        nesting.pop
      end
    end
    eval(name)
  end

  def self.method_targets(method, target, mod, instance = nil, superc = nil)
    sym = method.to_sym
    begin
      obj = resolve_target(target, mod)
      obj, instance = obj.class, true unless obj.is_a? Module
    end rescue nil

    if obj
      candidates = obj.ancestors - [obj]
      candidates -= obj.included_modules unless instance
      candidates +=
        ObjectSpace.each_object(obj.singleton_class).to_a unless superc

      if instance
        if defined? ActiveSupport::Concern and obj.is_a?(ActiveSupport::Concern)
          deps = obj.instance_variable_get("@_dependencies")
          candidates += deps if deps
        end
      end
    end

    mf, imf = MethodChecker.new(sym), InstanceMethodChecker.new(sym)
    targets, checkers = [], nil

    blk = lambda do |m|
      finder = checkers.find {|er| er.fits?(m)}
      targets << [m, finder.type] if finder
    end

    if candidates
      checkers = [instance ? imf : mf]
      candidates.each(&blk)
      unless instance
        checkers = [imf]
        obj.singleton_class.ancestors.each(&blk)
      end
    end

    if targets.any?
      targets.reject! do |(m, _)|
        !(m <= obj) && targets.find {|(t, _)| t < m}
      end
    elsif !obj or (sym != :initialize and !superc)
      checkers = [mf, imf]
      ObjectSpace.each_object(Module, &blk)
    end

    targets.map {|(m, type)| method_info(m, type, sym)}
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

  class MethodChecker
    def initialize(symbol)
      @sym = symbol
    end

    def fits?(mod)
      mod.methods(false).include?(@sym)
    end

    def type
      :module
    end
  end

  class InstanceMethodChecker < MethodChecker
    def fits?(mod)
      mod.instance_methods(false).include?(@sym) or
        mod.private_instance_methods(false).include?(@sym)
    end

    def type
      :instance
    end
  end
end
