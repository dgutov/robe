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

  def self.method_info(mod, type, sym)
    method = mod.send(type == :instance ? :instance_method : :method, sym)
    [mod.name, type, sym] + method.source_location.to_a
  end

  def self.resolve_target(name, mod)
    return eval(mod) unless name
    return eval(name) if name =~ /\A::/
    nesting = mod ? mod.split("::") : []
    while nesting.any?
      if obj = eval((nesting + [name]).join("::")) rescue nil
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

    mf, imf = MethodFinder.new(sym), InstanceMethodFinder.new(sym)
    targets, finders = [], nil

    blk = lambda do |m|
      next unless m.name
      finder = finders.find {|er| er.fits?(m)}
      targets << [m, finder.type] if finder
    end

    if candidates
      finders = [instance ? imf : mf]
      candidates.each(&blk)
      unless instance
        finders = [imf]
        obj.singleton_class.included_modules.each(&blk)
      end
    end

    if targets.any?
      targets.reject! do |(m, _)|
        !(m <= obj) && targets.find {|(t, _)| t < m}
      end
    elsif !obj or (sym != :initialize and !superc)
      finders = [mf, imf]
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

  class MethodFinder
    def initialize(symbol)
      @sym = symbol
    end

    def fits?(mod)
      if method = get_method(mod) rescue nil
        (owner = method.owner) == mod or
          !owner.name && !(mod.respond_to?(:superclass) &&
                           defined_in?(mod.superclass))
      end
    end

    def get_method(mod)
      mod.method(@sym)
    end

    def defined_in?(mod)
      mod.singleton_class.method_defined?(@sym)
    end

    def type
      :module
    end
  end

  class InstanceMethodFinder < MethodFinder
    def get_method(mod)
      mod.instance_method(@sym)
    end

    def defined_in?(mod)
      mod.method_defined?(@sym)
    end

    def type
      :instance
    end
  end
end
