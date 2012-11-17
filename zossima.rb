require "webrick"
require "json"

module Zossima
  class Handler < WEBrick::HTTPServlet::AbstractServlet
    def do_GET(req, res)
      _, endpoint, *args = req.path.split("/")
      value = Zossima.send(endpoint.to_sym, *args)
      res["Content-Type"] = "application/json"
      res.status = 200
      res.body = value.to_json
      raise WEBrick::HTTPStatus::OK
    end
  end

  def self.location(klass_name, method_type, method)
    klass = eval(klass_name)
    if method_type == "instance"
      klass.instance_method(method.to_sym).source_location
    else
      klass.method(method.to_sym).source_location
    end
  end

  def self.modules
    ObjectSpace.each_object(Module).map{|c| c.name }.compact!
  end

  def self.targets(obj)
    obj = eval(obj)
    if obj.is_a? Module
      module_methods = obj.methods.select{|m| obj.method(m).source_location}
        .map{|m| "#{obj}.#{m}"}
      instance_methods = (obj.instance_methods +
                          obj.private_instance_methods(false))
        .select{|m| obj.instance_method(m).source_location}
        .map{|m| "#{obj}\##{m}"}
      # XXX: Filter out not overridden methods? obj.instance_method(m).owner
      module_methods + instance_methods
    else
      self.targets(obj.class.to_s)
    end
  end

  def self.method_targets(method, target = nil, instance = nil)
    sym = method.to_sym
    if (eval(target).send(instance ? :instance_method : :method, sym) rescue nil)
      [[target, instance ? "instance" : "module"]]
    else
      targets = []
      ObjectSpace.each_object(Module) do |m|
        next unless m.name
        mf = [MethodFinder.new(sym), InstanceMethodFinder.new(sym)]
          .find {|f| f.candidate?(m)}
        targets << [m.name, mf.type] if mf
      end
      targets
    end
  end

  def self.start(port)
    @server ||= WEBrick::HTTPServer.new({:Port => port}).tap do |s|
      Rails.application.eager_load! rescue nil
      ['INT', 'TERM'].each {|signal| trap(signal) {s.shutdown} }
      s.mount("/", Handler)
      Thread.new { s.start }
    end
    nil # too noisy in inf-ruby otherwise
  end

  class MethodFinder
    def initialize(symbol)
      @sym = symbol
    end

    def candidate?(mod)
      if method = get_method(mod) rescue nil
        return false unless method.source_location
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
      "module"
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
      "instance"
    end
  end
end
