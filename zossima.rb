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

  def self.start(port)
    @server ||= WEBrick::HTTPServer.new({:Port => port}).tap do |s|
      ['INT', 'TERM'].each {|signal| trap(signal) {s.shutdown} }
      s.mount("/", Handler)
      Thread.new { s.start }
    end
    nil # too noisy in inf-ruby otherwise
  end
end
