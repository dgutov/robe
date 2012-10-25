require "webrick"
require "json"
require "pp"

module Zossima
  class Handler < WEBrick::HTTPServlet::AbstractServlet
    def do_GET(req, res)
      _, endpoint, klass, method = req.path.split("/")
      value = Zossima.send(endpoint.to_sym, klass, method)
      res["Content-Type"] = "application/json"
      res.status = 200
      res.body = value.to_json
      raise WEBrick::HTTPStatus::OK
    end
  end

  def self.location(klass, method)
    # TODO: replace eval with ruby's equivalent of resolve
    eval(klass).instance_method(method.to_sym).source_location
  end

  def self.start(port)
    @server ||= WEBrick::HTTPServer.new({:Port => port}).tap do |s|
      ['INT', 'TERM'].each {|signal| trap(signal) {s.shutdown} }
      s.mount("/", Handler)
      Thread.new { s.start }
    end
  end
end
