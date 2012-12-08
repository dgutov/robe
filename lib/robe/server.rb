require 'webrick'
require 'json'
require 'tmpdir'

module Robe
  class Server < WEBrick::HTTPServer
    def initialize(port, sash)
      access_log = File.open("#{Dir.tmpdir}/robe-access.log", "w")
      access_log.sync = true
      super(Port: port, AccessLog:
            [[access_log, WEBrick::AccessLog::COMMON_LOG_FORMAT]])
      mount("/", Handler, sash)
    end

    class Handler < WEBrick::HTTPServlet::AbstractServlet
      attr_reader :sash

      def initialize(server, sash)
        super(server)
        @sash = sash
      end

      def do_GET(req, res)
        _, endpoint, *args = req.path.split("/").map { |s| s == "-" ? nil : s }
        value = sash.public_send(endpoint.to_sym, *args)
        res["Content-Type"] = "application/json"
        res.status = 200
        res.body = value.to_json
        raise WEBrick::HTTPStatus::OK
      end
    end
  end
end
