require 'json'
require 'tmpdir'
require 'socket'
require 'webrick'
require 'logger'

module Robe
  class Server
    attr_reader :running

    def initialize(handler, port)
      @port = port
      @handler = handler

      @server = TCPServer.new("127.0.0.1", @port)
      @running = true
    end

    def start
      access = File.open("#{Dir.tmpdir}/robe-access.log", "w")
      access.sync = true

      error_logger = Logger.new($stderr)
      access_logger = Logger.new(access)

      client = nil

      loop do
        begin
          client = @server.accept
          req = WEBrick::HTTPRequest.new(:InputBufferSize => 1024,
                                         :Logger => error_logger)
          req.parse(client)
          access_logger.info "#{req.request_method} #{req.path}"

          begin
            body = @handler.call(req.path, req.body)
          rescue Exception => e
            error_logger.error "Request failed: #{req.path}. Please file an issue."
            error_logger.error "#{e.message}\n#{e.backtrace.join("\n")}"
          end

          resp = WEBrick::HTTPResponse.new(:OutputBufferSize => 1024,
                                           :Logger => error_logger,
                                           :HTTPVersion => "1.1")
          resp.status = 200
          resp.body = body

          resp.send_response(client)
          client.close
        rescue Errno::EINVAL
          break
        end
      end
    end

    def shutdown
      @running = false
      @server && @server.shutdown(Socket::SHUT_RDWR)
    end
  end
end
