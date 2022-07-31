require 'json'
require 'tmpdir'
require 'socket'
require 'logger'
require 'uri'

module Robe
  class Server
    attr_reader :running, :port

    REQUEST_TIMEOUT = 0.15

    def initialize(handler, host, port)
      @handler = handler
      @server = TCPServer.new(host, port)
      @running = true
      @port = @server.addr[1]
    end

    def start
      access = File.open("#{Dir.tmpdir}/robe-access-#{@port}.log", "w")
      access.sync = true

      error_logger = Logger.new($stderr)
      access_logger = Logger.new(access)

      client = nil

      loop do
        begin
          client = @server.accept

          if !wait_readable(client) || client.eof?
            client.close
            next
          end

          # Only read the first line. To support reading the body,
          # we'll have to parse the headers.
          /\A(?<request_method>[A-Z]+) (?<path>[^ ]+)/ =~ client.gets

          path = URI.decode_www_form_component(path)

          access_logger.info "#{request_method} #{path}"

          begin
            body = @handler.call(path, nil)
            status = 200
          rescue Exception => e
            body = "#{e.message}\n#{e.backtrace.join("\n")}"
            error_logger.error "Request failed: #{path}. Please file an issue."
            error_logger.error body
            status = 500
          end

          # XXX: If freezes continue, try: resp.keep_alive = false
          begin
            client.write("HTTP/1.1 #{status} OK\r\n" +
"Content-Length: #{body.bytesize}\r\n" +
"Connection: close\r\n\r\n")
            client.write(body)
            client.close
          rescue Errno::EPIPE
            error_logger.error "Connection lost, unsent response:"
            error_logger.error body
          end
        rescue Errno::EINVAL
          break
        rescue IOError
          # Hello JRuby
          break
        end
      end
    end

    def wait_for_it
      begin
        TCPSocket.new("127.0.0.1", @port).close
      rescue
        sleep 0.05
        retry
      end
    end

    def shutdown
      @running = false
      @server && @server.close
    end

    private

    # On 2.0+, can require 'io/wait' instead.
    def wait_readable(socket)
      if socket.respond_to?(:wait_readable)
        socket.wait_readable(REQUEST_TIMEOUT)
      else
        IO.select([socket], nil, nil, REQUEST_TIMEOUT)
      end
    end
  end
end
