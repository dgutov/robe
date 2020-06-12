require 'robe/sash'
require 'robe/server'

module Robe
  class << self
    attr_accessor :server, :server_thread

    def start(port = 0, host = '127.0.0.1')
      return running_string if @server

      @server = Server.new(Sash.new, host, port)

      ['INT', 'TERM'].each do |signal|
        trap(signal) { stop }
      end

      at_exit do
        stop
      end

      Thread.new do
        @server_thread = Thread.current

        unless server_thread[:__yard_registry__]
          server_thread[:__yard_registry__] = Thread.main[:__yard_registry__]
        end

        server_thread.abort_on_exception = true

        @server && @server.start
      end

      @server.wait_for_it

      running_string
    end

    def stop
      @server && @server.shutdown
      @server = nil
    end

    private

    def running_string
      "robe on #{@server.port}"
    end
  end
end
