require 'robe/server'
require 'robe/sash'

module Robe
  class << self
    attr_accessor :server

    def start(port)
      return if @server
      @server = Server.new(port, Sash.new)
      ['INT', 'TERM'].each do |signal|
        trap(signal) { stop }
      end
      Thread.new { @server.start }
      nil # no output
    end

    def stop
      @server.shutdown
      @server = nil
    end
  end
end
