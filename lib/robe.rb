require "robe/server"
require "robe/kitchen"

module Robe
  def self.start(port)
    return if @server
    @server = Server.new(port, Kitchen.new)
    ['INT', 'TERM'].each do |signal|
      trap(signal) { @server.shutdown; @server = nil }
    end
    Thread.new { @server.start }
    nil # no output
  end
end
