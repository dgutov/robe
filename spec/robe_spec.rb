require 'spec_helper'
require 'robe'
require 'net/http'

describe Robe do
  before do
    $stderr = File.new(IO::NULL, "w")
    Robe.start
  end

  after do
    Robe.stop if Robe.server
    $stderr = STDERR
  end

  it "has a server attribute" do
    expect(Robe.server).to be_a(Robe::Server)
  end

  it "has the server running" do
    expect(Robe.server.running).to be_true
  end

  it "has a stop method" do
    expect { Robe.stop }.to stop_it
  end

  it "proxies 'ping' to Sash" do
    resp = Net::HTTP.get("127.0.0.1", "/ping", Robe.server.port)
    expect(resp).to eq("\"pong\"")
  end

  it "returns 'robe on' when already running" do
    expect(Robe.start).to start_with("robe on")
  end

  %w(INT TERM).each do |signal|
    it "shuts down on #{signal}" do
      expect { Process.kill(signal, Process.pid) }.to stop_it
    end
  end

  RSpec::Matchers.define :stop_it do
    match do |proc|
      server = Robe.server
      proc.call
      sleep 0.001
      server.running == false
    end
  end
end
