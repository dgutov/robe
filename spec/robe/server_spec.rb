require 'spec_helper'
require 'robe/server'
require 'net/http'

describe Robe::Server do
  it "starts up and serves an empty response" do
    resp = start_and_send(proc {}, Net::HTTP::Get.new("/"))
    expect(resp.body).to eq("")
  end

  it "passes path and body to the handler" do
    rpath = nil
    rbody = nil

    handler = proc do |path, body|
      rpath = path
      rbody = body
      ""
    end

    req = Net::HTTP::Post.new("/foo")
    req.body = "bar\ntee"

    start_and_send(handler, req)
    expect(rpath).to eq("/foo")
    expect(rbody).to eq("bar\ntee")
  end

  it "responds with whatever the handler returns" do
    handler = proc { "foobles" }

    resp = start_and_send(handler, Net::HTTP::Get.new("/"))
    expect(resp.body).to eq("foobles")
  end

  it "prints out exceptions raised in the handler" do
    handler = proc { raise Exception.new("down with the king!") }

    begin
      $stderr = StringIO.new
      start_and_send(handler, Net::HTTP::Get.new("/"))
      $stderr.seek(0)
      expect($stderr.read).to match("down with the king!\n")
    ensure
      $stderr = STDERR
    end
  end

  it 'aborts slow requests' do
    start_and_yield(proc { "foo" }) do |server|
      socket = TCPSocket.new('127.0.0.1', server.port)
      sleep 0.2
      socket.puts("GET / HTTP/1.1\r\n")
      expect { socket.puts("\r\n") }.to raise_error(Errno::EPIPE)
    end
  end

  def start_and_yield(handler)
    server = described_class.new(handler, '127.0.0.1', 0)

    Thread.new do
      Thread.current.abort_on_exception = true
      server.start
    end

    server.wait_for_it

    yield(server)
  ensure
    server.shutdown
  end

  def start_and_send(handler, request)
    start_and_yield(handler) do |server|
      http = Net::HTTP.new('127.0.0.1', server.port)
      http.request(request)
    end
  end
end
