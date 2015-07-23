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

  def start_and_send(handler, request)
    server = described_class.new(handler, 0)

    Thread.new do
      Thread.current.abort_on_exception = true
      server.start
    end

    server.wait_for_it

    http = Net::HTTP.new("127.0.0.1", server.port)
    http.request(request)
  ensure
    server.shutdown
  end
end
