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

  $port = 3000

  def start_and_send(handler, request)
    $port += 1
    server = described_class.new(handler, $port)

    Thread.new do
      Thread.current.abort_on_exception = true
      server.start
    end

    Thread.new do
      http = Net::HTTP.new("127.0.0.1", $port)
      http.open_timeout = 1
      http.request(request)
    end.value
  ensure
    server.shutdown
  end
end
