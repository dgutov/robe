require 'spec_helper'
require 'robe'

describe Robe do
  before do
    $stderr = File.new(IO::NULL, "w")
    Robe.start(12345)
  end

  after do
    Robe.stop if Robe.server
    $stderr = STDERR
  end

  it "has a server attribute" do
    expect(Robe.server).to be_a(Robe::Server)
  end

  it "has a stop method" do
    expect_stop do
      Robe.stop
    end
  end

  %w(INT TERM).each do |signal|
    it "shuts down on #{signal}" do
      expect_stop do
        Process.kill(signal, Process.pid)
      end
    end
  end

  def expect_stop
    server = Robe.server
    yield
    expect(Robe.server).to be_nil
    expect(server.status).to eq(:Stop)
  end
end
