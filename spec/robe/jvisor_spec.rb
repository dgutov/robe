require 'spec_helper'
require 'robe/jvisor'

describe Robe::JVisor do
  subject { Robe::JVisor.new }

  it "iterates though all native classes" do
    res = subject.each_object(Module)
    expect(res).to include(String)
    expect(res).to include(Set)
    expect(res).to include(Enumerable)
  end

  it "returns only modules responding to :name and :instance_methods" do
    res = subject.each_object(Module)
    expect(res.all? { |m| m.respond_to? :name }).to eq(true)
    expect(res.all? { |m| m.respond_to? :instance_methods }).to eq(true)
  end

  it "returns descendants of a module" do
    res = subject.descendants(Enumerable)
    expect(res).to include(Array)
    expect(res).not_to include(String)
  end

  it "returns descendants of a class" do
    res = subject.descendants(Numeric)
    expect(res).to include(Integer, Complex)
    expect(res).not_to include(String)
  end
end
