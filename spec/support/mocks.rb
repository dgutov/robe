require 'robe/visor'

class MockVisor < Robe::Visor
  def initialize(*modules)
    @modules = modules
  end

  def fits?(type, m)
    true
  end

  def each_object(type)
    @modules.select { |m| fits?(type, m) }.each { |m| yield m if block_given? }
  end
end

class ScopedVisor < MockVisor
  def fits?(type, m)
    m.kind_of? type
  end
end
