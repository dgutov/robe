require 'robe/visor'

class ScopedVisor < Robe::Visor
  def initialize(*modules)
    if modules.last.is_a? Hash
      @namespace = modules.pop
    else
      @namespace = {}
    end
    @modules = modules + @namespace.values
  end

  def fits?(type, m)
    m.kind_of? type
  end

  def each_object(type)
    @modules.select { |m| fits?(type, m) }.each { |m| yield m if block_given? }
  end

  def resolve_path_in(base, nesting)
    super(@namespace[nesting.shift], nesting)
  end
end

class BlindVisor < ScopedVisor
  def fits?(type, m)
    true
  end
end
