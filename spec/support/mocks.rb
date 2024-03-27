# frozen_string_literal: true

require 'robe/visor'

class ScopedVisor < Robe::Visor
  def initialize(*modules)
    @namespace = if modules.last.is_a? Hash
                   modules.pop
                 else
                   {}
                 end
    @modules = modules + @namespace.values
  end

  def fits?(type, m)
    m.is_a? type
  end

  def each_object(type)
    @modules.select { |m| fits?(type, m) }.each { |m| yield m if block_given? }
  end

  def resolve_path_elems(nesting, lax = false)
    base = @namespace[nesting.shift]
    [base] + super(nesting, lax, base)
  end
end

class BlindVisor < ScopedVisor
  def fits?(_type, _m)
    true
  end
end
