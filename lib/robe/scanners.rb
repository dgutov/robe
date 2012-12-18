module Robe
  class Scanner
    attr_accessor :check_private
    attr_reader :candidates

    def initialize(sym, check_private)
      @candidates = []
      @sym = sym
      @check_private = check_private
    end

    def scan(modules, check_instance, check_module)
      modules.each do |m|
        if check_module
          scan_methods(m, :methods, :method)
          scan_methods(m, :private_methods, :method) if check_private
        end
        if check_instance
          scan_methods(m, :instance_methods, :instance_method)
          scan_methods(m, :private_instance_methods, :instance_method) if check_private
        end
      end
    end
  end

  class ModuleScanner < Scanner
    def scan_methods(mod, coll, getter)
      candidates << mod.send(getter, @sym) if mod.send(coll, false).include?(@sym)
    end
  end

  class MethodScanner < Scanner
    def initialize(*args)
      super
      @re = /^#{Regexp.escape(@sym || "")}/
    end

    def scan_methods(mod, coll, getter)
      return if coll == :private_methods
      mod.send(coll, false).grep(@re) do |sym|
        candidates << [sym, mod.send(getter, sym).parameters]
      end
    end
  end
end
