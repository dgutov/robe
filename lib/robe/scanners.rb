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
        if check_module && m.respond_to?(:singleton_class)
          sc = m.singleton_class
          scan_methods(sc, :instance_methods)
          scan_methods(sc, :private_instance_methods) if check_private
        end
        if check_instance
          scan_methods(m, :instance_methods)
          scan_methods(m, :private_instance_methods) if check_private
        end
      end
    end
  end

  class ModuleScanner < Scanner
    def scan_methods(mod, coll)
      candidates << mod.instance_method(@sym) if mod.send(coll, false).include?(@sym)
    end
  end

  class MethodScanner < Scanner
    def initialize(*args)
      super
      @re = /^#{Regexp.escape(@sym || "")}/
    end

    def scan_methods(mod, coll)
      mod.send(coll, false).grep(@re) do |sym|
        candidates << mod.instance_method(sym)
      end
    end
  end
end
