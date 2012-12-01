module Robe
  class Scanner
    attr_accessor :check_private
    attr_reader :candidates

    def initialize(sym, check_private)
      @candidates = []
      @sym = sym
      @check_private = check_private
    end
  end

  class ModuleScanner < Scanner
    def scan(modules, check_instance, check_module)
      modules.each do |m|
        if check_module
          scan_methods(m, :methods, :module)
          scan_methods(m, :private_methods, :module) if check_private
        end
        if check_instance
          scan_methods(m, :instance_methods, :instance)
          scan_methods(m, :private_instance_methods, :instance) if check_private
        end
      end
    end

    def scan_methods(mod, method, type)
      candidates << [mod, type] if mod.send(method, false).include?(@sym)
    end
  end

  class MethodScanner < Scanner
    def initialize(*args)
      super
      @re = /^#{Regexp.escape(@sym || "")}/
    end

    def scan(modules, check_instance, check_module)
      modules.each do |m|
        methods = []
        methods += m.instance_methods(false) if check_instance
        methods += m.methods(false) if check_module
        methods += m.private_instance_methods(false) if (check_instance &&
                                                         check_private)
        candidates.concat(methods.grep(@re))
      end
    end
  end
end
