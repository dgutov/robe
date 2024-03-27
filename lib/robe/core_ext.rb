# frozen_string_literal: true

class Module
  alias __name__ name unless method_defined?(:__name__)

  alias __singleton_class__ singleton_class unless method_defined?(:__singleton_class__)

  alias __include__? include? unless method_defined?(:__include__?)

  alias __instance_methods__ instance_methods unless method_defined?(:__instance_methods__)

  alias __public_instance_methods__ public_instance_methods unless method_defined?(:__public_instance_methods__)

  unless method_defined?(:__protected_instance_methods__)
    alias __protected_instance_methods__ protected_instance_methods
  end

  unless method_defined?(:__private_instance_methods__)
    alias __private_instance_methods__ private_instance_methods
  end
end
