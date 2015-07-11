class Module
  unless method_defined?(:__name__)
    alias_method :__name__, :name
  end

  if method_defined?(:singleton_class?)
    alias_method :__singleton_class__?, :singleton_class?
  else
    def __singleton_class__?
      self != Class && ancestors.first != self
    end
  end

  unless method_defined?(:__singleton_class__)
    alias_method :__singleton_class__, :singleton_class
  end

  unless method_defined?(:__include__?)
    alias_method :__include__?, :include?
  end
end
