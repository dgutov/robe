# frozen_string_literal: true

require 'pry'

begin
  require 'pry-doc' if RUBY_ENGINE == 'ruby'
rescue LoadError
  # Whatever, it's optional.
end

module Robe
  class Sash
    class DocFor
      MethodInfo = Struct.new(:docstring, :source, :aliases, :visibility)

      def initialize(method)
        @method = method
      end

      def format
        self.class.method_struct(@method).to_h
      end

      def self.visibility(method)
        owner = method.owner
        name = method.name

        if owner.__public_instance_methods__(false).include?(name)
          :public
        elsif owner.__protected_instance_methods__(false).include?(name)
          :protected
        elsif owner.__private_instance_methods__(false).include?(name)
          :private
        end
      end

      def self.method_struct(method)
        info = Pry::Method.new(method)

        aliases = info.aliases.map(&:to_sym)

        if info.dynamically_defined?
          doc = ''
          source = '# This method was defined outside of a source file.'
        else
          doc = info.doc
          source = (info.source? ? info.source : '# Not available.')
        end

        DocFor::MethodInfo.new(doc, source, aliases, visibility(method))
      rescue Pry::CommandError
        message = $ERROR_INFO.message =~ /pry-doc/ ? $ERROR_INFO.message : ''
        DocFor::MethodInfo.new(message, nil, aliases, visibility(method))
      end
    end
  end
end
