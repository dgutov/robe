require 'pry'
require 'pry-doc'

module Robe
  class Sash
    module PryDocInfo
      include Pry::Helpers::DocumentationHelpers

      def method_struct(method)
        Thread.exclusive do
          Thread.current[:__yard_registry__] = Thread.main[:__yard_registry__]
          begin
            info = Pry::Method.new(method)
            OpenStruct.new(docstring: info.doc,
                           source: info.source,
                           aliases: info.aliases.map(&:to_sym))
          rescue Pry::CommandError
            return OpenStruct.new(docstring: "")
          end
        end
      end
    end
  end
end
