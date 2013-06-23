require 'pry'
require 'pry-doc'

module Robe
  class Sash
    module PryDocInfo
      def method_struct(method)
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
