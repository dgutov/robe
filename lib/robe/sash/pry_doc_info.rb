require 'pry'
require 'pry-doc'

module Robe
  class Sash
    module PryDocInfo
      include Pry::Helpers::DocumentationHelpers

      def method_struct(method)
        Thread.exclusive do
          Thread.current[:__yard_registry__] = Thread.main[:__yard_registry__]
          info = Pry::MethodInfo.info_for(method)
          return OpenStruct.new(docstring: "") unless info
          source = info.source
          OpenStruct.new(docstring: info.docstring.to_s,
                         source: source && strip_comments_from_c_code(source),
                         aliases: method_aliases(info))
        end
      end

      private

      def method_aliases(info)
        overloads = info.tags.select { |t| t.tag_name == "overload" }
        overloads.map(&:name).uniq - [info.name]
      end
    end
  end
end
