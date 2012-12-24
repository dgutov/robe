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
          OpenStruct.new(docstring: info ? info.docstring : "",
                         source: info && strip_comments_from_c_code(info.source))
        end
      end
    end
  end
end
