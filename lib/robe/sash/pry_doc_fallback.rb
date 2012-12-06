module Robe
  class Sash
    module PryDocFallback
      def method_struct(method)
        if method.source_location
          buf = []
          loc, n = method.source_location
          File.open(loc) do |f|
            f.each_line.with_index do |line, index|
              break if index == n - 1
              case line
              when /\A[ \t]*#( (?<text>.*))?/
                buf << $~[:text]
              when /\A[ \t]*[^#]/
                buf.clear
              end
            end
          end
          comment = buf.join("\n")
        else
          comment = "Docstring not found. For core classes, try installing 'pry-doc'."
        end
        OpenStruct.new(docstring: comment)
      end
    end
  end
end
