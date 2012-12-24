module Robe
  class Sash
    module PryDocFallback
      def method_struct(method)
        if method.source_location
          doc_buf, src_buf, indent = [], []
          loc, n = method.source_location
          File.open(loc) do |f|
            f.each_line.with_index do |line, index|
              case line
              when /\A[ \t]*#( (?<text>.*))?/
                doc_buf << $~[:text]
              when /\A[ \t]*[^#]/
                doc_buf.clear
              end
              break if index == n - 2
            end
            f.each_line.with_index do |line, index|
              if index == 0
                break unless line =~ /\A([ \t]*)def\b/
                indent = $1.length
                src_buf << line[indent..-1]
                break if line =~ /\bend *\Z/
              else
                src_buf << line[indent..-1]
                break if line =~ /\A[ \t]{#{indent}}end\b/
              end
            end
          end
          comment = doc_buf.join("\n")
          source = src_buf.join
        else
          comment = "Docstring not found. For core classes, try installing 'pry-doc'."
        end
        OpenStruct.new(docstring: comment, source: source, aliases: [])
      end
    end
  end
end
