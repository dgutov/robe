module Robe
  class Sash
    class DocFor
      begin
        require 'robe/sash/pry_doc_info'
        include PryDocInfo
      rescue LoadError
        require 'robe/sash/pry_doc_fallback'
        include PryDocFallback
      end

      def initialize(method)
        @method = method
      end

      def format
        info = method_struct(@method)
        {docstring: info.docstring,
         source: info.source,
         aliases: info.aliases,
         visibility: visibility}
      end

      def visibility
        owner, name = @method.owner, @method.name
        if owner.public_instance_methods(false).include?(name)
          :public
        elsif owner.protected_instance_methods(false).include?(name)
          :protected
        elsif owner.private_instance_methods(false).include?(name)
          :private
        end
      end
    end
  end
end
