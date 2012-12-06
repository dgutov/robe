module Robe
  class Sash
    module PryDocInfo
      def method_struct(method)
        Thread.exclusive do
          Thread.current[:__yard_registry__] = Thread.main[:__yard_registry__]
          Pry::MethodInfo.info_for(method)
        end
      end
    end
  end
end
