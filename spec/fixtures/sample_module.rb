module SampleModule
  def self.included(other)
    other.module_eval do
      def bar
      end
    end
  end
end
