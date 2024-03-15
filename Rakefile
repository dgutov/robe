require 'rake'
require 'rspec/core/rake_task'

RSpec::Core::RakeTask.new do |t|
  t.verbose = false
  t.fail_on_error = false
end

RSpec::Core::RakeTask.new(:spec_for_build)

task :default => :build

task :build => [:spec_for_build, :ert] do
  system("emacs -Q --batch -l build.el -f build")
end
