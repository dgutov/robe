require 'rake'
require 'rspec/core/rake_task'

RSpec::Core::RakeTask.new do |t|
  t.verbose = false
  t.fail_on_error = false
end

RSpec::Core::RakeTask.new(:spec_for_build)

task :default => :build

task :ert do
  emacs = ENV.fetch('EMACS', 'emacs')
  system("#{emacs} --batch --eval \"(package-initialize)\" -l robe.el\
         -l ert/core-tests.el -l ert/completion-tests.el\
         -f ert-run-tests-batch-and-exit") or exit
end

task :build => [:spec_for_build, :ert] do
  system("emacs -Q --batch -l build.el -f build")
end
