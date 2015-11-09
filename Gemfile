source "https://rubygems.org"

gem "rspec", "~> 2.14"
gem "rake", "~> 10.3.0"
gem "pry", "~> 0.9.12"
gem "pry-doc", ">= 0.6.0", :require => false, :platforms => :mri
gem "method_source", ">= 0.8.2"

if RUBY_VERSION.to_f < 1.9
  gem "activesupport", "<= 3.1.0"
  gem "backports"
else
  gem "activesupport"
end


group :metrics do
  gem "simplecov", :require => false
end
