
def depend(location, type, *args)
  deps = fetch(:dependencies, {})
  deps[location] ||= {}
  deps[location][type] ||= []
  deps[location][type] << args
  set :dependencies, deps
end

namespace :package do
desc "Test build dependencies"
task 'check' do
  dependencies =  Dependencies.new(configuration)
  other = fetch(:dependencies, {})
  other.each do |location, types|
    types.each do |type, calls|
      calls.each do |args|
        dependencies.send(location).send(type, *args)
      end
    end
  end

  if dependencies.pass?
    puts "You appear to have all necessary dependencies installed"
  else
    puts "The following dependencies failed. Please check them and try again:"
    dependencies.reject { |d| d.pass? }.each do |d|
      puts "--> #{d.message}"
    end
    abort
  end
end
end
