namespace :check do
  desc "Check to have all necessary dependencies"
  task 'depend' do
    dependencies = Capistrano::Deploy::Dependencies.new(self) do|d|
      other = fetch(:dependencies, {})
      other.each do |location, types|
        types.each do |type, calls|
          if type == :gem
            dependencies.send(location).command(fetch(:findlib_command,
                                                      "ocamlfind")).
              or("`ocamlfind' command could not be found. Try setting :findlib_command")
          end

          calls.each do |args|
            d.send(location).send(type, *args)
          end
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

  task 'src',:roles => [:src] do
    run "mkdir -p #{test_path}"
    run "cd #{test_path} && rm -rf *"
    run "cd #{test_path} && tar xzf #{package_path}-src.tar.gz"
    run "cd #{test_path}/#{package_name}/ && #{omake} config PREFIX=#{File.expand_path test_path}/prefix/ && #{omake} all && #{omake} install"
    run "cd #{test_path} && #{test_path}/prefix/bin/habc #{test_path}/prefix/share/habc/example/swf.scm"
    run "ls #{test_path}/a.swf"
  end

  task 'win',:roles => [:win] do
  end
end
