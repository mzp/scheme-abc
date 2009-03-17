namespace :package do
  desc 'Create snapshot package'
  task 'snapshot' do
    set :version,"snapshot-#{real_revision}"
    set :package_name,"#{application}-#{real_revision}"

    create_package
  end

  desc "Create release package"
  task 'release' do
    unless ENV['VERSION']
      abort "Please specify the release-version, 'VERSION=0.4.2 cap ...'"
    end
    set :version,ENV['VERSION']
    set :package_name,"#{application}-#{version}"

    create_package
  end

  def create_package
    f = File.open("/tmp/scheme-abc", "w")
    f.flock(File::LOCK_EX)

    transaction do
      checkout
      archive

      check.package
    end
  end

  task 'checkout' do
    on_rollback { run "rm -rf #{package_path}; true" }

    run "rm -rf #{package_path}"
    run source.checkout(revision,package_path)
    run <<END
cd #{package_path} &&
echo #{version} > VERSION &&
#{omake} config &&
#{omake} check &&
#{omake} integrate &&
#{omake} distclean
END
  end

  task 'archive' do
    on_rollback { run "rm -rf #{package_path}-*; true" }

    parallel do |session|
      session.when "in?(:win)", <<WIN
rm -rf #{package_path}-win32 &&
cd #{package_path} &&
#{omake} config RELATIVE=true PREFIX=$(cygpath -m #{package_path}-win32) &&
#{omake} install &&
cd #{build_path} &&
zip -rq #{package_name}-win32.zip #{package_name}-win32
WIN

      session.when "in?(:src)", <<SRC
cd #{build_path} &&
mv  #{package_name}  #{package_name}-src &&
tar czf #{package_path}-src.tar.gz #{package_name}-src
SRC
    end
  end

  desc "Cleanup build file"
  task 'clean' do
    run "rm -rf #{build_path}/*"
  end
end
