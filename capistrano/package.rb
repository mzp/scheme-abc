namespace :package do
  desc 'Create snapshot package'
  task 'snapshot' do
    transaction do
      f = File.open("/tmp/scheme-abc", "w")
      f.flock(File::LOCK_EX)

      checkout
      archive

      check.snapshot
    end
  end

  desc 'Create release package'
  task 'release' do
    # checkout
    # tarball
    # tagging
  end

  desc 'Package MacOS X binary'
  task 'mac' do
    abort
  end

  desc 'Package Linux binary'
  task 'linux' do
    abort
  end

  task 'checkout' do
    on_rollback { run "rm -rf #{package_path}; true" }

    run "rm -rf #{package_path}"
    run source.checkout(revision,package_path)
    run "cd #{package_path} && #{omake} config && #{omake} check && #{omake} integrate && #{omake} distclean"
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
tar czf #{package_path}-src.tar.gz #{package_name}
SRC
    end
  end

  desc "Cleanup build file"
  task 'clean' do
    run "rm -rf #{build_path}/*"
  end
end
