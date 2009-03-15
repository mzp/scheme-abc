namespace :package do
  desc 'Create snapshot package'
  task 'snapshot' do
    transaction do
      checkout
      src
      win

      check.src
      check.win
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

  task 'src',:roles=>[:src] do
    run "cd #{build_path} && tar czf #{package_path}-src.tar.gz #{package_name}"
  end

  task 'win',:roles=>[:win] do
    on_rollback { run "rm -rf #{package_path}-win32; true" }
    run "rm -rf #{package_path}-win32"
    run "cd #{package_path} && #{omake} config RELATIVE=true PREFIX=$(cygpath -m #{package_path}-win32) && #{omake} install"
    run "cd #{build_path}   && zip -rq #{package_name}-win32.zip #{package_name}-win32"
  end

  desc "Cleanup build file"
  task 'clean' do
    run "rm -rf #{build_path}/*"
  end
end
