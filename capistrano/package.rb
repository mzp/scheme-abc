namespace :package do
  desc 'Create snapshot package'
  task 'snapshot' do
    transaction do
      checkout
      src
      win
    end
  end

  desc 'Create release package'
  task 'release' do
    # checkout
    # tarball
    # tagging
  end

  desc 'Package Windows binary'
  task 'win',:role=>:win do
    transaction do
      checkout
      src
      win
    end
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
    run "cd #{package_path} && omake check && omake integrate && omake clean"
  end

  task 'src',:role=>:src do
    run "cd #{build_path} && tar cvzf #{package_path}-src.tar.gz #{package_name}"
  end

  task 'win',:role=>:win do
    on_rollback { run "rm -rf #{package_path}-win32; true" }
    run "rm -rf #{package_path}-win32"
    run "cd #{package_path} && omake install PREFIX=$(cygpath -w #{package_path}-win32)"
    run "cd #{build_path}   && zip -r #{package_name}-win32.zip #{package_name}-win32"
  end

  desc "Cleanup build file"
  task 'clean' do
    run "rm -rf #{build_path}/*"
  end
end
