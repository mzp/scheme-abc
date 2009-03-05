namespace :package do
  desc 'Create snapshot package'
  task 'snapshot' do
    transaction do
      checkout
      tarball
    end
  end

  desc 'Create release package'
  task 'release' do
    # checkout
    # tarball
    # tagging
  end

  desc 'Package Windows binary'
  task 'win' do
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
    run source.checkout(revision,package_path)
    run "cd #{package_path} && omake check && omake integrate && omake clean"
  end

  task 'tarball' do
    run "cd #{build_path} && tar cvzf #{package_path}.tar.gz #{package_path}"
  end
end
