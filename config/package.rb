require 'yaml'
require 'capistrano/recipes/deploy/scm'
require 'capistrano/recipes/deploy/strategy'
require 'capistrano/recipes/deploy/dependencies'


namespace :package do
  desc 'Create snapshot package'
  task 'snapshot' do
    transaction do
      checkout
      tarball
    end
  end

  desc 'Create fail'
  task 'failure' do
    run 'hogehoge'
  end

  desc 'Create release package'
  task 'release' do
    # checkout
    # tarball
    # tagging
  end

  desc 'Create source code tarball'
  task 'tarball' do
    run "cd #{build_path} && tar cvzf #{release_name}.tar.gz #{release_name}"
  end

  task 'zip' do
    run "cd #{build_path} && zip -r cvzf #{release_name}.zip #{release_name}"
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

  desc 'Checkout code from repository'
  task 'checkout' do
    on_rollback { run "rm -rf #{package_path}; true" }
    run source.checkout(revision,package_path)
    run "cd #{package_path} && omake check && omake integrate && omake clean"
  end
end
