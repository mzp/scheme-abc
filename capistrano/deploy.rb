namespace :deploy do
  desc 'Deploy snapshot package to <deploy_server>'
  task 'snapshot' do
    transaction do
      package.snapshot
      upload
      symlink
      delete_old
    end
  end

  task 'release' do
    transaction do
      package.release

      on_rollback {
        run_remote deploy_server,"rm -f #{deploy_to}/snapshot/#{package_name}-*.*"
      }
      run "scp #{package_path}-*.* #{deploy_server}:#{deploy_to}/release/"
    end
  end

  task 'upload' do
    on_rollback {
      run_remote deploy_server,"rm -f #{deploy_to}/snapshot/#{package_name}-*.*"
    }
    run "scp #{package_path}-*.* #{deploy_server}:#{deploy_to}/snapshot/"
  end

  desc 'Update symlink to latest package'
  task 'symlink' do
    run_remote deploy_server,with_cd("#{deploy_to}/current"){
      "#{deploy_to}/script/update_syms #{deploy_to}/snapshot/"
    }
  end

  task 'delete_old' do
    run_remote deploy_server,"#{deploy_to}/script/delete_old 10 #{deploy_to}/snapshot/"
  end

  desc 'Update statics page'
  task 'statics' do
    run_remote deploy_server,with_cd("#{deploy_to}/"){
      "#{deploy_to}/script/update_statics #{real_revision} #{deploy_to}/current #{deploy_to}/release > index.html"
    }
  end

  desc 'Setup deploy server.'
  task "setup" do
    run_remote deploy_server,"mkdir -p #{deploy_to}/snapshot"
    run_remote deploy_server,"mkdir -p #{deploy_to}/release"
    run_remote deploy_server,"mkdir -p #{deploy_to}/current"
    run_remote deploy_server,"mkdir -p #{deploy_to}/script"

    run_locally "scp -r capistrano/css #{deploy_server}:#{deploy_to}"

    run_locally "scp capistrano/update_syms #{deploy_server}:#{deploy_to}/script"
    run_locally "scp capistrano/update_statics #{deploy_server}:#{deploy_to}/script"
    run_locally "scp capistrano/delete_old #{deploy_server}:#{deploy_to}/script"
    run_remote deploy_server,"chmod a+x #{deploy_to}/script/*"
  end
end
