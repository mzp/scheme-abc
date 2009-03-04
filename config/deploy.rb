namespace :deploy do
  desc 'deploy'
  task 'snapshot' do
    transaction do
      upload
      symlink
    end
  end

  task 'upload' do
    on_rollback {
      run_remote deploy_server,"rm -f #{deploy_to}/snapshot/#{package_path}.*"
    }
    run "scp #{package_path}.* #{deploy_server}:#{deploy_to}/snapshot/"
  end

  task 'symlink' do
    run_remote deploy_server,"cd #{deploy_to} && rm -f #{current_path}/source.tar.gz && ln -s #{package_name}.tar.gz #{current_path}/source.tar.gz"
    run_remote deploy_server,"cd #{deploy_to} && rm -f #{current_path}/source.zip && ln -s #{package_name}.zip #{current_path}/source.zip"
  end

  task "setup" do
    run_remote deploy_server,"mkdir -p #{deploy_to}/snapshot"
    run_remote deploy_server,"mkdir -p #{deploy_to}/release"
    run_remote deploy_server,"mkdir -p #{deploy_to}/current"
  end
end
