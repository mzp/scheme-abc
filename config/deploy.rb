namespace :deploy do
  desc 'Deploy snapshot package to <deploy_server>'
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

  desc 'Update symlink to latest package'
  task 'symlink' do
    run_remote deploy_server,"cd #{deploy_to} && ./script/update_syms"
  end

  desc 'Setup <deploy_server>'
  task "setup" do
    run_remote deploy_server,"mkdir -p #{deploy_to}/snapshot"
    run_remote deploy_server,"mkdir -p #{deploy_to}/release"
    run_remote deploy_server,"mkdir -p #{deploy_to}/current"
    run_remote deploy_server,"mkdir -p #{deploy_to}/script"

    run_locally "scp config/update_syms #{deploy_server}:#{deploy_to}/script"
  end
end
