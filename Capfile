# -*- mode:ruby -*-
Dir['vendor/plugins/*/recipes/*.rb'].each { |plugin| load(plugin) }
load 'capistrano/util.rb'
load 'capistrano/package.rb'
load 'capistrano/deploy.rb'
load 'capistrano/check.rb'

unless ENV['CAP_DEBUG'] then
  load 'capistrano/twitter-logger.rb'
end

set :application, "habc"
set :repository,  "git://github.com/mzp/scheme-abc.git"
set :scm, :git
set :git_shallow_clone, 1

set :deploy_server,"mzp.sakura.ne.jp"
set :deploy_to,"~/www/abc/deploy/"

if ENV['REVISION'] then
  set :revision,ENV['REVISION']
end

if ENV['BRANCH'] then
  set :branch,ENV['BRANCH']
else
  set :branch,"master"
end
set :omake,"omake --no--progress"

depend :remote, :command, "git"
depend :remote, :command, "ocaml"
depend :remote, :command, "omake"
depend :remote, :command, "tar"
depend :remote, :command, "zip"
depend :remote, :command, "avmplus"
depend :remote, :command, "readlink"
depend :remote, :command, "basename"
depend :remote, :command, "wget"

depend :remote, :findlib,"xml-light"
depend :remote, :findlib,"extlib"
depend :remote, :findlib,"oUnit"

depend :local, :command,"twtr"
depend :local, :command,"scp"

role :src, "localhost"
role :win, "itsuha.net",:port=>2222

set :build_path,"~/build-path"
set :test_path,"~/test-path"
