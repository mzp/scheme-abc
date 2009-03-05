# -*- mode:ruby -*-
# load 'deploy' if respond_to?(:namespace) # cap2 differentiator
Dir['vendor/plugins/*/recipes/*.rb'].each { |plugin| load(plugin) }
load 'config/aux.rb'
load 'config/package.rb'
load 'config/deploy.rb'
load 'config/twitter-logger.rb'

set :application, "habc"
set :repository,  "git://github.com/mzp/scheme-abc.git"
set :scm, :git
set :branch, "master"
set :git_shallow_clone, 1

set :deploy_server,"localhost"
set :deploy_to,"~/deploy/"

depend :remote, :command, "git"
depend :remote, :command, "ocaml"
depend :remote, :command, "omake"
depend :remote, :command, "tar"
depend :remote, :command, "zip"
depend :remote, :command, "avmplus"
depend :remote, :findlib,"xml-light"
depend :remote, :findlib,"extlib"
depend :remote, :findlib,"oUnit"
depend :local, :command,"twtr"
depend :local, :command,"scp"

role :mac, "localhost"

set :build_path,"~/build-path"


