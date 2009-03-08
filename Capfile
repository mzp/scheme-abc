# -*- mode:ruby -*-
Dir['vendor/plugins/*/recipes/*.rb'].each { |plugin| load(plugin) }
load 'capstriano/aux.rb'
load 'capstriano/package.rb'
load 'capstriano/deploy.rb'
load 'capstriano/twitter-logger.rb'

set :application, "habc"
set :repository,  "git://github.com/mzp/scheme-abc.git"
set :scm, :git
set :branch, "master"
set :git_shallow_clone, 1

set :deploy_server,"mzp.sakura.ne.jp"
set :deploy_to,"~/www/abc/deploy/"

depend :remote, :command, "git"
depend :remote, :command, "ocaml"
depend :remote, :command, "omake"
depend :remote, :command, "tar"
depend :remote, :command, "zip"
depend :remote, :command, "avmplus"
depend :remote, :command, "readlink"
depend :remote, :command, "basename"

depend :remote, :findlib,"xml-light"
depend :remote, :findlib,"extlib"
depend :remote, :findlib,"oUnit"

depend :local, :command,"twtr"
depend :local, :command,"scp"

role :src, "localhost"

set :build_path,"~/build-path"
