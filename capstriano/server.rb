require 'rubygems'
require 'sinatra'
require 'json'

post '/' do
  json = JSON.parse(params[:payload])
  if json["repository"]["url"] == 'http://github.com/mzp/scheme-abc' then
    ENV['REVISION'] = json['commits']['id']
    ENV['BRANCH']= File.basename json['ref']

    system('git pull && cap deploy:snapshot')
    system('cap deploy:statics')
  end
end
