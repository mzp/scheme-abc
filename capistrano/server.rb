require 'rubygems'
require 'sinatra'
require 'json'

post '/' do
  json = JSON.parse(params[:payload])
  if json["repository"]["url"] == 'http://github.com/mzp/scheme-abc' then
    ENV['REVISION'] = json['after']
    ENV['BRANCH']= File.basename json['ref']
    system('twtr up -m "start snapshot build"')
    system('git pull && cap deploy:snapshot')
    system('cap deploy:statics')
    system('twtr up -m "end snapshot build"')
  end
end
