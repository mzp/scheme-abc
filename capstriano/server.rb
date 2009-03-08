require 'rubygems'
require 'sinatra'
require 'json'

post '/' do
  if JSON.parse(params[:payload])["repository"]["url"] == 'http://github.com/mzp/scheme-abc' then
    system('git pull && cap deploy:snapshot')
    system('cap deploy:statics')
  end
end
