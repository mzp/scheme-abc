require 'rubygems'
require 'sinatra'
require 'json'

post '/' do
  p JSON.parse(params[:payload])
  "hello"
end
