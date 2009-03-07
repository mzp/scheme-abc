require 'rubygems'
require 'sinatra'
require 'json'

get '/hi' do
  p JSON.parse('[]')
  "hello"
end

