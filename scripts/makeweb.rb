#! /opt/local/bin/ruby -w
# -*- mode:ruby; coding:utf-8 -*-
require 'erb'
require 'pp'

Result = Struct.new 'Result',:entry,:ok

def sep_by_dot(log)
  File.open(log).map do|line|
    entry,value=line.split '...',2
    if entry != nil and value != nil
      Result.new(entry.strip,value.strip.downcase == 'ok')
    else
      []
    end
  end.flatten
end

def summary(xs)
  xs.all?{|x| x.ok}
end

def b(x)
  if x then "OK" else "FAIL" end
end

def cell(entry,b)
  %(<tr class="#{b ? 'ok' : 'fail'}">
      <td>#{entry}</td>
      <td>#{b ? 'OK' : 'FAIL'}</td>
    </tr>)

end

content = File.open('template.erb').read
@build     = File.read('build.log')
@unit_test = sep_by_dot('unittest.log')
@integrate_test = sep_by_dot('integrate.log')
@install_test = sep_by_dot('install.log')
ERB.new(content).run
