require 'capistrano/recipes/deploy/scm'
require 'capistrano/recipes/deploy/strategy'
require 'capistrano/recipes/deploy/dependencies'

def _cset(name, *args, &block)
  unless exists?(name)
    set(name, *args, &block)
  end
end

# =========================================================================
# These variables MUST be set in the client capfiles. If they are not set,
# the deploy will fail with an error.
# =========================================================================
_cset(:application) { abort "Please specify the name of your application, set :application, 'foo'" }
_cset(:repository)  { abort "Please specify the repository that houses your application's code, set :repository, 'foo'" }

_cset :scm, :subversion
_cset :deploy_via, :checkout

_cset(:build_path) { "build-path/#{application}" }
_cset(:revision)  { source.head }

_cset(:deploy_server) { abort "Please specify the server of deploy" }
_cset(:deploy_to) { "~/deploy/" }

# =========================================================================
# These variables should NOT be changed unless you are very confident in
# what you are doing. Make sure you understand all the implications of your
# changes if you do decide to muck with these!
# =========================================================================

_cset(:source)            { Capistrano::Deploy::SCM.new(scm, self) }
_cset(:real_revision)     { source.local.query_revision(revision) { |cmd| with_env("LC_ALL", "C") { run_locally(cmd) } } }

_cset(:strategy)          { Capistrano::Deploy::Strategy.new(deploy_via, self) }

_cset :current_dir,       "current"

_cset(:current_path)      { File.join(deploy_to, current_dir) }
_cset(:package_path)      { File.join(build_path, package_name) }

_cset(:run_method)        { fetch(:use_sudo, true) ? :sudo : :run }

# =========================================================================
# These are helper methods that will be available to your recipes.
# =========================================================================

# Auxiliary helper method for the `deploy:check' task. Lets you set up your
# own dependencies.
def depend(location, type, *args)
  deps = fetch(:dependencies, {})
  deps[location] ||= {}
  deps[location][type] ||= []
  deps[location][type] << args
  set :dependencies, deps
end

# Temporarily sets an environment variable, yields to a block, and restores
# the value when it is done.
def with_env(name, value)
  saved, ENV[name] = ENV[name], value
  yield
ensure
  ENV[name] = saved
end

def with_cd(dir,&cmd)
  "cd #{dir} && #{cmd.call()}"
end

# logs the command then executes it locally.
# returns the command output as a string
def run_locally(cmd)
  logger.trace "executing locally: #{cmd.inspect}" if logger
  `#{cmd}`
end

def run_remote(server,cmd)
  logger.trace "executing on #{server}: #{cmd.inspect}" if logger
  run "ssh #{server} '#{cmd}'"
end


# If a command is given, this will try to execute the given command, as
# described below. Otherwise, it will return a string for use in embedding in
# another command, for executing that command as described below.
#
# If :run_method is :sudo (or :use_sudo is true), this executes the given command
# via +sudo+. Otherwise is uses +run+. If :as is given as a key, it will be
# passed as the user to sudo as, if using sudo. If the :as key is not given,
# it will default to whatever the value of the :admin_runner variable is,
# which (by default) is unset.
#
# THUS, if you want to try to run something via sudo, and what to use the
# root user, you'd just to try_sudo('something'). If you wanted to try_sudo as
# someone else, you'd just do try_sudo('something', :as => "bob"). If you
# always wanted sudo to run as a particular user, you could do
# set(:admin_runner, "bob").
def try_sudo(*args)
  options = args.last.is_a?(Hash) ? args.pop : {}
  command = args.shift
  raise ArgumentError, "too many arguments" if args.any?

  as = options.fetch(:as, fetch(:admin_runner, nil))
  via = fetch(:run_method, :sudo)
  if command
    invoke_command(command, :via => via, :as => as)
  elsif via == :sudo
    sudo(:as => as)
  else
    ""
  end
end

# Same as sudo, but tries sudo with :as set to the value of the :runner
# variable (which defaults to "app").
def try_runner(*args)
  options = args.last.is_a?(Hash) ? args.pop : {}
  args << options.merge(:as => fetch(:runner, "app"))
  try_sudo(*args)
end

class Capistrano::Deploy::RemoteDependency
  def findlib(name,option={})
    @message ||= "findlib `#{name}' could not be found"
    findlib_cmd = configuration.fetch(:gem_command, "ocamlfind")
    try("#{findlib_cmd} query #{name}",option)
    self
  end

  def command(command, options={})
      @message ||= "`#{command}' could not be found in the path"
      try(%!test -e "$(which #{command})"!, options)
      self
  end
end

