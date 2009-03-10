class Capistrano::Logger
  alias_method :__log__,:log
  def log(level, message, prefix,*args)
    __log__(level,message,prefix,*args)
    if level == IMPORTANT
      system "twtr update -m '[#{prefix}] #{message}' > /dev/null"
    end
  end
end

