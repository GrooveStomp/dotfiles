require 'irb/ext/save-history'
IRB.conf[:SAVE_HISTORY] = 200
IRB.conf[:HISTORY_FILE] = "#{ENV['HOME']}/.irb-history"

IRB.conf[:AUTO_INDENT] = true
<<-'FOO'
begin
  require "pry"
  Pry.start
  ActiveRecord::Base.logger = Logger.new(STDOUT) if Object.const_defined?('ActiveRecord')
  exit
rescue LoadError => e
  warn "=> Unable to load pry: #{e}"
end
FOO
