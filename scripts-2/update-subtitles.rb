#!/usr/bin/env ruby

#require 'getoptlong'
require 'fileutils'
require 'byebug'

# -- Usage, parameter and option checking, misc setup --------------------------------------------------------
#opts = GetoptLong.new([ '--help', '-h', GetoptLong::NO_ARGUMENT ])

def usage
  puts <<-EOF
#{__method__} [option] time_offset subtitle_file

-h, --help:
   show help

subtitle_file: full path to .srt subtitle file
time_offset: number of seconds, positive or negative, and with optional decimal, to offset subtitles by
  EOF
  exit 0
end

# opts.each do |opt, arg|
#   case opt
#   when '--help'
#     usage
#   end
# end

usage if ARGV.length != 2

# -- Timing offset parsing -----------------------------------------------------------------------------------
begin
  offset = ARGV[0]
  if offset[0] == '-'
    negative = true
    offset = offset[1..-1]
  else
    negative = false
  end
  seconds, fraction = Float(offset).to_s.split('.')
  milliseconds = Integer(Float("0.#{fraction}") * 1000)
  adjustment = { seconds: seconds.to_i, milliseconds: milliseconds }
rescue
  usage
end

# -- File manipulation ---------------------------------------------------------------------------------------
file = ARGV[1]
usage unless File.extname(file) == '.srt'
dir = File.dirname(file)
FileUtils.cd(dir)
name = File.basename(file, '.srt')
FileUtils.cp("#{name}.srt", "#{name}.srt.backup")
out = File.open("#{name}.srt", "w+")

def increase_timing(timing, adjustment)
  hours, minutes, seconds, milliseconds = timing.split(/(:|,)/).delete_if { |i| i == ',' || i == ':' }.collect { |i| i.to_i }

  temp = milliseconds + adjustment[:milliseconds]
  carry = temp / 1000
  milliseconds = temp % 1000

  temp = carry + seconds + adjustment[:seconds]
  carry = temp / 60
  seconds = temp % 60

  temp = carry + minutes
  carry = temp / 60
  minutes = temp % 60

  hours = carry + hours

  sprintf('%02d:%02d:%02d,%03d', hours, minutes, seconds, milliseconds)
end

def decrease_timing(timing, adjustment)
  hours, minutes, seconds, milliseconds = timing.split(/(:|,)/).delete_if { |i| i == ',' || i == ':' }.collect { |i| i.to_i }

  temp = milliseconds - adjustment[:milliseconds]
  if temp < 0
    milliseconds = (1000 + milliseconds) - adjustment[:milliseconds]
    carry = 1
  else
    carry = 0
    milliseconds = temp
  end

  temp = seconds - carry - adjustment[:seconds]
  if temp < 0
    seconds = (60 + seconds) - carry - adjustment[:seconds]
    carry = 1
  else
    carry = 0
    seconds = temp
  end

  temp = minutes - carry
  if temp < 0
    minutes = (60 + minutes) - carry
    carry = 1
  else
    carry = 0
    minutes = temp
  end

  hours = hours - carry

  sprintf('%02d:%02d:%02d,%03d', hours, minutes, seconds, milliseconds)
end

# The file looks like this:
#
# 1
# 00:00:10,043 --> 00:00:11,243
#     What to do when a bunch
#   of data you want to destroy
#
File.foreach("#{name}.srt.backup") do |line|
  if match = line.match(/^\s*(?<start>\d{2}:\d{2}:\d{2},\d+)\s+-->\s+(?<end>\d{2}:\d{2}:\d{2},\d+)\s*$/)
    timings = [match[:start], match[:end]].collect do |timing|
      if negative
        decrease_timing(timing, adjustment)
      else
        increase_timing(timing, adjustment)
      end
    end

    out.write("#{timings.first} --> #{timings.last}\n")
  else
    out.write(line)
  end
end
