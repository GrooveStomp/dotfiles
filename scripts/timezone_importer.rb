#!/usr/bin/env ruby
require 'json'
require 'csv'

offsets_json = File.read('/home/aaron/Downloads/timezone_offsets.json')
offsets = JSON.parse(offsets_json)

timezones = {}

File.foreach('/home/aaron/Downloads/timezones.tab') do |line|
  match_data = /^([A-Z]{2})\s+((?:\+|\-)\d+(?:\+|\-)\d+)\s+((?:\w|\/|\-)+)(\s+(.*))?\s*$/.match(line)

  if match_data
    code = match_data[1]
    coord = match_data[2]
    name = match_data[3]
    comment = match_data.length > 4 ? match_data[4].strip : ""

    timezones[name] = { code: code, coord: coord, name: name, comment: comment, offset: offsets[name] }
  end
end

csv = CSV.open('/home/aaron/Downloads/timezones.csv', 'wb') do |csv|
  timezones.keys.sort.each do |tz_name|
    tz = timezones[tz_name]
    csv << [ tz[:name], tz[:code], tz[:coord], tz[:comment], tz[:offset] ]
  end
end
