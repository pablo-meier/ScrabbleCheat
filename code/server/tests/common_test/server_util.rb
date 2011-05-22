#!/usr/bin/ruby
#
# Since starting and stopping the server takes so long (mostly due to loading
# the dictionary binaries), this script is run to ensure we only start and
# stop the server when absolutely necessary when doing development.
#
# This script works as follows:
#
#       server_util.rb OPTION 
#
#  where OPTION is...
#
#       started - Ensure that the server is running. Start it otherwise.
#       stopped - Ensure that the server is stopped. Stop it otherwise.


REL = "./rel/scrabblecheat"

puts %x[pwd]

action = nil
status = nil
msg = ""
case ARGV[0]
    when "started"
        action = :started
    when "stopped"
        action = :stopped
    else
        action = :started
end

status = (%x[#{REL}/bin/scrabblecheat ping]).chomp == "pong" ? :started : :stopped


if status == :stopped && action == :started
   puts "Need to start the server"
   %x[#{REL}/bin/scrabblecheat start]
   puts "Waiting 20 seconds while we load dictionaries..."
   sleep 20
   msg = "Started Server when previously stopped."
elsif status == :started && action == :stopped
    puts "Need to stop the server"
    %x[#{REL}/bin/scrabblecheat stop]
    msg = "Stopped the server when it was started."
else
    msg = "Server in desired state :-)"
end

puts msg

