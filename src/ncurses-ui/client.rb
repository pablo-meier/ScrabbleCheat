
require 'welcome_screen.rb'
require 'connector.rb'

DEFAULT_SOCKET_CONNECTION = "lololol"


address = DEFAULT_SOCKET_CONNECTION

unless ARGV[0].nil?
    address = ARGV[0]
end

# Open a new socket and connect to the ScrabbleCheat server.
unless connect_to_server
    # If it fails, gracefully exit
    $stderr.puts "Unable to connect to host #{address}, please try again."
    Process.exit
end


# If it succeeds, present a welcome screen.
present_welcome_screen
