// Taken from the autogenerated skeleton file.

#include "ScrabbleCheat.h"
#include <protocol/TBinaryProtocol.h>
#include <server/TSimpleServer.h>
#include <transport/TServerSocket.h>
#include <transport/TBufferTransports.h>

#include <transport/TSocket.h>
#include <transport/TTransportUtils.h>
#include <protocol/TBinaryProtocol.h>

using namespace ::apache::thrift;
using namespace ::apache::thrift::protocol;
using namespace ::apache::thrift::transport;
using namespace ::apache::thrift::server;

using boost::shared_ptr;

class ScrabbleCheatHandler : virtual public ScrabbleCheatIf {

public:
	ScrabbleCheatHandler() {
		shared_ptr<TTransport> socket(new TSocket("localhost", 8888));
		shared_ptr<TTransport> transport(new TBufferedTransport(socket));
		shared_ptr<TProtocol> protocol(new TBinaryProtocol(transport));
		
		shared_ptr<ScrabbleCheatClient> client_initializer(new ScrabbleCheatClient(protocol));
		m_client = client_initializer;

		transport->open();
	}

	void new_game(Gamestate& _return, 
					const std::vector<std::string> & players, 
					const GameName::type game_name, 
					const Dictionary::type dict) {
		printf("new_game\n");
		// We just forward calls!
		m_client->new_game(_return, players, game_name, dict);
		printf("new_game successful!");
	}

	void game_info(GameInfo& _return, const GameName::type game_name) {
		printf("game_info\n");
		m_client->game_info(_return, game_name);
		printf("game_info successful!\n");
	}

	void play_move(Gamestate& _return, const std::vector<Tile> & tiles, const Gamestate& gamestate) {
		printf("play_move\n");
		m_client->play_move(_return, tiles, gamestate);
		printf("play_move successful\n");
	}
	
	void pass_turn(Gamestate& _return, const Gamestate& gamestate) {
		printf("pass_turn\n");
		m_client->pass_turn(_return, gamestate);
		printf("pass_turn returned!\n");
	}
	
	void get_scrabblecheat_suggestions(std::vector<Move> & _return, 
										const std::string& rack, 
										const Board& board, 
										const GameName::type game_name, 
										const Dictionary::type dict) {
		printf("get_scrabblecheat_suggestions\n");
		m_client->get_scrabblecheat_suggestions(_return, rack, board, game_name, dict);
		printf("get_scrabblecheat_suggestions successful!\n");
	}
	
	void quit() {
		printf("quit\n");
		m_client->quit();
		printf("quit returned\n");
	}
	
	private:
	shared_ptr<ScrabbleCheatClient> m_client;
};


// HTTP start code.
int main(int argc, char **argv) {
	int port = 9090;
	shared_ptr<ScrabbleCheatHandler> handler(new ScrabbleCheatHandler());
	shared_ptr<TProcessor> processor(new ScrabbleCheatProcessor(handler));
	shared_ptr<TServerTransport> serverTransport(new TServerSocket(port));
	shared_ptr<TTransportFactory> transportFactory(new TBufferedTransportFactory());
	shared_ptr<TProtocolFactory> protocolFactory(new TBinaryProtocolFactory());
	
	TSimpleServer server(processor, serverTransport, transportFactory, protocolFactory);
	server.serve();
	return 0;
}

