var jsonServer = require('json-server');

// var thrift = require('thrift');
// var ScrabbleCheat = require('./ScrabbleCheat');
// var scTypes = require('./ScrabbleCheat_types');
// 
// var transport = thrift.TBufferedTransport();
// var protocol = thrift.TBinaryProtocol();
// 
// var connection = thrift.createConnection("localhost", 8888, {
//     transport : transport,
//     protocol : protocol
// });
// 
// connection.on('error', function(err) {
//   console.error('Error on Thrift connection:', err);
// });
// 
// var client = thrift.createClient(ScrabbleCheat, connection);
// 
// client.new_game(['Draskyl', 'EE'], 'WORDS_WITH_FRIENDS', 'SOWPODS', function(err, response) {
//   if (err) return console.error(err);
//   console.log('response is');
//   console.log(JSON.stringify(response));
// });

var server = jsonServer.create();
server.use(jsonServer.defaults());

var router = jsonServer.router('./server_src/db.json');
server.use(router);

console.log('Listening at 4000');
server.listen(4000);
