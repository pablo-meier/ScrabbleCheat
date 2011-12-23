Why the hell is this is Ruby?
=============================

Because I tried writing it in C++, but the Thrift THttpClient code went to 
hell. Every time I tried connecting to the C++ HTTP Server, the client would
choke on its own vomit and throw an exception. I'm really thinking it was a
result of me passing in a & when it wanted a * but the "type" "system" of C++
not knowing any better, but hey, who can say. I searched around the source and
make a bunch of little tweaks, but I didn't want to have to deal with it 
anymore, when all I wanted to do was verify that the server was operating 
correctly.
