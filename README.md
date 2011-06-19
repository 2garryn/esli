## **ESLI** is web-url shortener is written on Erlang ##

### Dependencies ###

Riak key-store

* [http://wiki.basho.com](http://wiki.basho.com "Riak Homepage")

* [https://github.com/basho/riak](https://github.com/basho/riak "Riak on github")

### Installation and using ###

1. Get it from github:
   
   `$ git clone https://github.com/2garryn/esli`
   
2. Go to _esli_ directory and make it:

   `$ make`

3. Before using _esli_ start your _Riak_ node.
4. When _Riak_ node is started you can start _esli_:

   `$ rel/slinode/bin/slinode start`
   
   `$ rel/slinode/bin/slinode ping`
   
   `pong`
   
5. To stop url-shortener:

   `$ rel/slinode/bin/slinode stop`
   
By default _esli_ server is listening 8081 port of http://localhost, i.e. to make short link you should open http://localhost:8081/ in your web-browser.
On real server you can set other port and host in etc/app.config in _slinode_ directory. Anyway, look at this file :-).
All data is written to _sl_ bucket in _Riak_.


Also, you can look at slinode/var/htdocs, there is html-pages, which user get.

### License ###

Copyright (c) 2011 Artem Golovinsky <artemgolovinsky@gmail.com>.

All rights reserved.

Redistribution and use in source and binary forms are permitted
provided that the above copyright notice and this paragraph are
duplicated in all such forms and that any documentation,
advertising materials, and other materials related to such
distribution and use acknowledge that the software was developed
by the <organization>.  The name of the
University may not be used to endorse or promote products derived
from this software without specific prior written permission.
THIS SOFTWARE IS PROVIDED ``AS IS'' AND WITHOUT ANY EXPRESS OR
IMPLIED WARRANTIES, INCLUDING, WITHOUT LIMITATION, THE IMPLIED
WARRANTIES OF MERCHANTIBILITY AND FITNESS FOR A PARTICULAR PURPOSE. 
    
