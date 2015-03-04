# Synopsis

```prolog
:- use_module(library(web), []).

?- web:get('http://www.ndrix.com/hello.txt', codes(Text)).
Text = `Hello from the Internet`.

?- web:get('http://httpbin.org/ip', json(Dict)).
Dict = _{origin: "66.119.58.231"}.
```


# Description

This module helps you perform HTTP operations with as little code as possible.  This includes:

  * basic HTTP requests
  * helpful SSL defaults
  * convert HTTP responses from common data formats
    * JSON
    * HTML5

For more complex needs, use [http_open](http://www.swi-prolog.org/pldoc/man?section=httpopen) or [http_client](http://www.swi-prolog.org/pldoc/man?section=httpclient) directly.

# Installation

Using SWI-Prolog 7.1 or later:

    ?- pack_install(web).

This module uses [semantic versioning](http://semver.org/).

Source code available and pull requests accepted at
http://github.com/mndrix/web
