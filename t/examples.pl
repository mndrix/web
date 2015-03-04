:- use_module(library(web), []).

http('http://www.ndrix.com/hello.txt').
https('https://storage.googleapis.com/www.ndrix.com/hello.txt').

:- use_module(library(tap)).

% add tests showing common usage
http :-
    http(Url),
    web:get(Url,codes(Codes)),
    Codes == `Hello from the Internet`.

https :-
    https(Url),
    web:get(Url, codes(Codes)),
    Codes == `Hello from the Internet`.

json :-
    web:get('http://httpbin.org/ip', json(Dict)),
    nonvar(Dict),
    Dict = _{origin: Ip},
    string(Ip).
