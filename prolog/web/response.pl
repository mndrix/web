:- module(response,[
    body/2,
    content_type/2,
    exists/1,
    exists/2,
    status_code/2,
    well_formed/1
]).


:- use_module(library(error), []).
:- use_module(library(record)).

:- multifile error:has_type/2.
error:has_type(response, Response) :-
    response:well_formed(Response).

:- record response(
    status_code:integer,
    content_type:atom,
    body:stream
).

%% exists(-Response:response) is det.
%
%  Unifies Response with an empty response term.
exists(Response) :-
    default_response(Response).

%% exists(-Response:response, +Attributes:list(pair)) is det.
%
%  Unifies Response with a fresh response term that
%  has the given named Attributes.
exists(Response,Attributes) :-
    maplist(term_pair,Terms,Attributes),
    make_response(Terms,Response).

term_pair(Term,Name-Value) :-
    Term =.. [Name, Value].


%% well_formed(+Response) is semidet.
%
%  True if Response is a proper response term.
well_formed(Response) :-
    is_response(Response).


%% body(+Response:response,-Stream:stream) is det.
%% body(?Response:response,+Stream:stream) is semidet.
%
%  True if Response body can be read from Stream.
body(Response, Stream) :-
    response_body(Response,Stream).

%% content_type(+Response:response,-ContentType:atom) is det.
%% content_type(?Response:response,+ContentType:atom) is semidet.
%
%  True if Response includes a Content-Type header whose value
%  is ContentType.
content_type(Response, Type) :-
    response_content_type(Response,Type).


%% status_code(+Response:response,-Code:integer) is det.
%% status_code(?Response:response,+Code:integer) is semidet.
%
%  True if Response has the HTTP status Code.
status_code(Response,Code) :-
    response_status_code(Response,Code).
