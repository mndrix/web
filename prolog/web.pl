:- module(web, [
    get/2
]).

% our own libraries
:- use_module(library(web/response), []).

:- use_module(library(error), [must_be/2]).
:- use_module(library(http/http_client)), []). % parse HTTP responses
:- use_module(library(http/http_header), []). % support POST, PUT, etc. methods
:- use_module(library(http/http_ssl_plugin), []). % support SSL

:- redefine_system_predicate(get/2).

% let third parties define views on HTTP content
:- multifile content_view/3.

%% get(+Url, -Response) is det.
%
%  True if an HTTP GET request to Url produces a Response.
get(UrlText,View) :-
    must_be(UrlText,ground),
    text_atom(UrlText,Url),
    get_(Url,Response),
    once( header(Response,content_type,ContentType)
        ; ContentType = 'text/plain'
        ),
    content_view(View,ContentType,Response).

get_(Url,Response) :-
    Options = [
        method(get),
        status_code(StatusCode)
    ],
    setup_call_cleanup(
        http_open(Url,In,Options),
        todo(In),
        close(In)
    ),
    todo("create Response", Response).


%% text_atom(+Text:text,-Atom:atom) is det.
%
%  True if Text is represented as an Atom.  Text may be
%  a string, an atom or a code list.
text_atom(Text,Atom) :-
    atom(Text),
    !,
    Text = Atom.
text_atom(Text,Atom) :-
    string(Text),
    !,
    atom_string(Atom,Text).
text_atom(Text,Atom) :-
    is_list(Text),
    !,
    atom_codes(Atom,Text).
