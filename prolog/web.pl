:- module(web, [
    get/2
]).

% our own libraries
:- use_module(library(web/response), []).

% core libraries
:- use_module(library(error), [must_be/2]).
:- use_module(library(http/http_header), []). % support POST, PUT, etc. methods
:- use_module(library(http/http_open), [http_open/3]). % make HTTP responses
:- use_module(library(http/http_ssl_plugin), []). % support SSL
:- use_module(library(http/json), [json_read_dict/3]).  % support JSON
:- use_module(library(sgml), [load_structure/3]).  % support HTML parsing

:- redefine_system_predicate(get/2).

:- dynamic cacert_file/1.
cacert_file(File) :-
    absolute_file_name(library('../cacert-web.pem'), File, [access(read)]),
    retractall(cacert_file(_)),
    assert(cacert_file(File)),
    compile_predicates([cacert_file/1]).

% let third parties define views on HTTP content
:- multifile content_view/2.
content_view([],_).
content_view([View|Views],Response) :-
    content_view(View,Response),
    content_view(Views,Response).
content_view(codes(Codes),Response) :-
    response:body(Response,Body),
    read_stream_to_codes(Body,Codes).
content_view(html5(Dom),Response) :-
    response:body(Response,Body),
    load_structure(
        stream(Body),
        [Dom|_],
        [
            dialect(html5),
            shorttag(false),
            max_errors(-1),
            syntax_errors(quiet)
        ]
    ).
content_view(json(Dict),Response) :-
    response:content_type(Response,'application/json'),
    response:body(Response,Body),
    json_read_dict(Body,Dict,[tag('')]).
content_view(status_code(Code),Response) :-
    response:status_code(Response,Code).

%% get(+Url, -Response) is det.
%
%  True if an HTTP GET request to Url produces a Response.
%  Url can be an atom, string or list of codes.  If Response is
%  an unbound variable, it's unified with a value representing
%  the full HTTP response.  See library(response) in this pack
%  for predicates about this value.
%
%  Response may also be one of the following:
%
%    * `codes(Codes)` - returns response body as a list of codes
%    * `status_code(Code)` - unifies Code with the HTTP status code
%    * `html5(Dom)` - parses response body as HTML using load_structure/3
%    * `json(Dict)` - parses response body as JSON
%    * `[...]` - a list of the above; only one may parse the response body
get(UrlText,View) :-
    must_be(ground,UrlText),
    text_atom(UrlText,Url),
    get_(Url,Response),
    ( var(View) -> View=Response; content_view(View,Response) ).

get_(Url,Response) :-
    % make request
    cacert_file(CacertFile),
    Options = [
        method(get),
        header(content_type,ContentType),
        status_code(StatusCode),
        cacert_file(CacertFile)
    ],
    http_open(Url,Body,Options),

    % describe response value
    response:exists(Response, [
        status_code-StatusCode,
        content_type-ContentType,
        body-Body
    ]).


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
