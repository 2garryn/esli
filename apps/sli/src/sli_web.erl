%%% ---------------------------------------------------------------
%%% File    : sli_web.erl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description : "loop"-file for MochiWeb
%%% ---------------------------------------------------------------

-module(sli_web).

-include("sli.hrl").

-export([start/0, loop/2]).

-define(REQUEST_TIMEOUT, 3000).

start() ->
    DocRoot = sli_conf:get_config(docroot),
    ets:new(clients_ip, [duplicate_bag,
			 public,
			 named_table]),
    Loop = fun (Req) ->
                   ?MODULE:loop(Req,DocRoot)
	   end,
    WebConfig = sli_conf:get_config(web),
    mochiweb_http:start([{name, ?MODULE}, 
			 {loop, Loop} | WebConfig]).

loop(Req, DocRoot) ->
    "/" ++ Path = Req:get(path),

    try
	proceed_method(Path, Req, DocRoot)
    catch
        Type:What ->
            Report = ["web request failed",
                      {path, Path},
                      {type, Type}, {what, What},
                      {trace, erlang:get_stacktrace()}],
            error_logger:error_report(Report),
	    server_error(Req)

    end.

%%%
%%% functions to make "nice" and readable code
%%%
proceed_method(Path, Req, DocRoot) ->
    proceed_method2(Req:get(method), Path, Req, DocRoot).

proceed_method2('GET', Path, Req, DocRoot) ->
    proceed_get_path(Path, Req, DocRoot);

proceed_method2('HEAD', Path, Req, DocRoot) ->
    proceed_get_path(Path, Req, DocRoot);

proceed_method2('POST',Path, Req, DocRoot) ->
    proceed_post_path(Path, Req, DocRoot);

proceed_method2(_, _Path, Req, _DocRoot) -> 
    bad_request(Req). 

%%% Start short link proceeding
proceed_get_path("favicon.ico", Req, _DocRoot) ->
    not_found(Req);

proceed_get_path([], Req, DocRoot) ->
    index_file(Req, DocRoot);

proceed_get_path(Path, Req, DocRoot) ->
    case is_users_file(Path, DocRoot) of    
	true ->
	    proceed_file(Path, Req, DocRoot);
	false ->
	    proceed_short_link(Path, Req, DocRoot)
    end.

proceed_short_link(Path, Req, DocRoot) when length(Path) =:= ?MAX_LENGTH ->
    get_full_link(sli_checker:check_short(Path), Path, Req, DocRoot);

proceed_short_link(_Path, Req, DocRoot) ->
    not_found_file(Req, DocRoot).


get_full_link(true, Path, Req, DocRoot) ->
    try sli:get_full_link(Path) of
	{full_link, Fl} ->
	    Req:respond({301,[{"Location",Fl}], []});
	{error, 404} ->
	    not_found_file(Req, DocRoot);
	{error, 501} ->
	    server_error_file(Req, DocRoot)
    catch 
	_:_ ->
	    server_error_file(Req, DocRoot)
    end;

get_full_link(false, _Path, Req, DocRoot) ->
    not_found_file(Req, DocRoot).


proceed_file(Path, Req, DocRoot) ->
    Req:serve_file(Path, DocRoot).
%%% Stop short link proceeding. Full link is got


%%% Start full link proceeding
proceed_post_path("create", Req, DocRoot) -> 
    Ip = Req:get(peer),
    case ets:member(clients_ip, Ip) of
	true ->
	    Req:respond({423, [], ""});
	false ->
	    ets:insert(clients_ip, {Ip}),
	    timer:apply_after(?REQUEST_TIMEOUT, ets, delete, [clients_ip, Ip]),
	    Link = binary_to_list(Req:recv_body()),
	    get_short_link(sli_checker:check_and_update_full(Link), Req, DocRoot)
    end;

proceed_post_path(_, Req, _DocRoot) ->
    bad_request(Req).

get_short_link({true, UpdatedLink}, Req, _DocRoot) ->
    try sli:get_short_link(UpdatedLink) of
	{short_link, SLink} ->
	    Req:ok({"text/html",[], [sli_conf:get_config(domain) ++ "/" ++ SLink]});
	{error, 501} ->
	    server_error(Req)
    catch 
	_:_ ->
	    server_error(Req)
    end;
    
get_short_link(false, Req, _DocRoot) -> 
    bad_request(Req).

%%% Stop full link proceeding. short link is here
    
    
%%% Helpers for answer

index_file(Req, DocRoot) ->
    Req:serve_file("index.html", DocRoot).    

not_found_file(Req, DocRoot) ->
    Req:serve_file("not_found.html", DocRoot).

server_error_file(Req, DocRoot) ->   
    Req:serve_file("server_error.html", DocRoot). 
    
not_found(Req) ->
    Req:respond({404, [], ""}).

server_error(Req) ->
    Req:respond({501, [], "server_error"}).

bad_request(Req) ->
    Req:respond({400, [], "bad_request"}).
    
is_users_file(Path, DocRoot) ->
    filelib:is_file(filename:join(DocRoot, Path)).


    
