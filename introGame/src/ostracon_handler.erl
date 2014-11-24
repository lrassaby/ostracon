-module(ostracon_handler).
-behaviour(cowboy_http_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([handle/2]).
-export([terminate/3]).
-export([
    websocket_init/3, websocket_handle/3,
    websocket_info/3, websocket_terminate/3
]).

init({tcp, http}, _Req, _Opts) ->
  {upgrade, protocol, cowboy_websocket}.

% no 'get' requests should hit here
handle(_Req, State) ->
    {ok, Response} = cowboy_http_req:reply(404, [{'Content-Type', <<"text/html">>}]),
    {ok, Response, State}.

terminate(_Reason, _Req, _State) ->
	ok.


% websocket logic

websocket_init(_TransportName, Req, _Opts) ->
    {ok, Req, undefined_state}.

websocket_handle({text, JSON}, Req, State) ->
    Body = jiffy:decode(JSON),
    case Body of
        {[{<< "type" >>, << "vote" >>}, {<< "vote" >>, V}, {<< "team" >>, T}]} ->
            % TODO: pass in the team as well, and deal with it throughout the process
            ets:insert(voteDB, {self(), {V, T}}),
            Response = jiffy:encode({[{type, voteresponse}, {response, V}]}),
            {reply, {text, Response}, Req, State, hibernate };
        {[{<< "type" >>, << "statequery" >>}]} ->
            AppState = ets:tab2list(stateDB),
            Response = jiffy:encode({[{type, stateresponse}, {response, {AppState}}]}),
            {reply, {text, Response}, Req, State, hibernate };
        _ ->
            Response = jiffy:encode({[{type, error}, {error, << "Unrecognized query." >>}]}),
            {reply, {text, Response}, Req, State, hibernate }
    end;
websocket_handle(_, Req, State) ->
    {reply, {text, << "Bad Request" >>}, Req, State, hibernate }.

websocket_info({timeout, _Ref, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State, hibernate}.

websocket_terminate(_Reason, _Req, _State) ->
    ok.