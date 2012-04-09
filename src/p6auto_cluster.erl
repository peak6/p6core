%% Copyright 2011 PEAK6 Investments, L.P.
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
-module(p6auto_cluster).
-behaviour(gen_server).

-include("logger.hrl").

-export([start_link/0,start_link/1]).
-export([addNode/1,delNode/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state,{tref,acNodes=[],nodes=[]}).

addNode(Node) -> call({add,Node}).
delNode(Node) -> call({del,Node}).

start_link() ->
    case application:get_env(cluster_nodes) of
        {ok,Nodes} when is_list(Nodes) -> start_link(Nodes);
        _ -> start_link([])
    end.

start_link(Nodes) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, Nodes, []).


init(Nodes) ->
    net_kernel:monitor_nodes(true),
    MyNodes = [ fixName(N) || N <- Nodes ],
    ?linfo("Initialized with autoConnect nodes: ~p",[MyNodes]),
    {ok,#state{acNodes=MyNodes},0}.

handle_call({add,Node},From,State) ->
    {Reply,NS} = addAC(fixName(Node),State),
    gen_server:reply(From,Reply),
    {noreply,checkAndConnect(NS)};

handle_call({del,Node},_From,State) ->
    {Reply,NewState} = delAC(Node,State),
    {reply,Reply,NewState};

handle_call(Request, From, State) ->
    ?linfo("Unexpected cast: ~p from: ~p, with state: ~p",[Request,From,State]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?linfo("Unexpected cast: ~p, with state: ~p",[Msg,State]),
    {noreply, State}.

handle_info({nodeup,Node},State) ->
    {Rep,NS} = addN(Node,State),
    case Rep of
        ok -> ok;
        Rep -> ?lwarn("Unexpected result of {nodeAdded,~p} : ~p",[Node,Rep])
    end,
    {noreply,NS};
handle_info({nodedown,Node},State) ->
    {Rep,NS} = delN(Node,State),
    case Rep of
        ok -> ok;
        Rep -> ?lwarn("Unexpected result of {nodeRemoved,~p} : ~p",[Node,Rep])
    end,
    {noreply,checkAndConnect(NS)};

handle_info(check,State) ->
    {noreply,checkAndConnect(State)};

handle_info(timeout,State=#state{tref=undefined}) ->  % initial check
    {noreply,checkAndConnect(State)};

handle_info(Info, State) ->
    ?linfo("Unexpected info: ~p, with state: ~p",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

call(Term) -> gen_server:call(?SERVER,Term).

checkAndConnect(State=#state{tref=undefined,acNodes=[]}) -> State;
checkAndConnect(State=#state{tref=TRef,acNodes=[]}) ->
    {ok,cancel} = timer:cancel(TRef),
    State;
checkAndConnect(State=#state{tref=TRef,acNodes=ACNodes,nodes=Nodes}) ->
    Attempted =
        lists:foldl(fun(N,CNodes)->
                            case lists:member(N,Nodes) of
                                false -> connect(N),[N|CNodes];
                                true -> CNodes
                            end
                    end, [], ACNodes),
    case {TRef,Attempted} of
        {undefined,[]} -> ?linfo("No timer, and nothing to do"), State;
        {undefined,N} -> {ok,Ref} = timer:send_interval(1000,check),
                         ?linfo("Auto-connect nodes ~p are down, starting reconnect timer",[N]),
                         State#state{tref=Ref};
        {Ref,[]} -> {ok,cancel} = timer:cancel(Ref),
                   ?linfo("All nodes connected, canceled check timer"),
                   State#state{tref=undefined};
        {_Ref,N} -> ?linfo("connect attempts made to: ~p",[N]), State
    end.

connect(Node) ->
    case net_kernel:connect(Node) of
        true -> ?linfo("Connected to: ~p",[Node]),
                ok;
        _ -> ?linfo("Failed to connect to: ~p",[Node]),
             error
    end.

addAC(Node,State) -> add(#state.acNodes,Node,State).
delAC(Node,State) -> del(#state.acNodes,Node,State).
addN(Node,State) -> add(#state.nodes,Node,State).
delN(Node,State) -> del(#state.nodes,Node,State).

add(Elem,Node,State) ->
    Nodes = element(Elem,State),
    case lists:member(Node,Nodes) of
        true -> {duplicate,State};
        false -> {ok,setelement(Elem,State,lists:sort([Node|Nodes]))}
    end.

del(Elem,Node,State) ->
    Nodes = element(Elem,State),
    case lists:member(Node,Nodes) of
        false -> {not_found,State};
        true -> {ok,setelement(Elem,State,lists:delete(Node,Nodes))}
    end.

fixName(Name) when is_atom(Name) -> fixName(atom_to_list(Name));
fixName(Name) when is_list(Name) ->
    case lists:member($@,Name) of
        true -> list_to_atom(Name);
        false -> list_to_atom(re:replace(
                                atom_to_list(node()),
                                "^.*(@.*)$",
                                Name++"\\1",
                                [{return,list}]))
    end.
