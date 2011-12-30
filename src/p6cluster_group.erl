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
-module(p6cluster_group).
-behaviour(gen_server).
-include("logger.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------
-export([start_link/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% gen_server State
-record(state,{name,owner}).

start_link(Name,Owner) when is_pid(Owner) ->
    gen_server:start_link({local,Name}, ?MODULE, [Name,Owner], []).
    
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
init([Name,Owner]) ->
    net_kernel:monitor_nodes(true),
    castNodes(nodes(),Name,{ping,self(),Owner}),
    {ok, #state{name=Name,owner=Owner}}.

handle_call(Request, From, State) ->
    ?linfo("Unexpected cast: ~p from: ~p, with state: ~p",[Request,From,State]),
    {reply, ok, State}.

handle_cast({ping,RemoteCluster,RemoteOwner}, State=#state{owner=Owner}) ->
    case docall(Owner,{clusterAdd,RemoteOwner}) of
        new -> docast(RemoteCluster,{ping,self(),Owner});
        {exists,_} -> ok;
        Other -> 
            ?linfo("Unexpected response to clusterAdd, owner: ~p, remote: ~p, result: ~p",
                   [Owner,RemoteOwner,Other ])
    end,
    {noreply,State};
                  
handle_cast(Msg, State) ->
    ?linfo("Unexpected cast: ~p, with state: ~p",[Msg,State]),
    {noreply, State}.

handle_info({nodedown,_Node},State) -> 
    {noreply,State}; 

handle_info({nodeup,Node},State=#state{name=Name,owner=O}) ->
    castNode(Node,Name,{ping,self(),O}),
    {noreply,State};

handle_info(Info, State) -> 
    ?linfo("Unexpected info: ~p, with state: ~p",[Info,State]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
castNodes(Nodes,Name,Term) -> lists:foreach(fun(N)->castNode(N,Name,Term) end, Nodes).
castNode(Node,Name,Term) -> gen_server:cast({Name,Node},Term).
docast(Pid,Term) -> gen_server:cast(Pid,Term).
docall(Pid,Term) -> gen_server:call(Pid,Term).
