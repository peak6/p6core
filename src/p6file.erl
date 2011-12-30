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

-module(p6file).
-export([read_lines/2]).
-export([pathRead/2]).
-export([pathFindType/3]).
-export([readLink/1]).


-include_lib("kernel/include/file.hrl").

readLink(File) ->
    case file:read_link(File) of
        %% einval generated when the referenced file is there, but not a link
        %% so just return the original path
        {error,einval} -> {ok,File};
        Other -> Other
    end.

fileType(File) ->
    case file:read_file_info(File) of
        E={error,_} -> E;
        {ok,#file_info{type=regular}} -> file;
        {ok,#file_info{type=directory}} -> dir
    end.
    
pathFindType([],_Name,_Type) -> {error,enoent};
pathFindType([Dir|Rest],Name,Type) ->
    Check = Dir ++ "/" ++ Name,
    case fileType(Check) of
        T when T == Type -> {T,Check};
        _ -> pathFindType(Rest,Name,Type)
    end.

pathRead(Path,File) ->
    case pathFindType(Path,File,file) of
        {file,F} -> file:read_file(F);
        Other -> Other
    end.
   
read_lines(File,N) ->
    case file:open(File,[read]) of
        {ok,IO} ->
            readAndClose(IO,N);
        Other -> Other
    end.

finish(IODev,Acc) -> file:close(IODev),
                     lists:reverse(Acc).

readAndClose(IODev,N) -> readAndClose(IODev,N,[]).
readAndClose(IODev,0,Acc) -> finish(IODev,Acc);
readAndClose(IODev,N,Acc) ->
    case file:read_line(IODev) of
        {ok,Data} -> readAndClose(IODev,N-1,[Data|Acc]);
        eof -> finish(IODev,Acc);
        Other -> Other
    end.
                                   

            
