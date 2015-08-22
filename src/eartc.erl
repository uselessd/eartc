-module('eartc').

%% API exports
-export([init_listing/2, trace/2, flush/1]).

-define(PREFIX, "/usr/local/bin/").
-define(EARTC_DIR, "/usr/share/eartc/").
-define(MAKE, os:find_executable("make")).

-define(ARTRUN, string:join([?PREFIX, "artrun"], "")).
-define(ARTC, string:join([?PREFIX, "artc"], "")).
-define(STRACE_ARTC, string:join([?PREFIX, "strace-artc"], "")).
-define(GENINIT, string:join([?PREFIX, "geninit"], "")).

%%====================================================================
%% API functions
%%====================================================================

init_listing(Program, Dirs) ->
   ListingDir = file:make_dir(lists:concat([?EARTC_DIR, Program])),
   Port = vrunfile(?GENINIT, Dirs, 
                  lists:concat(
                  [?EARTC_DIR, Program, "/", Program, ".init"])
                  ),
   ok.

trace(Program, Args) ->
   Port = cmdpp(?STRACE_ARTC ++ 
                  " -o" ++
                  lists:concat(
                  [" ", ?EARTC_DIR, filename:basename(Program), "/", 
                  filename:basename(Program), ".strace"]) ++
                  " " ++ string:join([Program, Args], " ")),
   ok.

compile(Trace) ->
   ok.

replay(Trace) ->
   ok.

flush(Dir) ->
   del_dir(filelib:wildcard(?EARTC_DIR ++ Dir)),
   ok;
flush(Dir) when Dir =:= everything -> %% {error, enoent}
   del_dir(lists:reverse(filelib:wildcard(?EARTC_DIR ++ "*" ++ "*"))),
   ok.

%%====================================================================
%% Internal functions
%% Some adapted from nerves-utils
%%====================================================================

del_dir(Dir) ->
   lists:foreach(fun(D) ->
                    ok = file:del_dir(D)
                 end, del_all_files([Dir], [])).
 
del_all_files([], EmptyDirs) ->
   EmptyDirs;
del_all_files([Dir | T], EmptyDirs) ->
   {ok, FilesInDir} = file:list_dir(Dir),
   {Files, Dirs} = lists:foldl(fun(F, {Fs, Ds}) ->
                                  Path = Dir ++ "/" ++ F,
                                  case filelib:is_dir(Path) of
                                     true ->
                                          {Fs, [Path | Ds]};
                                     false ->
                                          {[Path | Fs], Ds}
                                  end
                               end, {[],[]}, FilesInDir),
   lists:foreach(fun(F) ->
                         ok = file:delete(F)
                 end, Files),
   del_all_files(T ++ Dirs, [Dir | EmptyDirs]).

run(Executable) ->
    run(Executable, [], []).

run(Executable, Args) ->
    run(Executable, Args, []).

run(Executable, Args, Input) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         Port = open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout]),
         Port ! {self(), {command, Input}},
         loop_till_done(Port, <<>>)
    end.
    
runnoloop(Executable, Args) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout])
    end.

vrun(Executable, Args) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         Port = open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout]),
         verbose_loop_till_done(Port)
    end.
    
vrunfile(Executable, Args, File) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         Port = open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout]),
         verbose_loop_till_done_file(File, Port)
    end.

loop_till_done(Port, Data) ->
    receive
      {Port, {data, NewData}} ->
         BinaryNewData = list_to_binary(NewData),
         ConcatenatedData = <<Data/binary, BinaryNewData/binary>>,
         loop_till_done(Port, ConcatenatedData);
      {Port, {exit_status, 0}} ->
         {ok, Data};
      {Port, {exit_status, ExitStatus}} ->
         {error, ExitStatus}
    end.
    
verbose_loop_till_done_file(File, Port) ->
    receive
      {Port, {data, NewData}} ->
         file:write_file(File, NewData),
         verbose_loop_till_done_file(File, Port);
      {Port, {exit_status, 0}} ->
         ok;
      {Port, {exit_status, ExitStatus}} ->
         {error, ExitStatus}
    end.

verbose_loop_till_done(Port) ->
    receive
      {Port, {data, NewData}} ->
         lists:foreach(fun(A) -> io:format("~s~n", [A]) end,
            string:tokens(NewData, "\n")),
         verbose_loop_till_done(Port);
      {Port, {exit_status, 0}} ->
         ok;
      {Port, {exit_status, ExitStatus}} ->
         {error, ExitStatus}
    end.

cmdpp(CmdLine) ->
    Output = os:cmd(CmdLine),
    lists:foreach(fun(A) -> io:format("~s~n", [A]) end,
		  string:tokens(Output, "\n")),
    ok.
