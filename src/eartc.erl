-module('eartc').

%% API exports
-export([]).

-define(MAKE, os:find_executable("make")).
-define(ARTRUN, "/usr/local/bin/artrun").
-define(ARTC, "/usr/local/bin/artc").
-define(STRACE_ARTC, "/usr/local/bin/strace-artc").
-define(GENINIT, "/usr/local/bin/geninit").

%%====================================================================
%% API functions
%%====================================================================
trace(Program) ->
   ok.

compile(Trace) ->
   ok.

replay(Trace) ->
   ok.
   
init_listing(Dirs) ->
   ok.

flush(Trace) ->
   ok.

%%====================================================================
%% Internal functions
%% Some adapted from nerves-utils
%%====================================================================

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
