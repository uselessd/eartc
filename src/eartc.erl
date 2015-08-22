-module('eartc').

%% API exports
-export([init_listing/2, 
         trace/2, 
         compile/1, 
         replay/1, 
         flush/1]).

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
   file:make_dir(new_trace_dir(Program)),
   vrunfile(?GENINIT, Dirs, 
            init_listing_string(Program)),
   ok.

trace(Program, Args) ->
   cmdpp(?STRACE_ARTC ++ 
                  " -o" ++
                  strace_string(Program) ++
                  " " ++ string:join([Program, Args], " ")).

compile(Trace) ->
   cmdpp(
   string:join([?ARTC, "--strace", 
               strace_string(Trace), 
               init_listing_string(Trace),
               bench_dir_string(Trace)], " ")
   ),
   cmdpp(
   string:join([?MAKE, "-C", bench_dir_string(Trace)], " ")
   ).

replay(Trace) ->
   case filelib:is_file(bench_shared_object(Trace)) of
      true ->
         vrunfile(?ARTRUN, ["-d", "fds,paths,aio,threads", bench_shared_object(Trace),
                      new_trace_dir(Trace)],
                      new_replay_file(Trace)),
         ok;
      false ->
         io:fwrite("Warning: No ARTC-generated shared object found.~n", [])
   end.

flush(Dir) when Dir =/= [all] ->
   del_dir(filelib:wildcard(?EARTC_DIR ++ Dir)),
   ok;
flush(all) ->
   del_dir(lists:reverse(filelib:wildcard(?EARTC_DIR ++ "*" ++ "*"))),
   ok.

%%====================================================================
%% Internal functions
%% Some adapted from nerves-utils
%%====================================================================

init_listing_string(Program) ->
   M = lists:concat([?EARTC_DIR, filename:basename(Program), "/", 
                     filename:basename(Program), ".init"]),
   M.
   
strace_string(Program) ->
   N = lists:concat([" ", ?EARTC_DIR, filename:basename(Program), "/", 
                     filename:basename(Program), ".strace"]),
   N.
   
new_trace_dir(Program) ->
   O = lists:concat([?EARTC_DIR, filename:basename(Program)]),
   O.
   
new_replay_file(Program) ->
   string:join([?EARTC_DIR, Program, "/", Program, ".replay.",
                integer_to_list(os:system_time())], "").

bench_dir_string(Program) ->
   string:join([?EARTC_DIR, Program, "/", Program, ".bench.d"], "").
   
bench_shared_object(Program) ->
   string:join([?EARTC_DIR, Program, "/", Program, ".bench.d/", "bench.so"], "").

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
    
vrunfile(Executable, Args, File) ->
    case os:find_executable(Executable) of
      false ->
         exit(enoent);
      FoundExecutable ->
         Port = open_port({spawn_executable, FoundExecutable},
                  [exit_status, {args, Args}, stderr_to_stdout]),
         verbose_loop_till_done_file(File, Port)
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

cmdpp(CmdLine) ->
    Output = os:cmd(CmdLine),
    lists:foreach(fun(A) -> io:format("~s~n", [A]) end,
		  string:tokens(Output, "\n")),
    ok.
