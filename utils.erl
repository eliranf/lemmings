-module(utils).
-compile(export_all).
-include("config.hrl").

%%========================================================================================%%
%% IMPLEMANTATION
%%========================================================================================%%

% Play Sound
%-----------------------------------------------------------------------------------------%
playSound(_SoundString, _IsPlayOn) ->	
	%Cmd = "aplay Sound/" ++ SoundString,
	%os:cmd(Cmd).
	ok.

% Randomize seed
%-----------------------------------------------------------------------------------------%
randomSeed() ->	
	random:seed(erlang:now()).

% Delay
%-----------------------------------------------------------------------------------------%
delay(Msec) -> 
	%timer:sleep(Msec).
  receive
  after (Msec) -> ok
  end.

% DebugPrint
%-----------------------------------------------------------------------------------------%
debugPrint(_String, _List) -> ok.  %io:fwrite(String, List).

debugPrint(Module, String, List) ->
	debugPrint(Module, String, List, 0).

debugPrint(_, _String, _List, 0) -> ok; % OFF %
%debugPrint("displayHandler", String, List, 1) -> io:fwrite(String, List);
debugPrint("lemming", String, List, 1) -> io:fwrite(String, List);
debugPrint("error", String, List, 1) -> io:fwrite(String, List);
debugPrint(_, _String, _List, 1) -> ok.
