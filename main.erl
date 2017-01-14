-module(main).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-include("config.hrl").

%%========================================================================================%%
%% IMPLEMANTATION
%%========================================================================================%%

%% Main program entry
%%----------------------------------------------------------------------------------------%%
start() ->
	% Servers number %
	ServersNumber = 1,
	put(serversNumber, ServersNumber),

	InitProcessPID = self(),

	% Open the display node %
	rpc:call(?DisplayNode, displayHandler, start, [InitProcessPID]),
	receive
		restart -> start();
		abort   -> ok
	end.
		
%%----------------------------------------------------------------------------------------%%
