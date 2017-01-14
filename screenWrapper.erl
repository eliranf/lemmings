-module(screenWrapper).

-behaviour(gen_server).

-include("config.hrl").
-include_lib("stdlib/include/qlc.hrl").
 
-export([start_link/2]).
-define(LAST_X_CELL, 10).
-define(LAST_Y_CELL, 10).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).
         
-export
	([setCell/2,
		getCell/1,
		getLock/1,
		acquireKeys/2,
		releaseKeys/2,
		getDisplayCell/1,
		moveCell/2,
		removeCell/1,
		updateCell/3,
		getScreen/0,
		getLemmsPids/0,
		getLemmsPids/1,
		demonitorTribe/1,
		getStartingPids/0,
		lemmsPerKingChange/0,
%		setFirstCell/0,
%		hasNextCell/0,
%		getNextCell/0,		
		incCell/2,
		incCell/3,
		decCell/2,
%		addInstance/3,
		speedUpLemmings/0,
		getAllStats/0,
		getServerIndexByCell/1,
		getServerNameByIndex/1,
		getServerNodeByIndex/1,
		getServerNameByCell/2,
		terminateServers/0,
		statsKeyByCell/1,
		statsKeyByServerIdx/1,
		createStartingPoint/3]). 

-record(server_opts,
	{ets,
	 locksEts,
	 %waitingQ,
   currCell,
	 serversNumber,
	 serverIdx}).

%%----------------------------------------------------------------------------------------%%
%% Handlers
%%----------------------------------------------------------------------------------------%%

setCell(Key,Value) ->	
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {setCell, {Key,Value}} ).

getCell(Key) -> 
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {getCell, Key}).

getLock(Key) -> 
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {getLock, Key}).

getDisplayCell(Key) -> 
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {getDisplayCell, Key}).		

moveCell(OldCell, NewCell) -> 
	ServersNumber = get(serversNumber),
	{OldKey, _} = OldCell,
	ServerName = getServerNameByCell(OldKey, ServersNumber),
	gen_server:call({global,ServerName}, {moveCell, {OldCell, NewCell}}, infinity).
	
updateCell(Key, OldVal, NewVal) -> 
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {updateCell, {Key, OldVal, NewVal}}).	
	
removeCell(Cell) -> 
	ServersNumber = get(serversNumber),
	{Key, _} = Cell,
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {removeCell, Cell}).
	
incCell(Key, ElemIndex) ->
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {incCell, {Key, ElemIndex}}).

incCell(Key, ElemIndex, Amount) ->
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {incCell, {Key, ElemIndex, Amount}}).
	
decCell(Key, ElemIndex) ->
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {decCell, {Key, ElemIndex}}).	
		
speedUpLemmings() ->
	LemmsList    = getLemmsPids(),
	StartingList = getStartingPids(),

	try
		lists:map(fun(Pid) -> gen_fsm:send_event(Pid, speedupEvt) end, LemmsList),
		lists:map(fun(Pid) -> Pid!speedupEvt end, StartingList )
	catch _Class:_Reason ->
		ok
	end.

lemmsPerKingChange() ->
	StartingList = getStartingPids(),
	lists:map(fun(Pid) -> Pid!lemmsPerKingChangeEvt end, StartingList).

createStartingPoint(Key, Tribe, Difficulty) ->
	ServersNumber = get(serversNumber),
	ServerName = getServerNameByCell(Key, ServersNumber),
	gen_server:call({global,ServerName}, {createStartingPoint, {Key, Tribe, ServersNumber, Difficulty}}).

getScreen() ->
	ServersNumber = get(serversNumber),
	getScreen(0, ServersNumber, []).
getScreen(ServersNumber, ServersNumber, Acc) -> Acc;
getScreen(I, ServersNumber, Acc) ->
	ServerName = getServerNameByIndex(I),
	List = gen_server:call({global,ServerName}, {getScreen,[]}),
	getScreen(I+1, ServersNumber, Acc ++ List).

getLemmsPids() ->
	ServersNumber = get(serversNumber),
	getLemmsPids(0, ServersNumber, []).
getLemmsPids(ServersNumber, ServersNumber, Acc) -> Acc;
getLemmsPids(I, ServersNumber, Acc) ->
	ServerName = getServerNameByIndex(I),
	List = gen_server:call({global,ServerName}, {getLemmsPids,[]}),
	getLemmsPids(I+1, ServersNumber, Acc ++ List).

getLemmsPids(Tribe) ->
	ServersNumber = get(serversNumber),
	getLemmsPids(0, ServersNumber, [], Tribe).
getLemmsPids(ServersNumber, ServersNumber, Acc, _) -> Acc;
getLemmsPids(I, ServersNumber, Acc, Tribe) ->
	ServerName = getServerNameByIndex(I),
	List = gen_server:call({global,ServerName}, {getLemmsPids,[Tribe]}),
	getLemmsPids(I+1, ServersNumber, Acc ++ List, Tribe).

demonitorTribe(Tribe) ->
	ServersNumber = get(serversNumber),
	demonitorTribe(0, ServersNumber, Tribe).
demonitorTribe(ServersNumber, ServersNumber, _) -> ok;
demonitorTribe(I, ServersNumber, Tribe) ->
	ServerName = getServerNameByIndex(I),
	gen_server:call({global,ServerName}, {demonitorTribe,[Tribe]}),
	demonitorTribe(I+1, ServersNumber, Tribe).	

getStartingPids() ->
	ServersNumber = get(serversNumber),
	getStartingPids(0, ServersNumber, []).
getStartingPids(ServersNumber, ServersNumber, Acc) -> Acc;
getStartingPids(I, ServersNumber, Acc) ->
	ServerName = getServerNameByIndex(I),
	List = gen_server:call({global,ServerName}, {getStartingPids,[]}),
	getStartingPids(I+1, ServersNumber, Acc ++ List).

terminateServers() ->
	ServersNumber = get(serversNumber),
	terminateServers(0, ServersNumber).

terminateServers(ServersNumber, ServersNumber) -> ok;
terminateServers(I, ServersNumber) ->
	io:fwrite("Terminating : ~p~n", [I]),
	ServerName = getServerNameByIndex(I),
	gen_server:call({global,ServerName}, {terminateServer,[]}),
	terminateServers(I+1, ServersNumber).

getAllStats() ->
	ServersNumber = get(serversNumber),
	getAllStats(0, ServersNumber, #statsRec{}).
getAllStats(ServersNumber, ServersNumber, StatsData) -> StatsData;
getAllStats(I, ServersNumber, StatsData) ->
	StatsKey = statsKeyByServerIdx(I),	
	{_, CurrStats} = getCell(StatsKey),
	AccStatsData = #statsRec{
		lemmCreated   = StatsData#statsRec.lemmCreated   + CurrStats#statsRec.lemmCreated,
		lemmAlive     = StatsData#statsRec.lemmAlive     + CurrStats#statsRec.lemmAlive,
		lemmDestroyed = StatsData#statsRec.lemmDestroyed + CurrStats#statsRec.lemmDestroyed,
		lemmSaved     = StatsData#statsRec.lemmSaved     + CurrStats#statsRec.lemmSaved,
		points        = StatsData#statsRec.points        + CurrStats#statsRec.points},
	getAllStats(I+1, ServersNumber, AccStatsData).

		
%%----------------------------------------------------------------------------------------%%	
%% LOCKS IMPLEMENTATION
%%----------------------------------------------------------------------------------------%%		

% ACQUIRE 
%-----------------------------------------------------------------------------------------%
acquireKeys({Kx1, Ky1}, {Kx2, Ky2}) ->
	if
		Kx1 < Kx2 ->
			KxL = Kx1, KyL = Ky1,
			KxR = Kx2, KyR = Ky2;
		true -> 
			KxL = Kx2, KyL = Ky2,
			KxR = Kx1, KyR = Ky1
	end,

	ServersNumber = get(serversNumber),

	% Syncing nodes transition %
	ServerNameOLXX = getServerNameByCell({KxL-1, KyL}, ServersNumber),
	ServerNameXOXX = getServerNameByCell({KxL,   KyL}, ServersNumber),
	ServerNameXLOX = getServerNameByCell({KxL+1, KyL}, ServersNumber),
	ServerNameXLXO = getServerNameByCell({KxL+2, KyL}, ServersNumber),
	case {ServerNameOLXX, ServerNameXOXX, ServerNameXLOX, ServerNameXLXO} of
		{X, X, X, Y} when X =/= Y -> NodesTrans = true;
		{X, X, Y, Y} when X =/= Y -> NodesTrans = true;
		{X, Y, Y, Y} when X =/= Y -> NodesTrans = true;
		{_Y, _Y, _Y, _Y}          -> NodesTrans = false
	end,

	case NodesTrans of
		true ->			
			SyncNodesKey = gen_server:call({global, ServerNameOLXX}, {acquireKeys, {[{{syncL, KyL}, lock}, {{syncR, KyR}, lock}]}}),
			case SyncNodesKey of
				true ->
					ServerName = getServerNameByCell({KxL,   KyL}, ServersNumber),
					Res = gen_server:call({global, ServerName}, {acquireKeys, {[{{KxL, KyL}, lock}, {{KxR, KyR}, lock}]}}),
					case Res of
						true  -> ReturnVal = true;
						false -> gen_server:call({global, ServerNameOLXX}, {releaseKeys, {[{syncL, KyL}, {syncR, KyR}]}}), ReturnVal = false
					end;
				false ->
					ReturnVal = false
			end;
		false ->
			ReturnVal = gen_server:call({global, ServerNameXLXO}, {acquireKeys, {[{{KxL, KyL}, lock}, {{KxR, KyR}, lock}]}})
	end,

	%case ReturnVal of
	%	true  -> ok;
	%	false -> queue:in(MonitorPid, Q1 :: queue(Item))
	%end,

	ReturnVal.

% RELEASE
%-----------------------------------------------------------------------------------------%
releaseKeys({Kx1, Ky1}, {Kx2, Ky2}) ->
	if
		Kx1 < Kx2 ->
			KxL = Kx1, KyL = Ky1,
			KxR = Kx2, KyR = Ky2;
		true -> 
			KxL = Kx2, KyL = Ky2,
			KxR = Kx1, KyR = Ky1
	end,

	ServersNumber = get(serversNumber),

	% Syncing nodes transition %
	ServerNameOLXX = getServerNameByCell({KxL-1, KyL}, ServersNumber),
	ServerNameXOXX = getServerNameByCell({KxL,   KyL}, ServersNumber),
	ServerNameXLOX = getServerNameByCell({KxL+1, KyL}, ServersNumber),
	ServerNameXLXO = getServerNameByCell({KxL+2, KyL}, ServersNumber),
	case {ServerNameOLXX, ServerNameXOXX, ServerNameXLOX, ServerNameXLXO} of
		{_X, _X, _X, _Y} -> NodesTrans = true;
		{_X, _X, _Y, _Y} -> NodesTrans = true;
		{_X, _Y, _Y, _Y} -> NodesTrans = true;
		{_Y, _Y, _Y, _Y} -> NodesTrans = false
	end,

	case NodesTrans of
		true ->
			ServerName = getServerNameByCell({KxL, KyL}, ServersNumber),			
			gen_server:call({global, ServerName}, {releaseKeys, {[{KxL, KyL}, {KxR, KyR}]}}),
			gen_server:call({global, ServerNameOLXX}, {releaseKeys, {[{syncL, KyL}, {syncR, KyR}]}});
		false ->			
			gen_server:call({global, ServerNameXLXO}, {releaseKeys, {[{KxL, KyL}, {KxR, KyR}]}})
	end,
	ok.

%%----------------------------------------------------------------------------------------%%	
%% Generic Server APIs
%%----------------------------------------------------------------------------------------%%		
start_link(ServerIdx, ServersNumber) ->
	ServerName = getServerNameByIndex(ServerIdx),
 	gen_server:start_link({global, ServerName}, ?MODULE, [ServersNumber, ServerIdx], []).
 
init([ServersNumber, ServerIdx]) ->
	utils:randomSeed(),
	% Servers Number %
	put(serversNumber, ServersNumber),	
	% Init ETS %
	TableScreenCells = ets:new(screenCells, [bag]),
	TableLocks       = ets:new(screenCells, [set]),	
	% Init Stats %
	StatsKey = statsKeyByServerIdx(ServerIdx),
	ets:insert(TableScreenCells, {StatsKey, {?CELL_LOCAL_STATS, #statsRec{points = ?POINTS_START div ServersNumber}}}),
	% Create state %	
	State = #server_opts{ets=TableScreenCells,currCell=ets:first(TableScreenCells), serversNumber = ServersNumber, serverIdx = ServerIdx, locksEts = TableLocks}, %waitingQ = WaitingQ},
  {ok, State}.
   
% ACQUIRE 
%-----------------------------------------------------------------------------------------%
handle_call({acquireKeys, {Keys}}, _From, State) ->
	Reply = ets:insert_new(State#server_opts.locksEts, Keys),
	{reply, Reply, State};

% RELEASE
%-----------------------------------------------------------------------------------------%
handle_call({releaseKeys, {Keys}}, _From, State) ->
	lists:map(fun(Key) -> ets:delete(State#server_opts.locksEts, Key) end, Keys),
	{reply, ok, State};

handle_call({setCell, {Key,Value}}, _From, State) ->	
  Response = ets:insert(State#server_opts.ets, {Key,Value}),
  {reply, Response, State};

handle_call({getDisplayCell, Key}, _From, State) ->
	Val = ets:lookup(State#server_opts.ets, Key),
	if  (Val =:= [] ) ->  Response = {?CELL_NONE, nil};
		 true -> [H|_] = Val, {_,Response} = H
	 end,
	{reply, Response, State};

handle_call({getLock, Key}, _From, State) ->
	Val = ets:lookup(State#server_opts.locksEts, Key),
	if
		Val =:= [] -> Response = false;
		true       -> Response = true
	end,
	{reply, Response, State};

handle_call({getCell, Key}, _From, State) ->
	Val = ets:lookup(State#server_opts.ets, Key),
	if 
		Val =:= []   ->  Response = {?CELL_NONE, nil};
		true ->
			ElemBoom  = [{Key2, {Type, Data}} || {Key2, {Type, Data}} <- Val, Type =:= ?CELL_BOOM ],
			ElemSaved = [{Key3, {Type, Data}} || {Key3, {Type, Data}} <- Val, Type =:= ?CELL_SAVED],
			List  = Val  -- ElemBoom,
			List2 = List -- ElemSaved,	
			if
				List2 =:= [] -> Response = {?CELL_NONE, nil};
				true         -> [{_,Response}|_] = List2
			end
	end,
	{reply, Response, State};

handle_call({moveCell, {OldCell, NewCell}}, _From, State) ->		
	{OldKey, {_, OldData}} = OldCell,
	{NewKey, {NewType, NewData}} = NewCell,
	OldServerIdx = getServerIndexByCell(OldKey),
	NewServerIdx = getServerIndexByCell(NewKey),
	if
		OldServerIdx =/= NewServerIdx, OldData#lemmRec.isKing =:= false  ->
			% Remove to a new node %			
			NewNodeName = getServerNodeByIndex(NewServerIdx),
			{NewX, NewY} = NewKey,			
			spawn(NewNodeName, lemming, relocate, [NewX, NewY, NewData]),
			%rpc:call(NewNodeName, lemming, relocate, [NewX, NewY, NewData]),		
			ets:delete_object(State#server_opts.ets, OldCell),	
			utils:debugPrint("lemming: ~p, move from server: ~p to ~p. \n", [OldData#lemmRec.fsmPid, OldServerIdx,NewServerIdx]),		
			Response = destroy;

		OldServerIdx =/= NewServerIdx, OldData#lemmRec.isKing =:= true  ->
			setCell(NewKey,{NewType, NewData}),
			ets:delete_object(State#server_opts.ets, OldCell),
			Response = ok;

		true ->			
			ets:insert(State#server_opts.ets, NewCell),
			ets:delete_object(State#server_opts.ets, OldCell),
			Response = ok
	end,
	{reply, Response, State};
	
handle_call({removeCell, Cell}, _From, State) ->	
	Response = ets:delete_object(State#server_opts.ets, Cell),
	{reply, Response, State};

handle_call({updateCell, {Key, OldVal, NewVal}}, _From, State) ->
	ets:delete_object(State#server_opts.ets, {Key,OldVal}),
	Response = ets:insert(State#server_opts.ets, {Key,NewVal}),
	{reply, Response, State};	

	
handle_call({incCell, {Key, ElemIndex}}, _From, State) ->	
	[{_,{Type, Data}}|_] = ets:lookup(State#server_opts.ets, Key),
	ets:delete(State#server_opts.ets, Key),
	NewData = setelement(ElemIndex, Data, element(ElemIndex,Data)+1),	
	Response =  ets:insert(State#server_opts.ets, {Key,{Type,NewData}}),
	{reply, Response, State};
	
handle_call({incCell, {Key, ElemIndex, Amount}}, _From, State) ->	
	[{_,{Type, Data}}|_] = ets:lookup(State#server_opts.ets, Key),
	ets:delete(State#server_opts.ets, Key),
	NewData = setelement(ElemIndex, Data, element(ElemIndex,Data)+Amount),	
	Response =  ets:insert(State#server_opts.ets, {Key,{Type,NewData}}),
	{reply, Response, State};

handle_call({decCell, {Key, ElemIndex}}, _From, State) ->	
	[{_,{Type, Data}}|_] =
		try
			ets:lookup(State#server_opts.ets, Key)
		catch _Exp ->
			[{nil,{nil, nil}}]
		end,
	ets:delete(State#server_opts.ets, Key),
	NewData = setelement(ElemIndex, Data, element(ElemIndex,Data)-1),
	Response = ets:insert(State#server_opts.ets, {Key,{Type,NewData}}),
	{reply, Response, State};	
	
handle_call({terminatt,[]}, _From, State) -> 
	ProcList = ets:tab2list(State#server_opts.ets),
	Response = lists:map(fun(Elem) -> checkAndTerminate(Elem) end, ProcList),
	{reply, Response, State};

handle_call({createStartingPoint, {{X, Y}, Tribe, ServersNumber, Difficulty}}, _From, State) ->	
	spawn(startingPoint, create, [X, Y, Tribe, ServersNumber, Difficulty]),
	{reply, ok, State};

handle_call({getLemmsPids, []}, _From, State) ->
	Q = qlc:q([Data#lemmRec.fsmPid || {_, {?CELL_LEMM, Data}} <- ets:table(State#server_opts.ets)]),
  Response = qlc:e(Q),
	{reply, Response, State};

handle_call({getLemmsPids, [Tribe]}, _From, State) ->
	Q = qlc:q([Data#lemmRec.fsmPid || {_, {?CELL_LEMM, Data}} <- ets:table(State#server_opts.ets), Data#lemmRec.tribe =:= Tribe]),
  Response = qlc:e(Q),
	{reply, Response, State};

handle_call({getScreen, []}, _From, State) ->
	Reply = ets:tab2list(State#server_opts.ets),
	{reply, Reply, State};

handle_call({demonitorTribe, [Tribe]}, _From, State) ->
	Q = qlc:q([Data#lemmRec.monitorRef || {_, {?CELL_LEMM, Data}} <- ets:table(State#server_opts.ets), Data#lemmRec.tribe =:= Tribe, Data#lemmRec.isKing =:= false]),
  RefsList = qlc:e(Q),
	lists:map(fun(Ref) -> erlang:demonitor(Ref) end, RefsList),
	{reply, ok, State};

handle_call({getStartingPids, []}, _From, State) ->
	Q = qlc:q([Data#startingRec.selfPid || {_, {?CELL_STARTING_POINT, Data}} <- ets:table(State#server_opts.ets)]),
  Response = qlc:e(Q),
	{reply, Response, State};

handle_call({terminateServer, []}, _From, State) ->
	{stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.
 
handle_info(_Info, State) ->
    {noreply, State}.
 
terminate(_Reason, _State) ->
    ok.
 
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%----------------------------------------------------------------------------------------%%	
%% Nodes Related
%%----------------------------------------------------------------------------------------%%	

%% Server name by index
%%----------------------------------------------------------------------------------------%%	
getServerNameByIndex(0) -> ?Server0;
getServerNameByIndex(1) -> ?Server1;
getServerNameByIndex(2) -> ?Server2;
getServerNameByIndex(3) -> ?Server3.

%% Server node by index
%%----------------------------------------------------------------------------------------%%	
getServerNodeByIndex(0) -> ?Server0Node;
getServerNodeByIndex(1) -> ?Server1Node;
getServerNodeByIndex(2) -> ?Server2Node;
getServerNodeByIndex(3) -> ?Server3Node.


%% Server index by cell
%%----------------------------------------------------------------------------------------%%	
getServerIndexByCell({X, _}) when X < 0 ->	0;
getServerIndexByCell({X, _}) ->
	ServersNumber = get(serversNumber),
	if
		X >= ?SCREEN_WIDTH -> ServersNumber-1;
		true ->
			Scaling = ?SCREEN_WIDTH div ServersNumber,
			(X div Scaling) rem ServersNumber
	end.

%% Server name by cell
%%----------------------------------------------------------------------------------------%%	
getServerNameByCell({X, _}, _ServersNumber) when X < 0              ->	getServerNameByIndex(0);
getServerNameByCell({X, _}, ServersNumber)  when X >= ?SCREEN_WIDTH ->	getServerNameByIndex(ServersNumber-1);
getServerNameByCell({X, _}, ServersNumber) ->	
	Scaling = ?SCREEN_WIDTH div ServersNumber,
	getServerNameByIndex((X div Scaling) rem ServersNumber).

%% Stats Key by cell
%%----------------------------------------------------------------------------------------%%	
statsKeyByCell({X, _}) ->
	ServersNumber = get(serversNumber),	
	Scaling = ?SCREEN_WIDTH div ServersNumber,
	ServerIdx = (X div Scaling) rem ServersNumber,
	{((ServerIdx * Scaling) + 2), ?SCREEN_HEIGHT-1}.

%% Stats Key by server index
%%----------------------------------------------------------------------------------------%%	
statsKeyByServerIdx(Idx) ->
	ServersNumber = get(serversNumber),	
	Scaling = ?SCREEN_WIDTH div ServersNumber,
	{((Idx * Scaling) + 2), ?SCREEN_HEIGHT-1}.
	
% TBD %
checkAndTerminate({_Key, {?CELL_LEMM, Data}}) ->
	exit(Data#lemmRec.fsmPid, kill);

checkAndTerminate({_Key, {?CELL_STARTING_POINT, Data}}) ->
	exit(Data#startingRec.selfPid, kill);

checkAndTerminate({_Key, {?CELL_ENDING_POINT, Data}}) ->
	exit(Data#endingRec.selfPid, kill).
	


