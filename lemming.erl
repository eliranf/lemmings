-module(lemming).
-compile(export_all).
-include_lib("wx/include/wx.hrl").
-include("config.hrl").
-behaviour(gen_fsm).

%%========================================================================================%%
%% IMPLEMANTATION
%%========================================================================================%%

%-----------------------------------------------------------------------------------------%
% UTILS
%-----------------------------------------------------------------------------------------%

% Update Cell wrapper
%-----------------------------------------------------------------------------------------%
% Wait for lock - and spin lock if needed %
updateCellWrapper(Key, OldCell, NewCell) ->
	screenWrapper:updateCell(Key, OldCell, NewCell).

%-----------------------------------------------------------------------------------------%
% FSM
%-----------------------------------------------------------------------------------------%

% Create a new lemming
%-----------------------------------------------------------------------------------------%
% Returns the new Lemming FSM's PID %
create(X, Y, Tribe, Delay, ServersNumber, IsKing, StartingPointPid, KingPid) ->	
	StatsKey = screenWrapper:statsKeyByCell({X, Y}),
	screenWrapper:incCell(StatsKey, ?STATS_CREATED_IDX),
	screenWrapper:incCell(StatsKey, ?STATS_ALIVE_IDX),

	% Random seed %
	utils:randomSeed(),
	
	% Randomize direction %
	Rand = random:uniform(),
	if		
		(Rand > 0.5) -> Dir = ?DIR_RIGHT;
		true         -> Dir = ?DIR_LEFT
	end,

	if
		IsKing =:= false ->
			Weapon = random:uniform(3),		
			State = ?LEMM_STATE_FALLING;

		true             ->
			Weapon = inf,
			State = ?LEMM_STATE_KING_WAITING
	end,

	% Init data %
	LemmData =#lemmRec
		{x = X,
		 y = Y,		
		 tribe = Tribe,
	 	 direction = Dir,
		 delay = Delay,
		 state = State,
		 isKing = IsKing,
		 weapon = Weapon,
		 startingPointPid = StartingPointPid,
		 kingPid = KingPid,
		 serversNumber = ServersNumber},

	start_link(LemmData, KingPid, false). % Returns the new lemming's PID %
	
% Relocate existing lemming
%-----------------------------------------------------------------------------------------%
relocate(NewX, NewY, LemmData) ->
	start_link(LemmData#lemmRec{x = NewX, y = NewY, state = ?LEMM_STATE_WALKING}, LemmData#lemmRec.kingPid, true).

% Start a new FSM
%-----------------------------------------------------------------------------------------%
% Not king %
start_link(LemmData, KingPid, IsRelocation) ->
		gen_fsm:start(lemming, [LemmData, KingPid, IsRelocation], []).

% King is dead! Sent by monitor
%-----------------------------------------------------------------------------------------%
handle_info({'DOWN', _Ref, process, _Pid, _Reason}, _StateName, LemmData) ->
	%io:fwrite("King is dead!"),
	screenWrapper:removeCell({{LemmData#lemmRec.x, LemmData#lemmRec.y},{?CELL_LEMM, LemmData}}),
	{stop, {shutdown, destroyed}, LemmData}.
	%{next_state, StateName1, StateData1}.

%-----------------------------------------------------------------------------------------%
% CALLBACK FUNCTIONS
%-----------------------------------------------------------------------------------------%

% Init
%-----------------------------------------------------------------------------------------%
init([LemmData, KingPid, IsRelocation]) ->
	% Random seed %
	utils:randomSeed(),

	% Servers numnber %
	put(serversNumber, LemmData#lemmRec.serversNumber),

	if
		% King %
		KingPid =:= nil ->
			MonRef = nil;
		
		% Not a king - add monitor on a king %
		true ->
			MonRef = erlang:monitor(process, LemmData#lemmRec.kingPid)
	end,
	% When initilaized gets a new pid. Add monitor refference %
	NewLemmData = LemmData#lemmRec{fsmPid = self(), monitorRef = MonRef},
	screenWrapper:setCell({NewLemmData#lemmRec.x, NewLemmData#lemmRec.y}, {?CELL_LEMM, NewLemmData}),
	case IsRelocation of
		true  -> realeseKeys({NewLemmData#lemmRec.x, NewLemmData#lemmRec.y}, {NewLemmData#lemmRec.x-NewLemmData#lemmRec.direction, NewLemmData#lemmRec.y});
		false -> ok
	end,
	
	case LemmData#lemmRec.state of
		?LEMM_STATE_WALKING -> {ok, walking, NewLemmData, NewLemmData#lemmRec.delay};
		?LEMM_STATE_FALLING -> {ok, falling, NewLemmData, NewLemmData#lemmRec.delay};
		?LEMM_STATE_KING_WAITING -> {ok, kingWaiting, NewLemmData};
		_ -> io:fwrite("Lemming init error! state=~p~n", [NewLemmData#lemmRec.state])
	end.

% Terminate
%-----------------------------------------------------------------------------------------%
terminate({shutdown, destroyed}, _StateName, LemmData) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,	
	screenWrapper:setCell({X, Y},{?CELL_BOOM, 0}),
	utils:delay(?BOOM_DELAY),

	% Update statistics %
	StatsKey = screenWrapper:statsKeyByCell({X, Y}),
	screenWrapper:decCell(StatsKey, ?STATS_ALIVE_IDX),
	screenWrapper:incCell(StatsKey, ?STATS_DESTROYED_IDX),	
  screenWrapper:decCell(StatsKey, ?STATS_POINTS_IDX),
	if
			LemmData#lemmRec.isKing =:= true -> try LemmData#lemmRec.startingPointPid!kingDestroyed catch _Exp -> ok end;
			true                     -> ok
	end,
	screenWrapper:removeCell({{X, Y}, {?CELL_BOOM, 0}});

terminate({shutdown, saved}, _StateName, LemmData) ->
	utils:playSound("YIPPEE.WAV", ?IS_SOUND_ON),
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	screenWrapper:setCell({X, Y-1},{?CELL_SAVED, LemmData#lemmRec.tribe}),
	utils:delay(?BOOM_DELAY),

	% Update statistics %
	StatsKey = screenWrapper:statsKeyByCell({LemmData#lemmRec.x, LemmData#lemmRec.y}),	
	screenWrapper:decCell(StatsKey, ?STATS_ALIVE_IDX),
	screenWrapper:incCell(StatsKey, ?STATS_SAVED_IDX),
	screenWrapper:removeCell({{X, Y},{?CELL_LEMM, LemmData}}),
	if
			LemmData#lemmRec.isKing =:= true ->
				screenWrapper:incCell(StatsKey, ?STATS_POINTS_IDX, ?POINTS_KING),			
				TribeMembers = screenWrapper:getLemmsPids(LemmData#lemmRec.tribe) -- [LemmData#lemmRec.fsmPid],
				screenWrapper:demonitorTribe(LemmData#lemmRec.tribe),				
				try LemmData#lemmRec.startingPointPid!kingDestroyed catch _Exp2 -> ok end,
				lists:map(fun(Pid) -> try gen_fsm:send_event(Pid, kingSaved) catch _Class:_Reason -> io:fwrite("BUG!~n") end end, TribeMembers),
				utils:delay(10);		
				% Wait for surely all processes demonitored king %
			true ->
				screenWrapper:incCell(StatsKey, ?STATS_POINTS_IDX, ?POINTS_LEMM),
				ok
	end,
	screenWrapper:removeCell({{X, Y-1}, {?CELL_SAVED, LemmData#lemmRec.tribe}});

terminate(_Reason, _StateName, _LemmData) ->
	ok. % No special return type %

% Bomb - outside the FSM
%-----------------------------------------------------------------------------------------%
bomb(LemmData) ->
    gen_fsm:send_event(LemmData#lemmRec.fsmPid, bomb).

bombByPid(Pid) ->
	gen_fsm:send_event(Pid, bomb).

% Speedup - outside the FSM
%-----------------------------------------------------------------------------------------%
speedup(LemmData) ->
    gen_fsm:send_event(LemmData#lemmRec.fsmPid, speedupEvt).

%-----------------------------------------------------------------------------------------%
% LOCKS
%-----------------------------------------------------------------------------------------%
realeseKeys(Key1, Key2) ->
	screenWrapper:releaseKeys(Key1, Key2),
	lockDelay().

lockDelay() ->
	?LOCK_DELAY_BASE + random:uniform(?LOCK_DELAY_RAND).

%-----------------------------------------------------------------------------------------%
% STATES
%-----------------------------------------------------------------------------------------%	

% KING WAITING
%-----------------------------------------------------------------------------------------%
kingWaiting(startKing, LemmData) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	NewData = LemmData#lemmRec{x = X+1},	
	screenWrapper:moveCell({{X, Y}, {?CELL_LEMM, LemmData}}, {{X+1, Y}, {?CELL_LEMM, NewData}}),
	performFall(NewData);

kingWaiting(timeout, LemmData) ->
	{next_state, kingWaiting, LemmData};

kingWaiting(speedupEvt, LemmData) ->
	speedUpLemm(LemmData, kingWaiting);

kingWaiting(bomb, LemmData) ->
	screenWrapper:removeCell({{LemmData#lemmRec.x, LemmData#lemmRec.y},{?CELL_LEMM, LemmData}}),
  {stop, {shutdown, destroyed}, LemmData}.

% Start king - outside the FSM %
startKing(KingPid) ->
	try
		gen_fsm:send_event(KingPid, startKing)
	catch _Class:_Reason ->
		ok
	end.

% FIGHT
%-----------------------------------------------------------------------------------------%
performFight(ActiveLemmData, PassiveLemmData, Key1, Key2) ->
	Xa = ActiveLemmData#lemmRec.x,
	Ya = ActiveLemmData#lemmRec.y,
	
	Reply =
		try
			gen_fsm:sync_send_event(PassiveLemmData#lemmRec.fsmPid, {attack, ActiveLemmData#lemmRec.weapon}, infinity)
		catch _Class:_Reason ->
			winner
		end,	

	case Reply of
		tie    ->
			% Change direction %
			NewData =ActiveLemmData#lemmRec{state = ?LEMM_STATE_WALKING, direction = switchDirection(ActiveLemmData#lemmRec.direction), bricksLeft = 0},
			updateCellWrapper({Xa, Ya}, {?CELL_LEMM, ActiveLemmData}, {?CELL_LEMM, NewData}),
			realeseKeys(Key1, Key2),
			{next_state, walking, NewData, NewData#lemmRec.delay};	

		winner ->
			utils:playSound("SPLAT.WAV", ?IS_SOUND_ON),
			NewData = ActiveLemmData#lemmRec{state = ?LEMM_STATE_WALKING},
			updateCellWrapper({Xa, Ya}, {?CELL_LEMM, ActiveLemmData}, {?CELL_LEMM, NewData}),
			realeseKeys(Key1, Key2),
			{next_state, walking, NewData, NewData#lemmRec.delay};

		loser  ->
			utils:playSound("SPLAT.WAV", ?IS_SOUND_ON),
			screenWrapper:removeCell({{Xa, Ya},{?CELL_LEMM, ActiveLemmData}}),
			realeseKeys(Key1, Key2),
			{stop, {shutdown, destroyed}, ActiveLemmData}
	end.

performFightPassive({attack, AttackerWeapon}, _From, PassiveLemmData) ->
	{Reply, Result} = decideWinner(AttackerWeapon, PassiveLemmData#lemmRec.weapon),

	X = PassiveLemmData#lemmRec.x,
	Y = PassiveLemmData#lemmRec.y,

	case PassiveLemmData#lemmRec.state of
		?LEMM_STATE_STOPPING ->
			case Result of
				tie ->										
					{reply, Reply, stopping, PassiveLemmData};
	
				winner ->
					{reply, Reply, stopping, PassiveLemmData};

				loser  ->
					screenWrapper:removeCell({{X, Y},{?CELL_LEMM, PassiveLemmData}}),
					{stop, {shutdown, destroyed}, Reply, PassiveLemmData}
			end;

		_Other               ->
			case Result of
				tie ->
					% Change direction %			
					NewData = PassiveLemmData#lemmRec{state = ?LEMM_STATE_WALKING, direction = switchDirection(PassiveLemmData#lemmRec.direction), bricksLeft = 0},
					updateCellWrapper({X, Y}, {?CELL_LEMM, PassiveLemmData}, {?CELL_LEMM, NewData}),
					{reply, Reply, walking, NewData, 0};
	
				winner ->
					NewData = PassiveLemmData#lemmRec{state = ?LEMM_STATE_WALKING},
					updateCellWrapper({X, Y}, {?CELL_LEMM, PassiveLemmData}, {?CELL_LEMM, NewData}),
					{reply, Reply, walking, NewData, 0};

				loser  ->
					screenWrapper:removeCell({{X, Y},{?CELL_LEMM, PassiveLemmData}}),
					{stop, {shutdown, destroyed}, Reply, PassiveLemmData}
			end
	end.

decideWinner(X, X) -> {tie, tie};
decideWinner(inf, _) -> {winner, loser};
decideWinner(_, inf) -> {loser, winner};

decideWinner(2, 1) -> {winner, loser};
decideWinner(3, 2) -> {winner, loser};
decideWinner(1, 3) -> {winner, loser};

decideWinner(1, 2) -> {loser, winner};
decideWinner(2, 3) -> {loser, winner};
decideWinner(3, 1) -> {loser, winner}.

% WALKING
%-----------------------------------------------------------------------------------------%
performWalk(LemmData) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	NewData = LemmData#lemmRec{state = ?LEMM_STATE_WALKING, x = X+LemmData#lemmRec.direction, frame = switchFrame(LemmData)},
	Reply = screenWrapper:moveCell({{X, Y}, {?CELL_LEMM, LemmData}}, {{X+LemmData#lemmRec.direction, Y}, {?CELL_LEMM, NewData}}),	
	case Reply of
		destroy ->
			%io:fwrite("<B> MOVE NODES~n"),
			{stop, normal, LemmData};
		ok      ->
			realeseKeys({X, Y}, {X+LemmData#lemmRec.direction, Y}),
			{next_state, walking, NewData, LemmData#lemmRec.delay}
	end.

% Action to mouse clicks %
walking(setStateStopping, LemmData) ->
	performStop(LemmData);

walking(setStateDigging, LemmData) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	{Type, Data} = screenWrapper:getCell({X, Y+1}),
	if
		Type =:= ?CELL_GROUND ->
			utils:playSound("SCRAPE.WAV", ?IS_SOUND_ON),
			screenWrapper:removeCell({{X, Y+1},{?CELL_GROUND, Data}});
		true -> ok
	end,
	% New state = FALL %
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	NewData = LemmData#lemmRec{state = ?LEMM_STATE_FALLING},
	updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),
	{next_state, falling, NewData, 0};
	
walking(setStateBridging, LemmData) ->
		X = LemmData#lemmRec.x,
		Y = LemmData#lemmRec.y,
		NewData = LemmData#lemmRec{bricksLeft = ?BRICKS_AMOUNT},
		updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),
		{next_state, walking, NewData, 0};

% State method %
walking(timeout, LemmData) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,

	% Lock two cells, or wait for them to be available %
	% The release is in each function implementation   %
		
	Result = screenWrapper:acquireKeys({X, Y}, {X+LemmData#lemmRec.direction, Y}),

	case Result of
		false ->
			% Locked! %
			{next_state, walking, LemmData, lockDelay()};
	
		true ->
			% Got lock! %
			{Type1, _Data1} = screenWrapper:getCell({X, Y+1}),
	
			case Type1 of
				?CELL_NONE   ->
					% New state = FALL %			
					NewData = LemmData#lemmRec{state = ?LEMM_STATE_FALLING},
					updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),
					realeseKeys({X, Y}, {X+LemmData#lemmRec.direction, Y}),
					{next_state, falling, NewData, 0};

				?CELL_GROUND ->	
					% Check for bridge building %
					{Type3, _Data3} = screenWrapper:getCell({X+LemmData#lemmRec.direction, Y+1}),
					NewLemmData = buildBridge(Type3, {X+LemmData#lemmRec.direction, Y+1}, LemmData),			

					{Type2, Data2} = screenWrapper:getCell({X+LemmData#lemmRec.direction, Y}),
					case Type2 of
						?CELL_NONE   ->					
							performWalk(NewLemmData);
		
						?CELL_GROUND ->
							% Change direction %
							NewData =LemmData#lemmRec{state = ?LEMM_STATE_WALKING, direction = switchDirection(LemmData#lemmRec.direction), bricksLeft = 0},
							updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),
							realeseKeys({X, Y}, {X+LemmData#lemmRec.direction, Y}), % Realese by old direction!!! %
							{next_state, walking, NewData, 0};			

						?CELL_ENDING_POINT ->	
							screenWrapper:removeCell({{X, Y},{?CELL_LEMM, LemmData}}),
							realeseKeys({X, Y}, {X+LemmData#lemmRec.direction, Y}),
							{stop, {shutdown, saved}, LemmData};

						?CELL_LEMM ->				
							if
								% Different tribes %
								Data2#lemmRec.tribe =/= NewLemmData#lemmRec.tribe ->
									performFight(LemmData, Data2, {X, Y}, {X+LemmData#lemmRec.direction, Y});

								% Same tribes %
								true ->                                    
									case Data2#lemmRec.state of
										?LEMM_STATE_STOPPING ->
											% Change direction %
											NewData =LemmData#lemmRec{state = ?LEMM_STATE_WALKING, direction = switchDirection(LemmData#lemmRec.direction), bricksLeft = 0},
											updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),
											realeseKeys({X, Y}, {X+LemmData#lemmRec.direction, Y}), % Realese by old direction!!! %
											{next_state, walking, NewData, 0};
										_Else                -> performWalk(NewLemmData)
									end
							end;				

						_Else ->
							performWalk(NewLemmData)
					end;

				_Else ->
					performWalk(LemmData)
			end
		end;	

walking(speedupEvt, LemmData) ->
	speedUpLemm(LemmData, walking);

walking(bomb, LemmData) ->
	screenWrapper:removeCell({{LemmData#lemmRec.x, LemmData#lemmRec.y},{?CELL_LEMM, LemmData}}),
  {stop, {shutdown, destroyed}, LemmData};

walking(kingSaved, LemmData) ->
	screenWrapper:removeCell({{LemmData#lemmRec.x, LemmData#lemmRec.y},{?CELL_LEMM, LemmData}}),
	{stop, {shutdown, saved}, LemmData}.

walking({attack, AttackerWeapon}, From, PassiveLemmData) ->
	performFightPassive({attack, AttackerWeapon}, From, PassiveLemmData).

% Build Bridge %
buildBridge(?CELL_NONE, Cordinate, LemmData) when LemmData#lemmRec.bricksLeft > 0 ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	utils:playSound("CHINK.WAV", ?IS_SOUND_ON),
	screenWrapper:setCell(Cordinate, {?CELL_GROUND, ?BRIDGE}),
	NewData =	LemmData#lemmRec{bricksLeft = LemmData#lemmRec.bricksLeft - 1},
	updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),
	NewData;

buildBridge(_Type, _Cordinate, LemmData) when LemmData#lemmRec.bricksLeft < ?BRICKS_AMOUNT ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	NewData = LemmData#lemmRec{bricksLeft = 0},
	updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),	
	NewData;

buildBridge(_Type, _Cordinate, LemmData) ->
	LemmData.

% FALLING
%-----------------------------------------------------------------------------------------%
performFall(LemmData) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	NewData = LemmData#lemmRec{state = ?LEMM_STATE_FALLING, y = Y+1},
	screenWrapper:moveCell({{X, Y}, {?CELL_LEMM, LemmData}}, {{X, Y+1}, {?CELL_LEMM, NewData}}),
	realeseKeys({X, Y}, {X, Y+1}),
	{next_state, falling, NewData, LemmData#lemmRec.delay}.

falling(setStateDigging, LemmData) ->
	{next_state, falling, LemmData, 1};

falling(timeout, LemmData) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,

	% Lock two cells, or wait for them to be available %
	% The release is in each function implementation   %

	Result = screenWrapper:acquireKeys({X, Y}, {X, Y+1}),

	case Result of
		false ->
			% Locked! %
			{next_state, falling, LemmData, lockDelay()};

		true ->			
			{Type, Data} = screenWrapper:getCell({X, Y+1}),
			case Type of
				?CELL_NONE   ->
					performFall(LemmData);

				?CELL_GROUND ->
					% New state = WALKING %
					% Choose a random direction to walk in %
					Rand = random:uniform(),
					if
						(Rand > 0.5) ->					
							NewData =LemmData#lemmRec{state = ?LEMM_STATE_WALKING, direction = switchDirection(LemmData#lemmRec.direction), bricksLeft = 0};
						true         ->					
							NewData = LemmData#lemmRec{state = ?LEMM_STATE_WALKING, bricksLeft = 0}
					end,			
					updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),
					realeseKeys({X, Y}, {X, Y+1}),
					{next_state, walking, NewData, 0};			

				?CELL_OBSTACLE ->
					% Destroy lemming                 %
					% Send terminate event to its FSM %		
					screenWrapper:removeCell({{X, Y},{?CELL_LEMM, LemmData}}),	
					realeseKeys({X, Y}, {X, Y+1}),
					{stop, {shutdown, destroyed}, LemmData};

				?CELL_ENDING_POINT ->
					screenWrapper:removeCell({{X, Y},{?CELL_LEMM, LemmData}}),
					realeseKeys({X, Y}, {X, Y+1}),
					{stop, {shutdown, saved}, LemmData};

				?CELL_LEMM ->
					if
						% Different tribes %
						Data#lemmRec.tribe =/= LemmData#lemmRec.tribe ->
							performFight(LemmData, Data, {X, Y}, {X, Y+1});

						% Same tribes %
						true -> performFall(LemmData)							
					end;

				_Else ->
					performFall(LemmData)
				
			end
		end;

falling(speedupEvt, LemmData) ->
	speedUpLemm(LemmData, falling);

falling(bomb, LemmData) ->
	screenWrapper:removeCell({{LemmData#lemmRec.x, LemmData#lemmRec.y},{?CELL_LEMM, LemmData}}),
  {stop, {shutdown, destroyed}, LemmData};

falling(kingSaved, LemmData) ->
	screenWrapper:removeCell({{LemmData#lemmRec.x, LemmData#lemmRec.y},{?CELL_LEMM, LemmData}}),
	{stop, {shutdown, saved}, LemmData}.

falling({attack, AttackerWeapon}, From, PassiveLemmData) ->
	performFightPassive({attack, AttackerWeapon}, From, PassiveLemmData).

% STOPPING
%-----------------------------------------------------------------------------------------%
performStop(LemmData) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	NewData = LemmData#lemmRec{state = ?LEMM_STATE_STOPPING},
	updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewData}),
	realeseKeys({X, Y}, {X+LemmData#lemmRec.direction, Y}),
	{next_state, stopping, NewData}. % No timeout! No need %

% Not easy to exit stopping state... %
% We can reach here after fight      %
stopping(timeout, LemmData) ->
	{next_state, stopping, LemmData};

stopping(speedupEvt, LemmData) ->
	speedUpLemm(LemmData, stopping);

stopping(bomb, LemmData) ->
	screenWrapper:removeCell({{LemmData#lemmRec.x, LemmData#lemmRec.y},{?CELL_LEMM, LemmData}}),
  {stop, {shutdown, destroyed}, LemmData};

stopping(kingSaved, LemmData) ->
	screenWrapper:removeCell({{LemmData#lemmRec.x, LemmData#lemmRec.y},{?CELL_LEMM, LemmData}}),
	{stop, {shutdown, saved}, LemmData}.

stopping({attack, AttackerWeapon}, From, PassiveLemmData) ->
	performFightPassive({attack, AttackerWeapon}, From, PassiveLemmData).

%-----------------------------------------------------------------------------------------%
% AUX APIs
%-----------------------------------------------------------------------------------------%
switchDirection(Dir)  -> Dir * (-1).
switchFrame(LemmData) -> 1 - LemmData#lemmRec.frame.

% SpeedUp lemming
%-----------------------------------------------------------------------------------------%
speedUpLemm(LemmData, StateName) ->
	X = LemmData#lemmRec.x,
	Y = LemmData#lemmRec.y,
	case LemmData#lemmRec.delay of
		?MIN_LEMM_DELAY -> NewDelay = ?MAX_LEMM_DELAY;
		_Other          -> NewDelay = LemmData#lemmRec.delay - ?LEMM_DELAY_DELTA
	end,
	NewLemmData = LemmData#lemmRec{delay = NewDelay},
	updateCellWrapper({X, Y}, {?CELL_LEMM, LemmData}, {?CELL_LEMM, NewLemmData}),
	{next_state, StateName, NewLemmData, 0}.

%-----------------------------------------------------------------------------------------%
% OTHER CALLBACKS
%-----------------------------------------------------------------------------------------%

% Code change
%-----------------------------------------------------------------------------------------%
code_change(_OldVsn, StateName, StateData, _Extra) ->			
	{ok, StateName, StateData}.

% Handle async event
%-----------------------------------------------------------------------------------------%
handle_event(stop, _StateName, StateData) ->
    {stop, normal, StateData};

handle_event(_Event, StateName, StateData) ->
	{next_state,StateName, StateData, StateData#lemmRec.delay}.

handle_sync_event(Event, _From, _StateName, _StateData) ->
	io:fwrite("ERROR! Unhandled sync event:~p~n", [Event]).

%-----------------------------------------------------------------------------------------%

