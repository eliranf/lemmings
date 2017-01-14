-module(startingPoint).
-compile(export_all).
-include("config.hrl").

%%========================================================================================%%
%% IMPLEMANTATION
%%========================================================================================%%
% Create a new starting point
%-----------------------------------------------------------------------------------------%
create(X, Y, Tribe, ServersNumber, Difficulty) ->

	% Servers numnber %
	put(serversNumber, ServersNumber),
	S = self(),
	StartingData = #startingRec{x=X, y=Y, tribe = Tribe, serversNumber = ServersNumber, selfPid = S, difficulty = Difficulty, lemmsPerKing = ?LEMMS_PER_KING_MIN},
	screenWrapper:setCell({X, Y}, {?CELL_STARTING_POINT, StartingData}),
	%screenWrapper:addInstance({X, Y}, ?CELL_STARTING_POINT, self()),
	mainLoop(StartingData).

%-----------------------------------------------------------------------------------------%
% MAIN LOOP
%-----------------------------------------------------------------------------------------%
mainLoop(StartingPonitData) ->	
	%io:fwrite("Counter: ~p~n", [StartingPonitData#startingRec.startKingCounter]),
	receive
		speedupEvt            ->
			NewStartingPonitData = updateSpeedup(StartingPonitData),
			mainLoop(NewStartingPonitData);

		kingDestroyed         ->
			mainLoop(StartingPonitData#startingRec{startKingCounter = StartingPonitData#startingRec.lemmsPerKing, kingPid = nil});

		lemmsPerKingChangeEvt ->
			case StartingPonitData#startingRec.lemmsPerKing of
				?LEMMS_PER_KING_MIN ->
					NewLemmsPerKing     = ?LEMMS_PER_KING_UNLIMITED,					
					NewStartKingCounter = ?LEMMS_PER_KING_UNLIMITED;
				?LEMMS_PER_KING_UNLIMITED ->
					NewLemmsPerKing     = ?LEMMS_PER_KING_MIN,
					NewStartKingCounter = 0
			end,
			if
				StartingPonitData#startingRec.startKingCounter > 0 ->
					mainLoop(StartingPonitData#startingRec{lemmsPerKing = NewLemmsPerKing, startKingCounter = NewStartKingCounter});
				true                                               ->
					mainLoop(StartingPonitData#startingRec{lemmsPerKing = NewLemmsPerKing})
			end;

		terminate  -> ok

  after StartingPonitData#startingRec.delay * StartingPonitData#startingRec.difficulty ->
		% Create lemmings, with STARTING_POINT_DELAY between each creation %			
		KingPid = StartingPonitData#startingRec.kingPid,
		S = self(),
		if
			KingPid =:= nil ->
				% Create a king %				
				{ok, PID} = lemming:create(StartingPonitData#startingRec.x-1, StartingPonitData#startingRec.y, StartingPonitData#startingRec.tribe, StartingPonitData#startingRec.delay, StartingPonitData#startingRec.serversNumber, true, S, nil), 
				NewStartKingCounter = StartingPonitData#startingRec.lemmsPerKing;

			true ->
				if
					StartingPonitData#startingRec.startKingCounter =/= 0 ->
						% Create a regular lemming %
						lemming:create(StartingPonitData#startingRec.x, StartingPonitData#startingRec.y+1, StartingPonitData#startingRec.tribe, StartingPonitData#startingRec.delay, StartingPonitData#startingRec.serversNumber, false, S, StartingPonitData#startingRec.kingPid);
					true -> ok
				end,
				NewStartKingCounter = StartingPonitData#startingRec.startKingCounter-1,
				PID = StartingPonitData#startingRec.kingPid
		end,

		if
			StartingPonitData#startingRec.startKingCounter =:= 0 -> lemming:startKing(StartingPonitData#startingRec.kingPid);%StartingPonitData#startingRec.kingPid;
			true -> ok
		end,

		NewStartingPonitData = StartingPonitData#startingRec{startKingCounter = NewStartKingCounter, kingPid = PID},
		mainLoop(NewStartingPonitData)
  end.
	%utils:delay(?STARTING_POINT_DELAY),	

% Speedup
%-----------------------------------------------------------------------------------------%

% CALLBACK - CALLED OUTSIDE %
%speedup(StartingPonitData) ->
%	StartingPonitData#startingRec.selfPid!speedupEvt.

% HANDLERS %
updateSpeedup(StartingPonitData) when  StartingPonitData#startingRec.delay =:= ?MIN_LEMM_DELAY ->
	StartingPonitData#startingRec{delay = ?MAX_LEMM_DELAY};

updateSpeedup(StartingPonitData) ->
	StartingPonitData#startingRec{delay = StartingPonitData#startingRec.delay - ?LEMM_DELAY_DELTA}.

