-module(displayHandler).
-compile(export_all).
-include("config.hrl").
-include_lib("wx/include/wx.hrl").
-behaviour(wx_object).

-define(LBL_CREATED,   0).
-define(LBL_ALIVE,     1).
-define(LBL_DESTROYED, 2).
-define(LBL_SAVED,     3).
-define(LBL_POINTS,    4).

-define(SCREEN_MENU_NODES,      0).
-define(SCREEN_MENU_DIFFICULTY, 1).
-define(SCREEN_MENU_LEVEL,      2).
-define(SCREEN_LEVEL,           3).

% Display data %
-record(displayRec,
	{frame,
	 serversNumber,
	 difficulty,
	 levelStarted    = false,
	 restart         = false,
	 selectedButtonX = ?buttonsStartX,
	 selectedButtonY = ?buttonsStartY,
	 selectedButton  = ?BUTT_BRIDGE,
	 activeScreen    = ?SCREEN_MENU_NODES,	 
	 initProcessPID}).

% Options buttons %
-define(BUTT_OPTION_W, 273).
-define(BUTT_OPTION_H, 153).
-define(BUTT_OPTION1_X, 60).
-define(BUTT_OPTION1_Y, 432).
-define(BUTT_OPTION2_X, 375).
-define(BUTT_OPTION2_Y, 432).
-define(BUTT_OPTION3_X, 60).
-define(BUTT_OPTION3_Y, 597).
-define(BUTT_OPTION4_X, 375).
-define(BUTT_OPTION4_Y, 597).

%%========================================================================================%%
%% IMPLEMANTATION
%%========================================================================================%%

%-----------------------------------------------------------------------------------------%
% START
%-----------------------------------------------------------------------------------------%
start(InitProcessPID) ->
	wx_object:start({global, ?DisplayNode}, displayHandler, [InitProcessPID], []).

% Init
%-----------------------------------------------------------------------------------------%
init([InitProcessPID]) ->
		
	% Init the GUI %
	Wx = wx:new(),
	Frame = wxFrame:new(Wx, -1, "Lemmings - Adir & Eliran", [{size,{?FRAME_WIDTH_PXLS, ?FRAME_HEIGHT_PXLS}}]),	
	wxWindow:setBackgroundColour(Frame, ?wxDARK_BLUE),

	% Center and show frame %
	wxFrame:center(Frame),
  wxFrame:show(Frame),

	State = #displayRec{frame = Frame, initProcessPID = InitProcessPID},
	
	displayMenu(Frame, ?SCREEN_MENU_NODES),

	% Register callbacks and interrupts %	
	wxFrame:connect(Frame, left_down),  % Mouse left click %
	wxFrame:connect(Frame, right_down), % Mouse right click %

	{Frame, State}.

% Start level
%-----------------------------------------------------------------------------------------%
startLevel(State, LevelNum) ->
	% Servers number %
	ServersNumber = State#displayRec.serversNumber,
	put(serversNumber, State#displayRec.serversNumber),

	Frame = State#displayRec.frame,

	wxFrame:refresh(Frame),

	% Callback when screen is refreshed %
	OnPaint =
		fun(_Evt, _Obj) ->			
			put(serversNumber, ServersNumber),
			updateBottomDisplay(Frame),
			refreshDisplay(Frame)
		end,

	showBottomDisplay(Frame),

	% Build level %
	buildLevel(ServersNumber, LevelNum, State#displayRec.difficulty),

	io:fwrite("<INIT FINISH> LEVEL IS NOW RUNNING~n"),

	% Register callbacks and interrupts %	
	wxFrame:connect(Frame, paint, [{callback, OnPaint}]),	% Refresh screen %	
				
	{noreply,State,?DISPLAY_DELAY}.


% Menus
%-----------------------------------------------------------------------------------------%
displayMenu(Frame, ?SCREEN_MENU_NODES) ->
	putImage(0, 0, "Menu/MenuBackground.png", Frame),
	putImage(40, 285, "Menu/ChooseNodesButt.png", Frame),

	putImage(?BUTT_OPTION1_X, ?BUTT_OPTION1_Y, "Menu/node1.png", Frame),
	putImage(?BUTT_OPTION2_X, ?BUTT_OPTION2_Y, "Menu/node2.png", Frame),
	putImage(?BUTT_OPTION3_X, ?BUTT_OPTION3_Y, "Menu/node4.png", Frame);

displayMenu(Frame, ?SCREEN_MENU_DIFFICULTY) ->
	wxFrame:refresh(Frame),
	putImage(0, 0, "Menu/MenuBackground.png", Frame),
	putImage(40, 285, "Menu/ChooseDiffButt.png", Frame),

	putImage(?BUTT_OPTION1_X, ?BUTT_OPTION1_Y, "Menu/Diff1.png", Frame),
	putImage(?BUTT_OPTION2_X, ?BUTT_OPTION2_Y, "Menu/Diff2.png", Frame),
	putImage(?BUTT_OPTION3_X, ?BUTT_OPTION3_Y, "Menu/Diff3.png", Frame),
	putImage(?BUTT_OPTION4_X, ?BUTT_OPTION4_Y, "Menu/Diff4.png", Frame);

displayMenu(Frame, ?SCREEN_MENU_LEVEL) ->
	wxFrame:refresh(Frame),
	putImage(0, 0, "Menu/MenuBackground.png", Frame),
	putImage(40, 285, "Menu/ChooseLevelButt.png", Frame),

	putImage(?BUTT_OPTION1_X, ?BUTT_OPTION1_Y, "Menu/Lvl1.png", Frame),
	putImage(?BUTT_OPTION2_X, ?BUTT_OPTION2_Y, "Menu/Lvl2.png", Frame),
	putImage(?BUTT_OPTION3_X, ?BUTT_OPTION3_Y, "Menu/Lvl3.png", Frame),
	putImage(?BUTT_OPTION4_X, ?BUTT_OPTION4_Y, "Menu/Lvl4.png", Frame).

% Displays
%-----------------------------------------------------------------------------------------%
showBottomDisplay(Frame) ->

	% Put selected button %
	screenWrapper:setCell({?buttonsStartX div ?CELL_W, ?buttonsStartY div ?CELL_H}, {?CELL_BUTTON, 0}),

	% Create stats lables %
	Lbl_created = wxTextCtrl:new(Frame, ?LBL_CREATED, [{pos, {?STATS_START_X_PXLS, ?STATS_START_Y_PXLS}}, {size, {?LBL_W, ?LBL_H}}, {style, ?wxTE_READONLY}]),
	wxTextCtrl:changeValue(Lbl_created, "0"),

	Lbl_alive = wxTextCtrl:new(Frame, ?LBL_ALIVE, [{pos, {?STATS_START_X_PXLS+?LBL_DIST_X, ?STATS_START_Y_PXLS}}, {size, {?LBL_W, ?LBL_H}}, {style, ?wxTE_READONLY}]),
	wxTextCtrl:changeValue(Lbl_alive, "0"),

	Lbl_destroyed = wxTextCtrl:new(Frame, ?LBL_DESTROYED, [{pos, {?STATS_START_X_PXLS+2*?LBL_DIST_X, ?STATS_START_Y_PXLS}}, {size, {?LBL_W, ?LBL_H}}, {style, ?wxTE_READONLY}]),
	wxTextCtrl:changeValue(Lbl_destroyed, "0"),

	Lbl_saved = wxTextCtrl:new(Frame, ?LBL_SAVED, [{pos, {?STATS_START_X_PXLS+3*?LBL_DIST_X, ?STATS_START_Y_PXLS}}, {size, {?LBL_W, ?LBL_H}}, {style, ?wxTE_READONLY}]),
	wxTextCtrl:changeValue(Lbl_saved, "0"),

	Lbl_points = wxTextCtrl:new(Frame, ?LBL_POINTS, [{pos, {?STATS_START_X_PXLS+4*?LBL_DIST_X, ?STATS_START_Y_PXLS}}, {size, {?LBL_W, ?LBL_H}}, {style, ?wxTE_READONLY}]),
	wxTextCtrl:changeValue(Lbl_points, "0"),
	
	screenWrapper:setCell(?STATS_KEY, {?CELL_STATS,
		#statsRec{lbl_created = Lbl_created, lbl_alive = Lbl_alive, lbl_destroyed = Lbl_destroyed, lbl_saved = Lbl_saved, lbl_points = Lbl_points}}).

% Update buttons display
%-----------------------------------------------------------------------------------------%
updateBottomDisplay(Frame) ->
	% Show buttons %
	putImage(0, ?BOTTOM_START_Y_PXLS, "buttomBackGround.png", Frame),
	putImage(?buttonsStartX,  ?buttonsStartY, "buttonBackSmall.ico", Frame),
	putImage(?buttonsStartX+?buttonsDistance, ?buttonsStartY, "buttonBackSmall.ico", Frame),
	putImage(?buttonsStartX+2*?buttonsDistance, ?buttonsStartY, "buttonBackSmall.ico", Frame),
	putImage(?buttonsStartX+3*?buttonsDistance, ?buttonsStartY, "buttonBackSmall.ico", Frame),
	putImage(?FAST_FORWARD_X, ?buttonsStartY, "fastForward.ico", Frame),

	putImage(?BUTT_NUCLEAR_X, ?buttonsStartY, "buttonBackSmall.ico", Frame),
	putImage(?BUTT_NUCLEAR_X+2, ?buttonsStartY+2, "nuclear.ico", Frame),

	% Show stats icons%
	putImage(?STATS_START_X_PXLS-11, ?buttonsStartY, "created.ico", Frame),
	putImage(?STATS_START_X_PXLS+?LBL_DIST_X+2, ?buttonsStartY, "alive.ico", Frame),
	putImage(?STATS_START_X_PXLS+2*?LBL_DIST_X-6, ?buttonsStartY, "angel.ico", Frame),
	putImage(?STATS_START_X_PXLS+3*?LBL_DIST_X-11, ?buttonsStartY, "exit.ico", Frame),
	putImage(?STATS_START_X_PXLS+4*?LBL_DIST_X+7, ?buttonsStartY, "points.ico", Frame).

% Update stats display
%-----------------------------------------------------------------------------------------%
updateStatsDisplay(DisplayData) ->
	StatsData = screenWrapper:getAllStats(),
	wxTextCtrl:changeValue(DisplayData#statsRec.lbl_created, integer_to_list(StatsData#statsRec.lemmCreated)),
	wxTextCtrl:changeValue(DisplayData#statsRec.lbl_alive, integer_to_list(StatsData#statsRec.lemmAlive)),
	wxTextCtrl:changeValue(DisplayData#statsRec.lbl_destroyed, integer_to_list(StatsData#statsRec.lemmDestroyed)),
	wxTextCtrl:changeValue(DisplayData#statsRec.lbl_saved, integer_to_list(StatsData#statsRec.lemmSaved)),
	wxTextCtrl:changeValue(DisplayData#statsRec.lbl_points, integer_to_list(StatsData#statsRec.points)).


% Handle Call
%-----------------------------------------------------------------------------------------%
%handle_call(Request, From, State) -> Result
%{noreply,NewState} | {noreply,NewState,Timeout}

handle_call(_Request, _From, State) ->
	{noreply, State}.
	%{stop,Reason,NewState}.

%handle_call(show, _From, State) ->
%	{reply, ok, State}.

% Handle Info
%-----------------------------------------------------------------------------------------%
% Refresh Frame %
handle_info(timeout, State) ->
	Frame = State#displayRec.frame,	
	wxFrame:refresh(Frame),
	{noreply,State,?DISPLAY_DELAY};

handle_info(Msg, State) ->
	io:format("Error Msg! ~p~n",[Msg]),
	{noreply, State}.

% Refresh display
%-----------------------------------------------------------------------------------------%
refreshDisplay(Frame) ->
	ScreenList = screenWrapper:getScreen(),
	lists:map(fun({{X, Y}, {Type, Data}}) -> refreshCell(X, Y, Type, Data, Frame) end, ScreenList).

refreshCell(X, Y, Type, Data, Frame) ->
	case Type of
		?CELL_NONE           -> ok;

    ?CELL_GROUND         -> putGround(X, Y, Data, 1, Frame);

		?CELL_OBSTACLE       -> putGround(X, Y, Data, 1, Frame);

    ?CELL_LEMM           ->
			if
				Data#lemmRec.state =:= ?LEMM_STATE_FIGHTING -> putLemming(Data#lemmRec{state = ?LEMM_STATE_WALKING}, Frame);
				true                                        -> putLemming(Data, Frame)
			end;

		?CELL_STARTING_POINT -> putStartingPoint(X, Y, Frame), putStartingPoint(X, Y, Frame);

		?CELL_ENDING_POINT   -> putExit(X, Y, Frame);

		?CELL_BUTTON         ->			
			case X of
				?buttonsStartX div ?CELL_W   -> Xput = ?buttonsStartX;
				?buttonsStartX div ?CELL_W+1 -> Xput = ?buttonsStartX+?buttonsDistance;
				?buttonsStartX div ?CELL_W+2 -> Xput = ?buttonsStartX+2*?buttonsDistance;
				?buttonsStartX div ?CELL_W+3 -> Xput = ?buttonsStartX+3*?buttonsDistance
			end,

			putImage(Xput, Y*?CELL_H, "buttonBackSelectedSmall.ico", Frame),
			putImage(44, ?buttonsStartY+2, "iconBridge.ico", Frame),
			putImage(129, 699, "iconDig.ico", Frame),
			putImage(226, ?buttonsStartY+6, "iconStop.ico", Frame),
			putImage(332, ?buttonsStartY+2, "iconBomb.ico", Frame);

		?CELL_STATS          -> updateStatsDisplay(Data);

		?CELL_LOCAL_STATS    -> ok;

		?CELL_BOOM           -> putImage(X*?CELL_W, Y*?CELL_H, "boom.ico", Frame);

		?CELL_SAVED          ->
			case Data of
				0 -> putImage(X*?CELL_W, Y*?CELL_H, "lemmSaved0.ico", Frame);
				1 -> putImage(X*?CELL_W, Y*?CELL_H, "lemmSaved1.ico", Frame);
				2 -> putImage(X*?CELL_W, Y*?CELL_H, "lemmSaved2.ico", Frame);
				3 -> putImage(X*?CELL_W, Y*?CELL_H, "lemmSaved3.ico", Frame)
			end;

		Other                -> io:fwrite("ERROR! No match in cell content: ~p~n", [Other])

	end.

% Explode all lemmings - End processes
%-----------------------------------------------------------------------------------------%
explodeAllLemms() ->
	LemmsPids = screenWrapper:getLemmsPids(),
	lists:map(fun(Pid) -> lemming:bombByPid(Pid) end, LemmsPids).

terminateOpennings() ->
	StartingPids = screenWrapper:getStartingPids(),
	lists:map(fun(Pid) -> Pid!terminate end, StartingPids).

% Terminate
%-----------------------------------------------------------------------------------------%
terminate(_Reason, State) ->	
	if
		State#displayRec.levelStarted =:= true ->
			explodeAllLemms(),
			terminateOpennings(),
			utils:delay(2000),
			screenWrapper:terminateServers();
		true -> ok
	end,
		
	%wxFrame:destroy(Frame),
	wx:destroy(),
	if
		State#displayRec.restart =:= true -> State#displayRec.initProcessPID!restart;
		true                              -> State#displayRec.initProcessPID!abort
	end.

	
%-----------------------------------------------------------------------------------------%
% OTHER CALLBACKS
%-----------------------------------------------------------------------------------------%

% Code change
%-----------------------------------------------------------------------------------------%
code_change(_, _, State) ->
	{stop, ignore, State}.

% Handle cast
%-----------------------------------------------------------------------------------------%
handle_cast(_Request, State) ->
	{noreply,State}.

%% Open server nodes
%%----------------------------------------------------------------------------------------%%
openNodes(ServersNumber, ServersNumber) -> ok;

openNodes(I, ServersNumber) ->
	rpc:call(screenWrapper:getServerNodeByIndex(I), screenWrapper, start_link, [I, ServersNumber]),
	openNodes(I+1, ServersNumber).

%-----------------------------------------------------------------------------------------%
% HANDLE EVENTS
%-----------------------------------------------------------------------------------------%

%-----------------------------------------------------------------------------------------%
% Mouse clicks
%-----------------------------------------------------------------------------------------%

% MENU
%-----------------------------------------------------------------------------------------%
% Option hit %

% Check if option was selected %
which_option_selected(X, Y) when
	?BUTT_OPTION1_X < X, X < ?BUTT_OPTION1_X+?BUTT_OPTION_W,
	?BUTT_OPTION1_Y < Y, Y < ?BUTT_OPTION1_Y+?BUTT_OPTION_H -> 1;

which_option_selected(X, Y) when
	?BUTT_OPTION2_X < X, X < ?BUTT_OPTION2_X+?BUTT_OPTION_W,
	?BUTT_OPTION2_Y < Y, Y < ?BUTT_OPTION2_Y+?BUTT_OPTION_H -> 2;

which_option_selected(X, Y) when
	?BUTT_OPTION3_X < X, X < ?BUTT_OPTION3_X+?BUTT_OPTION_W,
	?BUTT_OPTION3_Y < Y, Y < ?BUTT_OPTION3_Y+?BUTT_OPTION_H -> 3;

which_option_selected(X, Y) when
	?BUTT_OPTION4_X < X, X < ?BUTT_OPTION4_X+?BUTT_OPTION_W,
	?BUTT_OPTION4_Y < Y, Y < ?BUTT_OPTION4_Y+?BUTT_OPTION_H -> 4;

which_option_selected(_X, _Y) -> -1. % None %

% Mouse rigth click
%-----------------------------------------------------------------------------------------%
handle_event(#wx{event = #wxMouse{type = right_down}}, State) ->
	screenWrapper:lemmsPerKingChange(),		
	{noreply, State, ?DISPLAY_DELAY};

% Nodes Menu %
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State)
when State#displayRec.activeScreen =:= ?SCREEN_MENU_NODES ->
	SelectedOption = which_option_selected(X, Y),
	case SelectedOption of
		1 -> ServersNumber = 1;
		2 -> ServersNumber = 2;
		3 -> ServersNumber = 4;
		_Else -> ServersNumber = -1
	end,
	if		
		ServersNumber =:= -1 ->
			{noreply, State};
		true ->
			% Open all remote servers %
			openNodes(0, ServersNumber),
			displayMenu(State#displayRec.frame, ?SCREEN_MENU_DIFFICULTY),
			{noreply, State#displayRec{serversNumber = ServersNumber, activeScreen = ?SCREEN_MENU_DIFFICULTY}}
	end;

% Difficulty Menu %
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State)
when State#displayRec.activeScreen =:= ?SCREEN_MENU_DIFFICULTY ->
	SelectedOption = which_option_selected(X, Y),
	if		
		SelectedOption =:= -1 ->
			{noreply, State};
		true ->
			displayMenu(State#displayRec.frame, ?SCREEN_MENU_LEVEL),
			{noreply, State#displayRec{difficulty = (6-SelectedOption), activeScreen = ?SCREEN_MENU_LEVEL}}
	end;

% Level select Menu %
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State)
when State#displayRec.activeScreen =:= ?SCREEN_MENU_LEVEL ->
	SelectedOption = which_option_selected(X, Y),
	if		
		SelectedOption =:= -1 ->
			{noreply, State};
		true ->
			startLevel(State#displayRec{activeScreen = ?SCREEN_LEVEL, levelStarted = true}, SelectedOption)
	end;

% Menu shown, but button not clicked %
handle_event(#wx{event = #wxMouse{type = left_down,x = _X,y = _Y}}, State)
when State#displayRec.activeScreen =:= ?SCREEN_MENU_NODES;
     State#displayRec.activeScreen =:= ?SCREEN_MENU_DIFFICULTY;
		 State#displayRec.activeScreen =:= ?SCREEN_MENU_LEVEL -> {noreply, State};

% LEVEL
%-----------------------------------------------------------------------------------------%
% Fast forward button %
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State)
when ?buttonsStartY < Y, Y < ?buttonsStartY + ?buttonsSize, ?FAST_FORWARD_X < X, X < ?FAST_FORWARD_X+?buttonsSize  ->
		utils:playSound("MOUSEPRE.WAV", ?IS_SOUND_ON),
		screenWrapper:speedUpLemmings(),		
		{noreply, State, ?DISPLAY_DELAY};

% Buttons %
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State)
when ?buttonsStartY < Y, Y < ?buttonsStartY+?buttonsSize -> %, X < ?buttonsStartX+3*?buttonsDistance+?buttonsSize ->
	OldButtonKey = {?buttonsStartX div ?CELL_W+State#displayRec.selectedButton, ?buttonsStartY div ?CELL_H},
			if
				?buttonsStartX < X, X < ?buttonsStartX+?buttonsSize                                       -> NewState = State#displayRec{selectedButton = 0, selectedButtonX = ?buttonsStartX, selectedButtonY = ?buttonsStartY}, Termination = false;
				?buttonsStartX+?buttonsDistance < X, X < ?buttonsStartX+?buttonsDistance+?buttonsSize     -> NewState = State#displayRec{selectedButton = 1, selectedButtonX = ?buttonsStartX+?buttonsDistance, selectedButtonY = ?buttonsStartY}, Termination = false;
				?buttonsStartX+2*?buttonsDistance < X, X < ?buttonsStartX+2*?buttonsDistance+?buttonsSize -> NewState = State#displayRec{selectedButton = 2, selectedButtonX = ?buttonsStartX+2*?buttonsDistance, selectedButtonY = ?buttonsStartY}, Termination = false;
				?buttonsStartX+3*?buttonsDistance < X, X < ?buttonsStartX+3*?buttonsDistance+?buttonsSize -> NewState = State#displayRec{selectedButton = 3, selectedButtonX = ?buttonsStartX+3*?buttonsDistance, selectedButtonY = ?buttonsStartY}, Termination = false;
				
				% Terminate Level %
				?BUTT_NUCLEAR_X < X, X < ?BUTT_NUCLEAR_X+?buttonsSize -> 	NewState = State, Termination = true; %wx_object:stop(?DisplayNode);

				true -> NewState = State, Termination = false
			end,

			if
			  State =/= NewState -> screenWrapper:moveCell({OldButtonKey, {?CELL_BUTTON, 0}}, {{?buttonsStartX div ?CELL_W+NewState#displayRec.selectedButton, ?buttonsStartY div ?CELL_H}, {?CELL_BUTTON, 0}});
				true -> ok
			end,

			if
				Termination =:= true -> {stop, normal, State#displayRec{restart = true}};
				true -> {noreply, NewState, ?DISPLAY_DELAY}
			end;

% Other %
handle_event(#wx{event = #wxMouse{type = left_down,x = X,y = Y}}, State) ->
	{PointedCellX, PointedCellY} = {X div ?CELL_W, Y div ?CELL_H},
	{Type, Data} = screenWrapper:getCell({PointedCellX, PointedCellY}),	
	if		
		Type =:= ?CELL_LEMM ->
			% Handle lemming click %			
			handleClick(Data#lemmRec.state, State#displayRec.selectedButton, Data),
			{noreply, State, ?DISPLAY_DELAY};

		true ->
			{noreply, State, ?DISPLAY_DELAY}
	end.
									
handleClick(_, ?BUTT_BOMB, Data) ->
	lemming:bomb(Data);

handleClick(?LEMM_STATE_WALKING, ?BUTT_STOP, Data) ->
	if 
		Data#lemmRec.isKing =:= false ->
			utils:playSound("MOUSEPRE.WAV", ?IS_SOUND_ON),
			try gen_fsm:send_event(Data#lemmRec.fsmPid, setStateStopping) catch _EXp -> ok end;
		true                          -> ok
	end;
			
handleClick(?LEMM_STATE_WALKING, ?BUTT_DIG, Data) ->
	utils:playSound("MOUSEPRE.WAV", ?IS_SOUND_ON),
	try gen_fsm:send_event(Data#lemmRec.fsmPid, setStateDigging) catch _EXp -> ok end;

handleClick(?LEMM_STATE_WALKING, ?BUTT_BOMB, Data) ->
	utils:playSound("MOUSEPRE.WAV", ?IS_SOUND_ON),
	try gen_fsm:send_event(Data#lemmRec.fsmPid, setStateBombing) catch _EXp -> ok end;

handleClick(?LEMM_STATE_WALKING, ?BUTT_BRIDGE, Data) when Data#lemmRec.bricksLeft =:= 0 ->
	utils:playSound("MOUSEPRE.WAV", ?IS_SOUND_ON),
	try gen_fsm:send_event(Data#lemmRec.fsmPid, setStateBridging) catch _EXp -> ok end;

handleClick(_, _, _) -> ok.		

%wxStaticText:new(Frame, 0,"50 $",[{pos, {X, Y}}]),	

%
%-----------------------------------------------------------------------------------------%
%type = key_down, 
%handle_event(Wx, State) ->
%	io:fwrite("XX~n"),
%	{noreply, State, ?DISPLAY_DELAY};
%-----------------------------------------------------------------------------------------%
% Graphics APIs
%-----------------------------------------------------------------------------------------%

% Scale
%-----------------------------------------------------------------------------------------%
scaleX(X) -> X*?CELL_W.
scaleY(Y) -> Y*?CELL_H.

% Put Image
%-----------------------------------------------------------------------------------------%
putImage(X, Y, ImageName, Panel) ->
	putImage(X, Y, ImageName, Panel, false).

putImage(X, Y, ImageName, Panel, IsRotateImage) ->	
  ImagePre = wxImage:new(ImageName),
	case IsRotateImage of
		true -> Image = wxImage:mirror(ImagePre);
		_ -> Image = wxImage:new(ImageName)
	end,
	Bmp = wxBitmap:new(Image),
	Paint = wxPaintDC:new(Panel),
	wxDC:drawBitmap(Paint, Bmp, {X, Y}),
	wxImage:destroy(Image),
	wxImage:destroy(ImagePre),		
	wxBitmap:destroy(Bmp),
	wxPaintDC:destroy(Paint).

%-----------------------------------------------------------------------------------------%
% Lemmings
%-----------------------------------------------------------------------------------------%

% Images names
%-----------------------------------------------------------------------------------------%
getImage(_, ?LEMM_STATE_KING_WAITING, _, _) -> nil;

% Tribe, State, Frame %
getImage(0, ?LEMM_STATE_WALKING, 0, false) -> "lemm0Walking0.png";
getImage(0, ?LEMM_STATE_WALKING, 1, false) -> "lemm0Walking1.ico";
getImage(1, ?LEMM_STATE_WALKING, 0, false) -> "lemm1Walking0.png";
getImage(1, ?LEMM_STATE_WALKING, 1, false) -> "lemm1Walking1.ico";
getImage(2, ?LEMM_STATE_WALKING, 0, false) -> "lemm2Walking0.png";
getImage(2, ?LEMM_STATE_WALKING, 1, false) -> "lemm2Walking1.ico";
getImage(3, ?LEMM_STATE_WALKING, 0, false) -> "lemm3Walking0.png";
getImage(3, ?LEMM_STATE_WALKING, 1, false) -> "lemm3Walking1.ico";

getImage(0, ?LEMM_STATE_WALKING, 0, true)  -> "lemm0KingWalking0.ico";
getImage(0, ?LEMM_STATE_WALKING, 1, true)  -> "lemm0KingWalking1.ico";
getImage(1, ?LEMM_STATE_WALKING, 0, true)  -> "lemm1KingWalking0.ico";
getImage(1, ?LEMM_STATE_WALKING, 1, true)  -> "lemm1KingWalking1.ico";
getImage(2, ?LEMM_STATE_WALKING, 0, true)  -> "lemm2KingWalking0.ico";
getImage(2, ?LEMM_STATE_WALKING, 1, true)  -> "lemm2KingWalking1.ico";
getImage(3, ?LEMM_STATE_WALKING, 0, true)  -> "lemm3KingWalking0.ico";
getImage(3, ?LEMM_STATE_WALKING, 1, true)  -> "lemm3KingWalking1.ico";

getImage(0, ?LEMM_STATE_FALLING, _, false) -> "lemm0Falling0.ico";
getImage(1, ?LEMM_STATE_FALLING, _, false) -> "lemm1Falling0.ico";
getImage(2, ?LEMM_STATE_FALLING, _, false) -> "lemm2Falling0.ico";
getImage(3, ?LEMM_STATE_FALLING, _, false) -> "lemm3Falling0.ico";

getImage(0, ?LEMM_STATE_FALLING, _, true)  -> "lemm0KingWalking0.ico";
getImage(1, ?LEMM_STATE_FALLING, _, true)  -> "lemm1KingWalking0.ico";
getImage(2, ?LEMM_STATE_FALLING, _, true)  -> "lemm2KingWalking0.ico";
getImage(3, ?LEMM_STATE_FALLING, _, true)  -> "lemm3KingWalking0.ico";

getImage(0, ?LEMM_STATE_STOPPING, F, _) when F < 2 -> "lemm0Stopping0.ico";
getImage(0, ?LEMM_STATE_STOPPING, F, _) when F < 4 -> "lemm0Stopping1.ico";
getImage(1, ?LEMM_STATE_STOPPING, F, _) when F < 2 -> "lemm1Stopping0.ico";
getImage(1, ?LEMM_STATE_STOPPING, F, _) when F < 4 -> "lemm1Stopping1.ico";
getImage(2, ?LEMM_STATE_STOPPING, F, _) when F < 2 -> "lemm2Stopping0.ico";
getImage(2, ?LEMM_STATE_STOPPING, F, _) when F < 4 -> "lemm2Stopping1.ico";
getImage(3, ?LEMM_STATE_STOPPING, F, _) when F < 2 -> "lemm3Stopping0.ico";
getImage(3, ?LEMM_STATE_STOPPING, F, _) when F < 4 -> "lemm3Stopping1.ico";

getImage(_, ?LEMM_STATE_BOMBING, _, _) -> "boom.ico".

isRotateImage(?DIR_RIGHT) -> true;
isRotateImage(?DIR_LEFT)  -> false.

% Put lemming
%-----------------------------------------------------------------------------------------%
putLemming(Lemming, Panel) ->
	ImageName = getImage(Lemming#lemmRec.tribe, Lemming#lemmRec.state, Lemming#lemmRec.frame, Lemming#lemmRec.isKing),
	if
		ImageName =/= nil ->
			putImage(Lemming#lemmRec.x*?CELL_W, Lemming#lemmRec.y*?CELL_H, ImageName, Panel, isRotateImage(Lemming#lemmRec.direction)),
			ImageWeapon = getWeaponImage(Lemming#lemmRec.weapon),
			putImage(Lemming#lemmRec.x*?CELL_W, Lemming#lemmRec.y*?CELL_H-10, ImageWeapon, Panel);
		true -> ok
	end.

getWeaponImage(1) -> "1.ico";
getWeaponImage(2) -> "2.ico";
getWeaponImage(3) -> "3.ico";
getWeaponImage(inf) -> "inf.ico".

%-----------------------------------------------------------------------------------------%
% BackGround
%-----------------------------------------------------------------------------------------%
imageNameByGroundStyle(?GROUND0) -> "ground0.png";
imageNameByGroundStyle(?GROUND1) -> "ground1.ico";
imageNameByGroundStyle(?GROUND2) -> "ground2.ico";
imageNameByGroundStyle(?GROUND3) -> "ground3.ico";
imageNameByGroundStyle(?BRIDGE)  -> "bridge.ico";

imageNameByGroundStyle(?GROUND_SPIKE) -> "spike.ico";
imageNameByGroundStyle(?GROUND_LAVA)  -> "lava3.ico";

imageNameByGroundStyle(_) -> "ground2.ico".

% Ground
%-----------------------------------------------------------------------------------------%
putGround(_, _, _,0, _) ->
	ok;

putGround(X, Y, Style, Length, Panel) ->
	putGround(X+Length-1, Y, Style, Panel),
	putGround(X, Y, Style, Length-1, Panel).

putGround(X, Y, Style, Panel) ->
	ImageName = imageNameByGroundStyle(Style),
	putImage(X*?CELL_W, Y*?CELL_H, ImageName, Panel, false).	

% Starting Point
%-----------------------------------------------------------------------------------------%
putStartingPoint(X, Y, Panel) ->
	putImage((X-1)*?CELL_W, Y*?CELL_H, "openning.png", Panel, false).

% Exit Point
%-----------------------------------------------------------------------------------------%
putExit(X, Y, Panel) ->
	putImage((X-1)*?CELL_W, (Y-2)*?CELL_H, "exit.png", Panel, false).

% PutBoarders
%-----------------------------------------------------------------------------------------%
putBoarders(0)         -> ok;
putBoarders(LeftToPut) ->
	screenWrapper:setCell({-1, LeftToPut}, {?CELL_GROUND, ?GROUND0}),
	screenWrapper:setCell({?SCREEN_WIDTH, LeftToPut}, {?CELL_GROUND, ?GROUND0}),
	%screenWrapper:setCell({0, LeftToPut}, {?CELL_GROUND, ?GROUND0}),
	%screenWrapper:setCell({?SCREEN_WIDTH-1, LeftToPut}, {?CELL_GROUND, ?GROUND0}),
	putBoarders(LeftToPut-1).

%%----------------------------------------------------------------------------------------%%
%% BUILD LEVEL
%%----------------------------------------------------------------------------------------%%
putCell(_, _, _,0) -> ok;
putCell(X, Y, Cell, Length) ->
	screenWrapper:setCell({X+Length-1, Y}, Cell),
	putCell(X, Y, Cell, Length-1).

putCellX(Xs, Xe, _, _) when Xs =:= Xe+1  -> ok;
putCellX(Xs, Xe, Y, Cell) ->
	screenWrapper:setCell({Xs, Y}, Cell),
	putCellX(Xs+1, Xe, Y, Cell).

putGround(_, _, 0) -> ok;
putGround(X, Y, Length) ->
	BackIndx = screenWrapper:getServerIndexByCell({X+Length-1, Y}),
	screenWrapper:setCell({X+Length-1, Y}, {?CELL_GROUND, BackIndx}),
	putGround(X, Y, Length-1).

putGroundX(Xs, Xe, _) when Xs =:= Xe+1 -> ok;
putGroundX(Xs, Xe, Y) ->
	BackIndx = screenWrapper:getServerIndexByCell({Xs, Y}),
	screenWrapper:setCell({Xs, Y}, {?CELL_GROUND, BackIndx}),
	putGroundX(Xs+1, Xe, Y).

% Level 1
%-----------------------------------------------------------------------------------------%
buildLevel1(N, N) -> ok;
buildLevel1(Y, N) -> putGround(0, 6+Y*3, ?SCREEN_WIDTH), buildLevel1(Y+1, N).
buildLevelBackGround(1) ->
	buildLevel1(1, 10),

	% ENDING POINTS %	
	putCell(4, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(2, 40, 6),
	putCell(22, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(20, 40, 6),
	putCell(40, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(38, 40, 6),
	putCell(58, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(56, 40, 6);

% Level 2
%-----------------------------------------------------------------------------------------%
% 4 22 40 58
buildLevelBackGround(2) ->
	putGroundX(2, 8, 6),
	putGroundX(18, 26, 6),
	putGroundX(37, 43, 6),
	putGroundX(55, 61, 6),
	putCellX(0, 10, 10, {?CELL_OBSTACLE, ?GROUND_SPIKE}),
	putCellX(15, 44, 10, {?CELL_OBSTACLE, ?GROUND_SPIKE}),
	putCellX(58, 63, 10, {?CELL_OBSTACLE, ?GROUND_SPIKE}),
	putGroundX(8, 24, 15),
	putGroundX(38, 60, 15),
	putCellX(0,  8, 21, {?CELL_OBSTACLE, ?GROUND_SPIKE}),
	putCellX(11, 14, 21, {?CELL_OBSTACLE, ?GROUND_SPIKE}),
	putCellX(17, 39, 21, {?CELL_OBSTACLE, ?GROUND_SPIKE}),
	putCellX(42, 47, 21, {?CELL_OBSTACLE, ?GROUND_SPIKE}),
	putCellX(50, 63, 21, {?CELL_OBSTACLE, ?GROUND_SPIKE}),

	putGroundX(2, 53, 27),

	putGroundX(11, 16, 33),
	putGroundX(30, 38, 33),
	putGroundX(48, 52, 33),

	putCellX(0, 7, 36, {?CELL_OBSTACLE, ?GROUND_SPIKE}),
	putCellX(23, 35, 36, {?CELL_OBSTACLE, ?GROUND_SPIKE}),

	% ENDING POINTS %	
	putCell(4, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGroundX(0, 8, 40),
	putCell(22, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(19, 40, 26),
	putCell(40, 39, {?CELL_ENDING_POINT, 0}, 1),
	putCell(58, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGroundX(54, 63, 40),
	ok;

% Level 3
%-----------------------------------------------------------------------------------------%
buildLevelBackGround(3) ->
	putGroundX(2, 8, 6),
	putGroundX(18, 26, 6),
	putGroundX(37, 43, 6),
	putGroundX(55, 61, 6),
	putGroundX(8, 24, 15),
	putGroundX(38, 60, 15),

	putGroundX(2, 53, 27),

	putGroundX(11, 16, 33),
	putGroundX(30, 38, 33),
	putGroundX(48, 52, 33),

	% ENDING POINTS %	
	putCell(4, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGroundX(0, 8, 40),
	putCell(22, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(19, 40, 26),
	putCell(40, 39, {?CELL_ENDING_POINT, 0}, 1),
	putCell(58, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGroundX(54, 63, 40),
	ok;

% 4 22 40 58
%buildLevelBackGround(3) ->
%	putGroundX(2, 8, 6),
%	putGroundX(18, 26, 6),
%	putGroundX(37, 43, 6),
%	putGroundX(55, 61, 6),
%	putGroundX(8, 24, 15),
%	putGroundX(38, 60, 15),
%	putGroundX(2, 53, 27),
%
%	putGroundX(11, 16, 33),
%	putGroundX(30, 38, 33),
%	putGroundX(48, 52, 33),
%
%	% ENDING POINTS %	
%	putCell(4, 39, {?CELL_ENDING_POINT, 0}, 1),
%	putGroundX(0, 8, 40),
%	putCell(22, 39, {?CELL_ENDING_POINT, 0}, 1),
%	putGround(19, 40, 26),
%	putCell(40, 39, {?CELL_ENDING_POINT, 0}, 1),
%	putCell(58, 39, {?CELL_ENDING_POINT, 0}, 1),
%	putGroundX(54, 63, 40),
%	ok;

% Level 4
%-----------------------------------------------------------------------------------------%


buildLevelBackGround(4) ->
	buildLevel4(1, 10),

	% ENDING POINTS %	

	putCell(4, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(2, 40, 6),
	putCell(22, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(20, 40, 6),
	putCell(40, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(38, 40, 6),
	putCell(58, 39, {?CELL_ENDING_POINT, 0}, 1),
	putGround(56, 40, 6);


% NO LEVEL
%-----------------------------------------------------------------------------------------%
buildLevelBackGround(100) ->
	putCell(0, 8, {?CELL_GROUND, ?GROUND3}, 10),
	putCell(15, 10, {?CELL_GROUND, ?GROUND3}, 10),

	putCell(15, 10, {?CELL_GROUND, ?GROUND0}, 10),
	putCell(10, 15, {?CELL_GROUND, ?GROUND0}, 20),


	putCell(5, 20, {?CELL_GROUND, ?GROUND0}, 30),

	putCell(35, 12, {?CELL_GROUND, ?GROUND1}, 10),
	putCell(30, 17, {?CELL_GROUND, ?GROUND1}, 20),
	putCell(25, 22, {?CELL_GROUND, ?GROUND1}, 30),

	putCell(3, 25, {?CELL_GROUND, ?GROUND2}, 60).
	
buildLevel(_ServersNumber, LevelNum, Difficulty) ->
	% LAVA AT BOTTOM %	
	putCell(0, ?BOTTOM_START_Y-1, {?CELL_OBSTACLE, ?GROUND_LAVA}, ?SCREEN_WIDTH),

	% GROUNDS %
	buildLevelBackGround(LevelNum),		

	% PUT BOARDERS %
	putBoarders(?BOTTOM_START_Y-2),

	% ENDING POINTS %	
	%putCell(4, 39, {?CELL_ENDING_POINT, 0}, 1),
	%putGround(2, 40, 6),
	%putCell(22, 39, {?CELL_ENDING_POINT, 0}, 1),
	%putGround(20, 40, 6),
	%putCell(40, 39, {?CELL_ENDING_POINT, 0}, 1),
	%putGround(38, 40, 6),
	%putCell(58, 39, {?CELL_ENDING_POINT, 0}, 1),
	%putGround(56, 40, 6),

	% STARTING POINTS %	
	screenWrapper:createStartingPoint({4,  1}, 0, Difficulty),
	screenWrapper:createStartingPoint({22, 1}, 1, Difficulty),
	screenWrapper:createStartingPoint({40, 1}, 2, Difficulty),
	screenWrapper:createStartingPoint({58, 1}, 3, Difficulty).

buildLevel4(N, N) -> ok;
buildLevel4(Y, N) -> 	putGround(0,  6+Y*3, ?SCREEN_WIDTH),
										 	putGround(15, 5+Y*3, 2),
											putGround(31, 5+Y*3, 2),
											putGround(47, 5+Y*3, 2),
										 	buildLevel4(Y+1, N).
%-----------------------------------------------------------------------------------------%

