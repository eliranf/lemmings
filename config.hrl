%-module(config).
%-compile(export_all).

%%========================================================================================%%
%% CONSTS
%%========================================================================================%%
% Servers
%-----------------------------------------------------------------------------------------%
%-define(DisplayNode, 'Display@localhost').
%-define(Server0,  'S0@localhost').
%-define(Server1,  'S1@localhost').
%-define(Server2,  'S2@localhost').
%-define(Server3,  'S3@localhost').

% TO CHANGE IN THE END!!!!!!!!! %
-define(DisplayNode, 'main@132.72.104.199').
-define(Server0Node, 'S0@132.72.104.219').
-define(Server1Node, 'S1@132.72.104.197').
-define(Server2Node, 'S2@132.72.104.196').
-define(Server3Node, 'S3@132.72.104.236').

-define(Server0, serv0).
-define(Server1, serv1).
-define(Server2, serv2).
-define(Server3, serv3).

%-define(Server0, 'main@132.72.104.119').
%-define(Server1, 'S1@132.72.104.219 ').
%-define(Server2, 'S2@localhost').
%-define(Server3, 'S3@localhost').

-define(RESTART_DELAY, 800).

% Screen Config
%-----------------------------------------------------------------------------------------%
-define(CELL_W,            16).
-define(CELL_H,            16).
-define(FRAME_WIDTH_PXLS,  1024).
-define(FRAME_HEIGHT_PXLS, 768).
-define(SCREEN_WIDTH,      64).
-define(SCREEN_HEIGHT,     48).
-define(PXL, 2).

-define(BOTTOM_START_Y_PXLS, 672).
-define(BOTTOM_START_Y,      42).

-define(IS_SOUND_ON, true).
% Buttons
%-----------------------------------------------------------------------------------------%
-define(buttonsStartX,   32).
-define(buttonsStartY,   688).
-define(buttonsSize,     64).
-define(buttonsDistance, 96).

-define(BUTT_NUCLEAR_X, 850).

-define(FAST_FORWARD_X, ?FRAME_WIDTH_PXLS - 80).

% Timeouts
%-----------------------------------------------------------------------------------------%
-define(LEMM_DELAY_START,        400).

-define(MAX_LEMM_DELAY,   500).
-define(MIN_LEMM_DELAY,   100).
-define(LEMM_DELAY_DELTA, 100).

-define(BOOM_DELAY, 300).

-define(STARTING_POINT_DELAY,    ?LEMM_DELAY_START).
-define(DISPLAY_DELAY,           75).
-define(CELLS_GAP_BETWEEN_LEMMS, 4).

-define(LOCK_DELAY_BASE, 100).
-define(LOCK_DELAY_RAND, 4).
%-define(LEMM_DELAY,     400).
%-define(STARTING_POINT_DELAY, ?LEMM_DELAY*3).

% Cell Types
%-----------------------------------------------------------------------------------------%
-define(CELL_NONE,           undefined).
-define(CELL_GROUND,         1).
-define(CELL_LEMM,           2).
-define(CELL_OBSTACLE,       3).
-define(CELL_STARTING_POINT, 4).
-define(CELL_ENDING_POINT,   5).
-define(CELL_BUTTON,         6).
-define(CELL_STATS,          7).
-define(CELL_LOCAL_STATS,    8).
-define(CELL_BOOM,           9).
-define(CELL_SAVED,          10).

-define(CELL_LOCKED,         1000).

-define(GROUND0,       0).
-define(GROUND1,       1).
-define(GROUND2,       2).
-define(GROUND3,       3).
-define(BRIDGE,        4).

-define(GROUND_SPIKE,  31).
-define(GROUND_LAVA,   32).

% Lemming
%-----------------------------------------------------------------------------------------%
% States %
-define(LEMM_STATE_WALKING,  21).
-define(LEMM_STATE_FALLING,  22).
-define(LEMM_STATE_STOPPING, 23).
-define(LEMM_STATE_BRIDGING, 24).
-define(LEMM_STATE_BOMBING,  25).
-define(LEMM_STATE_KING_WAITING, 26).
-define(LEMM_STATE_FIGHTING, 27).

% Tribes %
-define(TRIBE0, 0).
-define(TRIBE1, 1).
-define(TRIBE2, 2).
-define(TRIBE3, 3).

% Directions %
-define(DIR_RIGHT, 1).
-define(DIR_LEFT, -1).

-define(BRICKS_AMOUNT, 3).

%  Buttons
%-----------------------------------------------------------------------------------------%
-define(BUTT_BRIDGE,  0).
-define(BUTT_DIG,     1).
-define(BUTT_STOP,    2).
-define(BUTT_BOMB,    3).
-define(BUTT_SPEED,   4).

%  Colors
%-----------------------------------------------------------------------------------------%
-define(wxDARK_BLUE,  {0, 3, 61}).

% Stats
%-----------------------------------------------------------------------------------------%

-define(LBL_DIST_Y, 14).
-define(LBL_DIST_X, 80).
-define(LBL_H, 26).
-define(LBL_W, 42).

-define(STATS_START_Y_PXLS, ?buttonsStartY + 38).
-define(STATS_START_Y,      44).
-define(STATS_START_X_PXLS, 448).
-define(STATS_START_X,      28).

-define(STATS_KEY, {?STATS_START_X, ?STATS_START_Y}).

% DON'T CHANGE THE ORDER!!! OR VALUE %
-define(STATS_CREATED_IDX,   2).
-define(STATS_ALIVE_IDX,     3).
-define(STATS_DESTROYED_IDX, 4).
-define(STATS_SAVED_IDX,     5).
-define(STATS_POINTS_IDX,    6).

% POINTS values %
-define(POINTS_START, 200).
-define(POINTS_LEMM,  1).
-define(POINTS_KING,  5).

%%========================================================================================%%
%% RECORDS
%%========================================================================================%%

% DON'T CHANGE THE ORDER!!! OR VALUE %
-record(statsRec,
	{lemmCreated   = 0,
	 lemmAlive     = 0,
	 lemmDestroyed = 0,
	 lemmSaved     = 0,
	 points        = ?POINTS_START,
	 lbl_created,
	 lbl_alive,
	 lbl_destroyed,
	 lbl_saved,
	 lbl_points}).

% Lemming %
-record(lemmRec,
	{x,
	 y,
	 startingPointPid,
	 state = ?LEMM_STATE_WALKING,
	 direction = ?DIR_RIGHT,
	 delay = ?LEMM_DELAY_START,
%	 speed = ?LEMM_DELAY,
	 frame = 0,
   tribe = 0,
	 fsmPid,
	 bricksLeft = 0,
	 isKing = false,
	 weapon,
	 kingPid,
	 monitorRef,
	 serversNumber}).

-define(LEMMS_PER_KING_MIN,       5).
-define(LEMMS_PER_KING_UNLIMITED, 100000).

% Starting Point %
-record(startingRec,
	{x,
	 y,
	 kingPid = nil,
	 startKingCounter = ?LEMMS_PER_KING_MIN,
	 delay = ?STARTING_POINT_DELAY,
	 selfPid,
	 tribe = 0,
	 serversNumber,
	 lemmsPerKing = ?LEMMS_PER_KING_MIN,
	 difficulty}).

% Ending Point %
-record(endingRec,
	{x,
	 y,
	 selfPid,
	 tribe = 0}).

%-----------------------------------------------------------------------------------------%

