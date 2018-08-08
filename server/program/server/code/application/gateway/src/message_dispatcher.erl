
-module(message_dispatcher).
-export([handle/4]).
-include_lib("protob/include/p01_login_pb.hrl").
-include_lib("protob/include/p02_room_pb.hrl").
-include_lib("protob/include/p03_mahjong_fight_pb.hrl").
-include_lib("protob/include/p04_player_pb.hrl").
-include_lib("protob/include/p05_common_pb.hrl").
-include_lib("protob/include/p06_feedback_pb.hrl").
handle(Msg, _GatePid, _ClientSocket, PlayerPid) ->
    case Msg of
		#cs_login{} -> lib_gate:login("p01_login", PlayerPid, Msg);
		#sc_login{} -> lib_gate:ok("p01_login", PlayerPid, Msg);
		#cs_login_reconnection{} -> lib_gate:login_reconnection("p01_login", PlayerPid, Msg);
		#sc_login_reconnection{} -> lib_gate:ok("p01_login", PlayerPid, Msg);
		#cs_login_logoff{} -> lib_gate:logoff("p01_login", PlayerPid, Msg);
		#sc_login_repeat{} -> lib_gate:ok("p01_login", PlayerPid, Msg);
		#cs_login_heartbeat{} -> lib_gate:heartbeat("p01_login", PlayerPid, Msg);
		#sc_login_heartbeat{} -> lib_gate:ok("p01_login", PlayerPid, Msg);
		#cs_room_find{} -> lib_gate:player_msg("p02_room", PlayerPid, Msg);
		#sc_room_find_response{} -> lib_gate:ok("p02_room", PlayerPid, Msg);
		#cs_room_quit{} -> lib_gate:player_msg("p02_room", PlayerPid, Msg);
		#sc_room_quit_response{} -> lib_gate:ok("p02_room", PlayerPid, Msg);
		#cs_room_sit{} -> lib_gate:player_msg("p02_room", PlayerPid, Msg);
		#sc_room_sit_response{} -> lib_gate:ok("p02_room", PlayerPid, Msg);
		#sc_room_update{} -> lib_gate:ok("p02_room", PlayerPid, Msg);
		#sc_room_clean{} -> lib_gate:ok("p02_room", PlayerPid, Msg);
		#cs_room_owner_list{} -> lib_gate:player_msg("p02_room", PlayerPid, Msg);
		#sc_room_owner_list_response{} -> lib_gate:ok("p02_room", PlayerPid, Msg);
		#cs_room_start_mahjong{} -> lib_gate:player_msg("p02_room", PlayerPid, Msg);
		#sc_room_start_mahjong{} -> lib_gate:ok("p02_room", PlayerPid, Msg);
		#cs_mahjong_fight_die{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_die{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#cs_mahjong_fight_play{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#cs_mahjong_fight_meld{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#cs_mahjong_fight_win{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_end{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_info{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_state{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#cs_mahjong_fight_cancel{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#cs_mahjong_fight_ready{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#cs_mahjong_fight_prepare{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_play{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_win{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_cancel{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_prepare{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_ready{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_left_time{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#cs_mahjong_fight_record{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_record{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#cs_mahjong_fight_update{} -> lib_gate:player_msg("p03_mahjong_fight", PlayerPid, Msg);
		#sc_mahjong_fight_meld{} -> lib_gate:ok("p03_mahjong_fight", PlayerPid, Msg);
		#sc_player_info{} -> lib_gate:ok("p04_player", PlayerPid, Msg);
		#cs_player_recharge{} -> lib_gate:player_msg("p04_player", PlayerPid, Msg);
		#sc_player_recharge{} -> lib_gate:ok("p04_player", PlayerPid, Msg);
		#cs_query_game_statistics{} -> lib_gate:player_msg("p05_common", PlayerPid, Msg);
		#sc_query_game_statistics_response{} -> lib_gate:ok("p05_common", PlayerPid, Msg);
		#cs_query_game_record{} -> lib_gate:player_msg("p05_common", PlayerPid, Msg);
		#sc_query_game_record_response{} -> lib_gate:ok("p05_common", PlayerPid, Msg);
		#cs_query_game_find{} -> lib_gate:player_msg("p05_common", PlayerPid, Msg);
		#sc_query_game_find_response{} -> lib_gate:ok("p05_common", PlayerPid, Msg);
		#sc_common_game_init{} -> lib_gate:ok("p05_common", PlayerPid, Msg);
		#cs_common_chat{} -> lib_gate:player_msg("p05_common", PlayerPid, Msg);
		#sc_common_chat{} -> lib_gate:ok("p05_common", PlayerPid, Msg);
		#sc_common_notice{} -> lib_gate:ok("p05_common", PlayerPid, Msg);
		#cs_feedback_fill{} -> lib_gate:player_msg("p06_feedback", PlayerPid, Msg);
		#sc_feedback_fill{} -> lib_gate:ok("p06_feedback", PlayerPid, Msg);
		_ -> logger:warning_msg("~p handle not match! Msg = ~p~n", [?MODULE, Msg])
	end.
