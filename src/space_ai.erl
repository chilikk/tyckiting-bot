-module(space_ai).

-export([give_moves/6]).

-include("include/space.hrl").

%% To be changed by AI implementer:
-record(state, {shoot, shootn, ignore = []}).

give_moves(RoundId, Config, Team, OtherTeams, Events, state_uninitialized) ->
    State = #state{shoot = false},
    give_moves(RoundId, Config, Team, OtherTeams, Events, State);

give_moves(_RoundId,
           #config{bots         = _ConfigBots,
                   field_radius = ConfigFieldRadius,
                   move         = _ConfigMove,
                   start_hp     = _ConfigStartHp,
                   cannon       = _ConfigCannon,
                   radar        = _ConfigRadar,
                   see          = _ConfigSee,
                   max_count    = _ConfigMaxCound,
                   asteroids    = _ConfigAsteroids,
                   loop_time    = _ConfigLoopTime,
                   no_wait      = _ConfigNoWait} = _Config,
           #team{name    = _MyTeamName,
                 team_id = _MyTeamId,
                 bots    = MyBots} = _MyTeam,
           _OtherTeams,
           Events,
           State0) ->

    MyBotIds = [MB#bot.bot_id || MB <- MyBots],

    %% Current AI state to keep information
    %% not given by the server (my own memory)
    io:format("Events: ~p~n", [Events]),
    io:format("Current state: ~p~n", [State0]),

    %% All hostile bots seen:
    RadarSeen0    = [Pos || {radar_echo, Pos} <- Events],
    AdjacentSeen0 = [Pos || {see, _Source, BotId, Pos} <- Events,
                           not lists:member(BotId, MyBotIds)],

    AdjacentMy0 = [Source || {see, Source, BotId, _Pos} <- Events,
                           not lists:member(BotId, MyBotIds)],
    AdjacentMy = uniq(AdjacentMy0),

    State =
    lists:foldl(
      fun({X,Y}, S) ->
              NewIgnore = not lists:member({X,Y}, S#state.ignore)
              andalso lists:member({X,Y}, S#state.shoot)
              andalso not lists:member({X,Y}, [Bot#bot.pos || Bot <- MyBots])
              andalso length([Z || Z = {hit, _, _} <- Events]) =:= 0,
              if NewIgnore ->
                     S#state{ignore = [{X,Y}|S#state.ignore]};
                 true ->
                     S
              end
      end, State0, RadarSeen0 ++ AdjacentSeen0),

    RadarSeen = RadarSeen0 -- State#state.ignore,
    AdjacentSeen = AdjacentSeen0 -- State#state.ignore,


    Detected = [ DetectedBot || {detected, DetectedBot} <- Events ] ++
               [ DetectedBot || {see, DetectedBot, BotId, _} <- Events,
                                lists:member(DetectedBot, MyBotIds),
                                not lists:member(BotId, MyBotIds) ],

    Damaged = [ BotId || {damaged, BotId, _Damage} <- Events],

    NAlive = lists:foldl(fun(#bot{alive = true},A) ->
                                 A+1;
                            (_, A) ->
                                 A
                         end, 0, MyBots)
             - length(AdjacentMy) - length(uniq(Detected)),

    io:format("(We have) Seen: ~p~n(We are) Detected: ~p~n(We are) Damaged:~p~nNAlive: ~p~n",
              [RadarSeen ++ AdjacentSeen, Detected, Damaged, NAlive]),

    %% Other supported events:
    %% Hits    = [{radar_echo, BotId, Source} ||
    %%               {radar_echo, BotId, Source} <- Events],
    %% Dies    = [{die, BotId} || {die, BotId} <- Events],
    %% Detects = [{detected, BotId} || {detected, BotId} <- Events],
    %% Damages = [{damaged, BotId, Damage}
    %%               || {damaged, BotId, Damage} <- Events],
    %% Moves   = [{move, BotId, {X, Y}} || {move, BotId, {X, Y}} <- Events],
    %% NoActs  = [{noaction, BotId} || {noaction, BotId} <- Events],

    %% TODO: Implement your logic and return, right now,
    %%       all bots are scanning and cannoning
    MyCleverAI = fun(#bot{bot_id  = BotId,
                          name    = _MyBotName,
                          team_id = _MyBotTeaMId,
                          hp      = _MyBotHP,
                          alive   = _IsMyBotAlive,
                          pos     = {MyBotX, MyBotY}}, {Actions, S}) ->

                     IAmDetected =
                       lists:member(BotId, Detected ++ Damaged ++ AdjacentMy),
                     {Action, NewState} =
                         if IAmDetected ->
                                {run_as_fast_as_you_can(BotId, MyBotX, MyBotY,
                                                       ConfigFieldRadius), S};
                            AdjacentSeen =/= [] ->
                                Where = hd(AdjacentSeen),
                                {shoot(BotId, Where), write_shoot(S, Where)};
                            RadarSeen =/= []
                            andalso (S#state.shootn < NAlive-1
                                     orelse NAlive =:= 1) ->
                                Where = hd(RadarSeen),
                                {shoot(BotId, Where), write_shoot(S, Where)};
                            RadarSeen =/= []
                            andalso S#state.shootn =:= NAlive-1 ->
                                {scanxy(BotId, hd(RadarSeen)), S};
                            true->
                                {scan(BotId, ConfigFieldRadius), S}
                         end,
                     {[Action|Actions], NewState}
                 end,
    ClearState = State#state{shoot=[], shootn = 0},
    {BotActions, NewState} = lists:foldl(MyCleverAI, {[], ClearState}, MyBots),
    erase(scatter),

    io:format("Actions: ~p~n", [BotActions]),
    {BotActions, NewState}.

write_shoot(#state{shoot = X} = State, Where) ->
    State1 =
    case lists:member(Where, X) of
        true ->
            State;
        false ->
            State#state{shoot = [Where | X]}
    end,
    State1#state{shootn = State1#state.shootn+1}.

run_as_fast_as_you_can(BotId, MyBotX, MyBotY, ConfigFieldRadius) ->
    Directions = [
                  {MyBotX,     MyBotY - 2},
                  {MyBotX + 2, MyBotY - 2},
                  {MyBotX + 2, MyBotY},
                  {MyBotX,     MyBotY + 2},
                  {MyBotX - 2, MyBotY + 2},
                  {MyBotX - 2, MyBotY}
                 ],
    AvailableDirections = [ {X,Y} ||
                            {X,Y} <- Directions,
                            inside_field(X,Y,ConfigFieldRadius)
                          ],
    {GoTo,_} = random(AvailableDirections),
    {move, BotId, GoTo}.

shoot(BotId, {WhereX, WhereY}) ->
    Scatter0 = get(scatter),
    {MyWhere, Scatter} =
    case Scatter0 of
        undefined ->
            {L0,_} = random([
                [{WhereX+1, WhereY}, {WhereX-1, WhereY+1}, {WhereX, WhereY-1}],
                [{WhereX-1, WhereY}, {WhereX+1, WhereY-1}, {WhereX, WhereY+1}]
            ]),
            L = shuffle_list(L0),
            random(L);
        _ ->
            random(Scatter0)
    end,
    put(scatter, Scatter),
    {cannon, BotId, MyWhere}.

random(List) ->
    H = lists:nth(
             random:uniform(
               length(List)
              ),
             List
            ),
    {H,List--[H]}.

shuffle_list(List) ->
   {NewList, _} = lists:foldl( fun(_El, {Acc,Rest}) ->          
       RandomEl = lists:nth( random:uniform(length(Rest)), Rest),
       {[RandomEl|Acc], lists:delete(RandomEl, Rest)}            
   end, {[],List}, List),
   NewList.

scan(BotId, ConfigFieldRadius) ->
    ScanRadius = ConfigFieldRadius - 2,
    AllCoords = [{X,Y} || X <- lists:seq(-ScanRadius, ScanRadius),
                          Y <- lists:seq(-ScanRadius, ScanRadius),
                          inside_field(X,Y,ScanRadius)],
    {{RX,RY},_} = random(AllCoords),
    {radar, BotId, {RX, RY}}.

scanxy(BotId, Location) ->
    {radar, BotId, Location}.

inside_field(X,Y,Radius) ->
    abs(X) =< Radius andalso abs(Y) =< Radius andalso abs(X+Y) =< Radius.

uniq(List) ->
    lists:foldl( fun(X, L) ->
                         case lists:member(X, L) of
                             true ->
                                 L;
                             false ->
                                 [X|L]
                         end
                 end, [], List).
