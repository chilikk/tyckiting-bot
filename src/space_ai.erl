-module(space_ai).

-export([give_moves/6]).

-include("include/space.hrl").

%% To be changed by AI implementer:
-record(state, {some_data :: string()}).

give_moves(RoundId, Config, Team, OtherTeams, Events, state_uninitialized) ->
    State = #state{some_data = "Initial state!"},
    give_moves(RoundId, Config, Team, OtherTeams, Events, State);

give_moves(RoundId,
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
           #state{some_data = SomeData} = State) ->

    MyBotIds = [MB#bot.bot_id || MB <- MyBots],

    %% Current AI state to keep information
    %% not given by the server (my own memory)
    io:format("Current state: ~p~n", [SomeData]),

    %% Some modifications to the local state, information
    %% that is not provided by the server.
    NewState = State#state{some_data = "Our grand plan is to win!"},

    %% All hostile bots seen:
    RadarSeen    = [Pos || {radar_echo, Pos} <- Events],
    AdjacentSeen = [Pos || {see, _Source, BotId, Pos} <- Events,
                           not lists:member(BotId, MyBotIds)],

    Detected = [ DetectedBot || {detected, DetectedBot} <- Events ] ++
               [ DetectedBot || {see, DetectedBot, BotId, _} <- Events,
                                lists:member(DetectedBot, MyBotIds),
                                not lists:member(BotId, MyBotIds) ],

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
                          pos     = {MyBotX, MyBotY}}) ->
                     IAmDetected = lists:member(BotId, Detected),
                     if IAmDetected ->
                            io:format("Detected ~p:~p~n", [MyBotX, MyBotY]),
                            run_as_fast_as_you_can(BotId, MyBotX, MyBotY,
                                                   ConfigFieldRadius);
                        AdjacentSeen =/= [] ->
                            shoot(BotId, hd(AdjacentSeen));
                        RadarSeen =/= [] ->
                            shoot(BotId, hd(RadarSeen));
                        true->
                            scan(BotId, ConfigFieldRadius)
                     end
                 end,
    BotActions = lists:map(MyCleverAI, MyBots),
    erase(scatter),
    {space:mk_actions(RoundId, BotActions), NewState}.

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
    space:mk_action(<<"move">>,
                    BotId,
                    GoTo).

shoot(BotId, {WhereX, WhereY}) ->
    Scatter0 = get(scatter),
    {MyWhere, Scatter} =
    case Scatter0 of
        undefined ->
            {L,_} = random([
                [{WhereX+1, WhereY}, {WhereX-1, WhereY+1}, {WhereX, WhereY-1}],
                [{WhereX-1, WhereY}, {WhereX+1, WhereY-1}, {WhereX, WhereY+1}]
            ]),
            random(L);
        _ ->
            random(Scatter0)
    end,
    put(scatter, Scatter),
    space:mk_action(<<"cannon">>, BotId, MyWhere).

random(List) ->
    H = lists:nth(
             random:uniform(
               length(List)
              ),
             List
            ),
    {H,List--[H]}.

scan(BotId, ConfigFieldRadius) ->
    ScanRadius = ConfigFieldRadius - 2,
    AllCoords = [{X,Y} || X <- lists:seq(-ScanRadius, ScanRadius),
                          Y <- lists:seq(-ScanRadius, ScanRadius),
                          inside_field(X,Y,ScanRadius)],
    {{RX,RY},_} = random(AllCoords),
    space:mk_action(<<"radar">>,
                    BotId,
                    {RX, RY}).

inside_field(X,Y,Radius) ->
    abs(X) =< Radius andalso abs(Y) =< Radius andalso abs(X+Y) =< Radius.
