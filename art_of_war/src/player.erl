-module(player).
-export([start/0]).
-include_lib("eunit/include/eunit.hrl").

start() ->
    await([]).

await(Cards) ->
    receive
        {receive_card, Card} -> await(Cards ++ [Card]);
        {divulge_cards, Pid} ->
            Pid ! {cards, Cards},
            await(Cards);
        {play_card, Pid} when erlang:length(Cards) == 0 ->
            Pid ! {card, out},
            await(Cards);
        {play_card, Pid} ->
            [Card|RemainingCards] = Cards,
            Pid ! {card, Card},
            await(RemainingCards)
    end.

%%% TEST %%%
receiving_card_test() ->
    Player = spawn_link(player, start, []),
    Player ! {receive_card, "A"},
    verify_hand_is(["A"], Player),
    Player ! {receive_card, "B"},
    verify_hand_is(["A", "B"], Player).

playing_a_card_test() ->
    Player = spawn_link(player, start, []),
    Player ! {receive_card, "A"},
    Player ! {receive_card, "B"},
    verify_next_card_played("A", Player),
    verify_hand_is(["B"], Player),
    verify_next_card_played("B", Player),
    verify_hand_is([], Player),
    verify_next_card_played(out, Player).

verify_hand_is(ExpectedCards, Player) ->
    Player ! {divulge_cards, self()},
    receive
        {cards, Cards} -> ?assertEqual(ExpectedCards, Cards)
    end.

verify_next_card_played(ExpectedCard, Player) ->
    Player ! {play_card, self()},
    receive
        {card, Card} -> ?assertEqual(ExpectedCard, Card)
    end.
