-module(dealer).
-export([start_game/0]).
-include_lib("eunit/include/eunit.hrl").

start_game() ->
    % Prep the players
    Alice = spawn_link(player, start, []),
    Bob = spawn_link(player, start, []),
    Deck = cards:make_deck(),
    distribute_cards(Deck, Alice, Bob),
    play(Alice, Bob).

distribute_cards([], _, _) ->
    io:format("Cards distributed...~n");
distribute_cards([NextCard|Deck], ThisPlayer, NextPlayer) ->
    io:format("Distributing ~p to ~p~n", [NextCard, ThisPlayer]),
    ThisPlayer ! {receive_card, NextCard},
    distribute_cards(Deck, NextPlayer, ThisPlayer).

play(ThisPlayer, NextPlayer) ->
    ThisPlayerCard = play_card_from(ThisPlayer),
    NextPlayerCard = play_card_from(NextPlayer),
    case cards_equal(ThisPlayerCard, NextPlayerCard) of
        true -> io:format("tie");
        false -> io:format("winner")
    end.

play_card_from(Player) ->
    io:format("Playing for ~p~n", [Player]),
    Player ! {play_card, self()},
    Card = receive_card(),
    io:format("Got card ~p from ~p~n", [Card, Player]),
    Card.

receive_card() ->
    receive
        {card, Card} -> Card
    end.

cards_equal({First,_}, {Second,_}) ->
    First == Second.

%%% TEST %%%

cards_equal_test() ->
    ?assertEqual(true, cards_equal({"A", "Spades"}, {"A", "Clubs"})).
