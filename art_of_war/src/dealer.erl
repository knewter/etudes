-module(dealer).
-export([start_game/0]).
-include_lib("eunit/include/eunit.hrl").

-record(state, {players,
                mode=pre_battle,
                alice_cards=[],
                bob_cards=[]}).

start_game() ->
    % Prep the players
    Alice = spawn_link(player, start, []),
    Bob = spawn_link(player, start, []),
    OrderedDeck = cards:make_deck(),
    Deck = cards:shuffle(OrderedDeck),
    %Deck = OrderedDeck,
    State = #state{players=[Alice,Bob]},
    distribute_cards(Deck, Alice, Bob, State),
    play(State).

distribute_cards([], _, _, _) ->
    io:format("Cards distributed...~n");
distribute_cards([NextCard|Deck], ThisPlayer, NextPlayer, State) ->
    io:format("Distributing ~p to ~p~n", [NextCard, player_name(ThisPlayer, State)]),
    ThisPlayer ! {receive_card, NextCard},
    distribute_cards(Deck, NextPlayer, ThisPlayer, State).

play(State=#state{players=[Alice,Bob], mode=pre_battle}) ->
    io:format("--------Starting new battle--------~n"),
    AliceCard = play_card_from(Alice, State),
    BobCard = play_card_from(Bob, State),
    NewState = add_cards_to_state([AliceCard], [BobCard], State),
    determine_winner(NewState);
play(State=#state{players=[Alice,Bob], mode=war}) ->
    AliceCardsRaw = [play_card_from(Alice, State), play_card_from(Alice, State), play_card_from(Alice, State)],
    BobCardsRaw = [play_card_from(Bob, State), play_card_from(Bob, State), play_card_from(Bob, State)],
    % Don't include 'out' cards in card lists
    AliceCards = lists:filter(fun(Card) -> Card =/= out end, AliceCardsRaw),
    BobCards = lists:filter(fun(Card) -> Card =/= out end, BobCardsRaw),
    NewState = add_cards_to_state(AliceCards, BobCards, State),
    determine_winner(NewState).

determine_winner(State=#state{players=[Alice,Bob]}) ->
    AliceCard = lists:last(State#state.alice_cards),
    BobCard = lists:last(State#state.bob_cards),
    if
        AliceCard == out -> announce_winner(Bob, State);
        BobCard == out -> announce_winner(Alice, State);
        true -> case cards_equal(AliceCard, BobCard) of
                    true -> handle_tie(State);
                    false -> handle_winner(State)
                end
    end.

announce_winner(Player, State) ->
    io:format("Woot, ~p won!~n", [player_name(Player, State)]).

handle_tie(State) ->
    NewState = State#state{mode=war},
    io:format("lol tie, now in war.~n"),
    play(NewState).

handle_winner(State=#state{players=[Alice,Bob]}) ->
    AliceCards = State#state.alice_cards,
    BobCards = State#state.bob_cards,
    AliceCard = lists:last(AliceCards),
    BobCard = lists:last(BobCards),
    NewState = State#state{alice_cards=[], bob_cards=[], mode=pre_battle},
    Winner = case value(AliceCard) > value(BobCard) of
                  true -> Alice;
                  false -> Bob
              end,
    distribute_cards_to_winner(AliceCards ++ BobCards, Winner),
    io:format("winner: ~p~n", [player_name(Winner, NewState)]),
    play(NewState).

play_card_from(Player, State) ->
    io:format("Playing for ~p~n", [player_name(Player, State)]),
    Player ! {play_card, self()},
    Card = receive_card(),
    io:format("Got card ~p from ~p~n", [Card, player_name(Player, State)]),
    Card.

add_cards_to_state(AliceCards, BobCards, State) ->
    NewAliceCards = State#state.alice_cards ++ AliceCards,
    NewBobCards = State#state.bob_cards ++ BobCards,
    State#state{alice_cards=NewAliceCards, bob_cards=NewBobCards}.

distribute_cards_to_winner([], _) ->
    ok;
distribute_cards_to_winner([Card|Rest], Winner) ->
    Winner ! {receive_card, Card},
    distribute_cards_to_winner(Rest, Winner).

receive_card() ->
    receive
        {card, Card} -> Card
    end.

cards_equal({First,_}, {Second,_}) ->
    First == Second.

value({Value,_}) ->
    if
        Value == out -> 0;
        Value == "A" -> 1;
        Value == "J" -> 11;
        Value == "Q" -> 12;
        Value == "K" -> 13;
        true -> Value
    end.

player_name(Pid, #state{players=[Alice,_]}) ->
    case Pid == Alice of
        true -> "Alice";
        false -> "Bob"
    end.

%%% TEST %%%
cards_equal_test() ->
    ?assertEqual(true, cards_equal({"A", "Spades"}, {"A", "Clubs"})).

value_test() ->
    ?assertEqual(1, value({"A", "Spades"})),
    ?assertEqual(2, value({2, "Spades"})).

player_name_test() ->
    ?assertEqual("Alice", player_name(foo, #state{players=[foo,bar]})),
    ?assertEqual("Bob", player_name(bar, #state{players=[foo,bar]})).
