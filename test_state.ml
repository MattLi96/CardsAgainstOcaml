open State

let (test_u_s_state:univ_s_state) = {
    judge  = 1;
    played = [(2, "w10")];
    b_card = "What's that smell?";
    scores = [(1,0);(2,0);(3,0)];
    winners = None;

    (*decks*)
    b_deck = BDeck ["What's that smell?" ; "testblack1" ; "testblack2"];
    w_deck = WDeck ["w1";"w2";"w3";"w4";"w5";"w6";"w7";"w8";"w9";"w10"];

    (*List of (card, player) pairs matching cards played to users who played
    them*)
    card_to_player = [(1,None);(2,Some "w10");(3,None)];
    hands          = [(1,["w1";"w2";"w3"]);(2,["w4";"w5";"w6"]);
                      (3,["w7";"w8";"w9"])]
  }

let test_s_state = Playing test_u_s_state

let (test_u_c_state:univ_c_state) = {
    played = [(2, "w10")];
    b_card = "What's that smell?";
    scores = [(1,0);(2,0);(3,0)];
    winners = None;
    hand    = ["w4";"w5";"w6"];
  }

let test_c_state = PWaiting test_u_c_state

(*TO IMPLEMENT*)
TEST_UNIT "test get_previous_wins" = ()

TEST "test curr_black_card" =
  ((curr_black_card test_c_state) != "Whasdfsdf that smell?") &&
  ((curr_black_card test_c_state) = "What's that smell?")

TEST "test scores" =
  (scores test_c_state != [(1,2);(2,0);(3,0)]) &&
  (scores test_c_state = [(1,0);(2,0);(3,0)])

TEST "test users_played" =
  (users_played test_c_state != [1]) &&
  (users_played test_c_state = [2])

TEST "test played_white_cards" =
  (played_white_cards test_c_state != ["w13"]) &&
  (played_white_cards test_c_state = ["w10"])

TEST "test get_user_state" =
  get_user_state test_s_state 2 = test_c_state

TEST "test get_white_deck" =
  get_white_deck test_s_state = WDeck ["w1";"w2";"w3";"w4";"w5";
                                       "w6";"w7";"w8";"w9";"w10"];

TEST "test get_black_deck" =
  get_black_deck test_s_state = BDeck ["What's that smell?" ;
                                       "testblack1" ; "testblack2"];