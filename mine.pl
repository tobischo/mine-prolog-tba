%(c) David Hildenbrand, Tobias Schoknecht

%static configuration data
%general object information
object(fuse_cord).
object(desk).
object(map).
object(flashlight).
object(box).
object(dynamite).
object(pickaxe).
object(fuse).
object(mine_chariot).
object(helmet).
object(pouch).
object(goldnugget).
object(key).
object(shovel).
object(bunch_of_rocks).
object(railway_switch).
object(usable_fuse).
object(dynamite_with_fuse).

%objects that can contain another object
container(desk).
container(box).
container(pouch).

%general location definition
location(exit).
location(tunnel1).
location(tunnel2).
location(tunnel3).
location(tunnel4).
location(break_chamber).
location(gold_vein_chamber).
location(silver_vein_chamber).

%connections between locations
simpleConn(exit, tunnel1).
simpleConn(tunnel1, tunnel2).
simpleConn(tunnel1, break_chamber).
simpleConn(tunnel2, silver_vein_chamber).
simpleConn(tunnel2, gold_vein_chamber).
simpleConn(tunnel2, tunnel3).
simpleConn(tunnel3, tunnel4).

%takeable items
takeable(fuse).
takeable(fuse_cord).
takeable(usable_fuse).
takeable(dynamite_with_fuse).
takeable(map).
takeable(flashlight).
takeable(pickaxe).
takeable(dynamite).
takeable(helmet).
takeable(pouch).
takeable(goldnuggets).
takeable(key).
takeable(shovel).

%usable items
usable(dynamite_with_fuse).
usable(key).

%combinable items
combinable(dynamite, usable_fuse, dynamite_with_fuse).
combinable(fuse, fuse_cord, usable_fuse).
bothWayCombinable(X, Y, Z) :-combinable(X, Y, Z), 
  !.
bothWayCombinable(X, Y, Z) :-combinable(Y, X, Z).

%object stores are containers, locations and inventory
object_store(inventory).
object_store(X) :-container(X).
object_store(X) :-location(X).

%init basic game info
init_basic :-
  asserta(game_state(started)), 
  asserta(position(tunnel4)), 
  asserta(blocked(tunnel1, exit)).

%initialize items
init_items :-
  asserta(contains(flashlight, inventory)), 
  asserta(contains(fuse_cord, break_chamber)), 
  asserta(contains(desk, break_chamber)), 
  asserta(contains(map, tunnel4)), %todo restore to desk
  asserta(contains(box, gold_vein_chamber)), 
  asserta(contains(dynamite, box)), 
  asserta(contains(pickaxe, gold_vein_chamber)), 
  asserta(contains(fuse, silver_vein_chamber)), 
  asserta(contains(mine_chariot, silver_vein_chamber)), 
  asserta(contains(helmet, silver_vein_chamber)), 
  asserta(contains(pouch, tunnel4)), 
  asserta(contains(goldnugget, pouch)), 
  asserta(contains(key, pouch)), 
  asserta(contains(shovel, tunnel4)), 
  asserta(contains(bunch_of_rocks, tunnel3)), 
  asserta(contains(railway_switch, tunnel2)).

%easy access to the inventory
inventory(X) :-
  contains(X, inventory).

add_to_inventory(X) :-
  asserta(contains(X, inventory)), 
  write(X), 
  writeln(' was added to the inventory.').

remove_from_inventory(X) :-
  retract(contains(X, inventory)), 
  write(X), 
  writeln(' was removed from the inventory.').

print_inventory :-
  inventory(_)->
    (writeln('Your inventory contains:'), 
    print_inventory_list;
    !);
  writeln('Your inventory is empty.').

print_inventory_list :-
  inventory(X), 
  tab, 
  writeln(X), 
  fail.

%combine two items and add the result to the inventar
combine(X, Y) :-
  inventory(X), 
  inventory(Y), 
  bothWayCombinable(X, Y, Z), 
  add_to_inventory(Z), 
  remove_from_inventory(X), 
  remove_from_inventory(Y), 
  !.
combine(X, Y) :-
  not(object(X)), 
  write(X), 
  write(' is not an object.'), 
  !.
combine(X, Y) :-
  not(object(Y)), 
  write(Y), 
  write(' is not an object.'), 
  !.
combine(X, Y) :-
  not(inventory(X)), 
  write(X), 
  write(' is not in your inventory'), 
  !.
combine(X, Y) :-
  not(inventory(Y)), 
  write(Y), 
  write(' is not in your inventory'), 
  !.
combine(X, Y) :-
  inventory(X), 
  inventory(Y), 
  not(bothWayCombinable(X, Y, Z)), 
  write('Cannot combine '), 
  write(X), 
  write(' and '), 
  write(Y).

%print map
printMap :-
  writeln('   /--------------|--------------------------|------'), 
  writeln('  / break_chamber |      tunnel1             | exit'), 
  writeln('  |         /-----|------------/   /---------|------'), 
  writeln('  |        /                  /   /'), 
  writeln('  \\-------/                  /---/'), 
  writeln('                            /   /'), 
  writeln(' /------------------|------/    |'), 
  writeln('/ gold_vein_chamber | tunnel2   \\'), 
  writeln('|              /----|---/   /\\   \\'), 
  writeln('\\-------------/        /   /  \\   \\'), 
  writeln('                      /---/    \\---\\'), 
  writeln('                     /   /      \\   \\-------\\'), 
  writeln('                    /   /        |           \\--------\\'), 
  writeln('           tunnel3--|-  |        | silver_vein_chamber \\'), 
  writeln('                    |   |        |                     |'), 
  writeln('                    /   /        \\                     /'), 
  writeln('       /-----------/---/          \\-------------------/'), 
  writeln('       |  tunnel4     /'), 
  writeln('       |   /---------/'), 
  writeln('       \\---/').

%init
init :-
  init_basic, 
  init_items, 
  init_text.

%start text
init_text :-
  write('You are exploring an old mine.'), 
  nl, 
  write('A sudden tremor occured and some rocks are falling down.'), 
  nl, 
  write('You are heading for the exit.'), 
  nl, 
  nl, 
  write('It is blocked.'), 
  nl, 
  print_pos->
    true;
  true.

%print position and possible ways
print_pos :-
  position(X), 
  write('You are in '), 
  write(X), 
  write('.'), 
  nl, 
  print_possible_paths->
    true;
  true.

print_possible_paths :-
  position(X), 
  writeln('You can go to the following areas: '), 
  connection(X, Y), 
  tab, 
  writeln(Y), 
  fail.

%take from current position
take(X) :-
  position(Z), 
  take_from_any(X, Z)->
    true:
  true.

%take from any object store except the inventory 
take_from_any(X, inventory) :-
  writeln('Cannot take something which is already in the inventory!'), 
 !.
take_from_any(_, X) :-
  not(container(Z)), 
  not(location(Z)), 
  write(Z), 
  writeln(' is not a valid place for an object.'), 
  !.
take_from_any(X, _) :-
  not(object(X)), 
  write(X), 
  writeln(' is not a valid object!'), 
  !.
take_from_any(X, _) :-
  not(takeable(X)), 
  write(X), 
  writeln(' cannot be carried around!'), 
  !.
take_from_any(X, Z) :-
  not(contains(X, Z)), 
  write(X), 
  write(' not found at '), 
  write(Z), 
  writeln('!'), 
  !.
take_from_any(X, Z) :-
  container(Z), 
  retract(contains(X, Z)), 
  add_to_inventory(X), 
  !.
take_from_any(X, Z) :-
  position(Z), 
  retract(contains(X, Z)), 
  add_to_inventory(X), 
  !.
take_from_any(X, Z) :-
  write(X), 
  writeln(' is not reachable from here!'), 
  !.

%put something back
put(X) :-
  position(Z), 
  put_to_any(X, Z)->
    true:
  true.

%put to any position or container
put_to_any(X, inventory) :-
  not(inventory(X)), 
  write(X), 
  writeln(' is not in the inventory.'), 
  !.
put_to_any(X, Y) :-
  not(container(Y)), 
  not(location(Y)), 
  write(Y), 
  writeln(' is not a valid place for an object.'), 
  !.
put_to_any(X, Y) :-
  container(Y), 
  position(Z), 
  not(contains(Y, Z)), 
  not(inventory(Y)), 
  writeln('Cannot beam objects to containers which are not around here.'), 
  !.
put_to_any(X, Y) :-
  container(Y), 
  asserta(contains(X, Y)), 
  remove_from_inventory(X), 
  write('Put '), 
  write(X), 
  write(' to '), 
  write(Y), 
  writeln('.'), 
  !.
put_to_any(X, Y) :-
  not(position(Y)), 
  writeln('Cannot beam objects to places which are not around.'), 
  !.
put_to_any(X, Y) :-
  position(Y), 
  asserta(contains(X, Y)), 
  remove_from_inventory(X), 
  write('Put '), 
  write(X), 
  write(' to '), 
  write(Y), 
  writeln('.'), 
  !.
put_to_any(X, Y) :-
  write('Cannot place '), 
  write(X), 
  writeln(' here').

%use an item which is in the inventory or at the current position
use(X) :-
  not(object(X)), 
  write(X), 
  writeln(' is not a valid object'), 
  !.
use(X) :-
  not(usable(X)), 
  write('Cannot use '), 
  write(X), 
  writeln('.'), 
  !.
use(X) :-
  not(inventory(X)), 
  position(Z), 
  not(contains(X, Z)), 
  write(X), 
  writeln(' is not reachable from here.'), 
  !.

%ignite the dynamite to remove the blockage
fire :-
  position(X), 
  not(contains(dynamite_with_fuse, X)), 
  write('Cannot fire the dynamite from the current location.'), 
  !.
fire :-
  inventory(dynamite_with_fuse), 
  write('Cannot fire the dynamite while it is still in the inventory.'), 
  !.
fire :-
  (inventory(dynamite);
  inventory(fuse);
  inventory(fuse_cord))->
    write('You have to combine dynamite, fuse and fuse_cord first.'), 
    !.
fire :-
  position(X), 
  not(way_blocked(X, _)), 
  write('There is no blockage here.'), 
  !.
fire :-
  position(X), 
  way_blocked(X, Y), 
  retract(contains(dynamite_with_fuse), X), 
  (retract(blocked(X, Y));
  retract(blocked(Y, X))), 
  !.
fire :-
  write('Cannot fire dynamite.'), 
  !.

%look around
look_around :-
  position(X), 
  contains(Y, X)->
    writeln('You can see:'), 
    print_viewable_objects;
  writeln('Nothing in here!'), 
  true.

print_viewable_objects :-
  position(X), 
  contains(Y, X), 
  tab, 
  writeln(Y), 
  fail.

%examine objects
examine_object(Y) :-
  not(object(Y)), 
  write(Y), 
  writeln(' is not a valid object.'), 
  !.
examine_object(Y) :-
  not(inventory(Y)), 
  position(Z), 
  not(contains(Y, Z)), 
  write(Y), 
  writeln(' is not reachable from here.'), 
  !.
examine_object(Y) :-
  not(container(Y)), 
  write(Y), 
  writeln(' cannot contain anything.'), 
  !.
examine_object(Y) :-
  contains(_, Y)->
    write(Y), 
    writeln(' contains the following objects:'), 
    examine(Y);
  write(Y),
  writeln(' is empty.').
examine(Y) :-
  position(X), 
  contains(Y, X), 
  examine_object_list(Y)->
    true:
  true.
examine(Y) :-
  inventory(Y), 
  examine_object_list(Y)->
    true:
  true.
examine_object_list(Y) :-
  contains(Z, Y), 
  tab, 
  writeln(Z), 
  fail.

%two way connections
connection(X, Y) :-
  simpleConn(X, Y).
connection(X, Y) :-
  simpleConn(Y, X).

%two way block check
way_blocked(X, Y) :-
  blocked(X, Y).
way_blocked(X, Y) :-
  blocked(Y, X).

%movement rules
movement_rule(X, Y) :-
  not(way_blocked(X, Y)).
movement_rule(X, Y) :-
  way_blocked(X, Y), 
  blocked_reason(X, Y), 
  fail.

%detailed block reason
blocked_reason(tunnel1, exit) :-
  write('The way is blocked. You can smell'),
  write(' the fresh air streaming in the tunnel. '),
  writeln('Find a way to remove the stones.'), 
  writeln('You shall not pass!').

%two way movement rules
mv_r(X, Y) :-
  movement_rule(X, Y).
mv_r(X, Y) :-
  movement_rule(Y, X).

single_move(X, Y) :-
  connection(X, Y), 
  mv_r(X, Y), 
  writeln('changing the room'), 
  retract(position(Y)), 
  asserta(position(X)).

%move action
goto(X) :-
  not(location(X)), 
  write(X), 
  writeln(' is not a valid location.').
goto(X) :-
  location(X), 
  position(Y), 
  not(connection(X, Y)), 
  write('You cannot go to '), 
  write(X), 
  writeln(' directly.').
goto(X) :-
  location(X), 
  position(Y), 
  single_move(X, Y), 
  print_pos.

%initialize Game
start_game :-
  init, 
  start_interactive_mode.

%restart the game
restart_game :-
  retract(game_state(_)), 
  retract(position(_)), 
  retract(blocked(_, _)), 
  retract(contains(_, _)), 
  start_game.

%main loop
start_interactive_mode :-
  repeat, 
    read_command(Y), 
    process_command(Y), 
    game_state(finished), 
    print_end_message, 
  halt.

%process a command
process_command(Y) :-
  preprocess_command(Y, Z), 
  command(Z)->
    true;
  writeln('What makes sis command?! Enter help for more information.').

%string_to_lower([], []).
%string_to_lower([U|UT], [L|LT]) :-writeln(U), not(between(65, 90, U)), L is U, string_to_lower(U, LT).
%string_to_lower([U|UT], [L|LT]) :-writeln(U), between(65, 90, U)->L is U+32, string_to_lower(U, LT).

%preprocess the command
preprocess_command(A, B) :-
  command_lowercase(A, T), 
  command_clean(T, B), 
  write('Processed command: '), 
  writeln(B).

%convert words to lower case
command_lowercase([], []).
command_lowercase([A|B], [C|D]) :-
  downcase_atom(A, C), 
  command_lowercase(B, D).

%remove words that will be ignored from the list
command_clean([A|B], C) :-
  ignored_word(A), 
  command_clean(B, C).
command_clean([], []).
command_clean([A|B], [A|C]) :-
  not(ignored_word(A)), 
  command_clean(B, C).

%the words that will be filtered out
ignored_word(';').
ignored_word(', ').
ignored_word('.').
ignored_word('!').
ignored_word('?').
ignored_word('the').
ignored_word('a').
ignored_word('this').
ignored_word('that').
ignored_word('hell').
ignored_word('heck').
ignored_word('fuck').
ignored_word('fuckin').
ignored_word('fucking').
ignored_word('and').
ignored_word('to').
ignored_word('into').
ignored_word('onto').
ignored_word('out').
ignored_word('of').
ignored_word('from').

%end the game
command([stop]) :-
  command([end]).
command([quit]) :-
  command([end]).
command([halt]) :-
  command([end]).
command([end]) :-
  retract(game_state(_)), 
  asserta(game_state(finished)).

%reload the file - for debugging
command([reload]) :-
  writeln('Reloading file "mine.pl"'), 
  consult(mine), 
  restart_game.

%restart the game
command([restart]) :-
  restart_game.

%display the help message
command([help]) :-
  writeln('Available commands are:'), 
  tab,
  writeln('"help" - Display this help'), 
  tab, 
  writeln('"end" - End the game'), 
  tab, 
  writeln('"go to X" - Go to position X'), 
  tab, 
  writeln('"where am I" - Where the hell are you?'), 
  tab, 
  writeln('"look around" - Take a look around.'), 
  tab, 
  writeln('"reload" - Reload the file (debugging purpose)'), 
  tab, 
  writeln('"restart" - Restart the game'), 
  tab, 
  writeln('"take X" - Take something'), 
  tab, 
  writeln('"take X out of Y" - Take something out of something'), 
  tab,
  writeln('"put X into Y" - Put something into something.'), 
  tab,
  writeln('"drop X" - Drop something'), 
  tab,
  writeln('"use X" - Use something'), 
  tab, 
  writeln('"combine X Y" - Combine two things creating a third one.'), 
  tab, 
  writeln('"examine X" - Examine an object.'), 
  tab, 
  writeln('"show map" - Display the map.'), 
  tab, 
  writeln('"show inventoy" - Display the inventory.').
command([go|[to, R]]) :-
  goto(R);
  true.

%display location information
command([where, am, i]) :-
  print_pos;
  true.
                      
%take a look around
command([look, around]) :-
  look_around;
  true.

%take something
command([take, R]) :-
  take(R);
  true.
command([take, R, Z]) :-
  take_from_any(R, Z);
  true.

%drop something
command([drop, R]) :-
  put(R);
  true.

%put something to some destination
command([put, A, B]) :-
  put_to_any(A, B);
  true. 

%use something
command([use, R]) :-
  use(R);
  true.

%combine objects
command([combine, A, B]) :-
  combine(A, B);
  true.

%examine something
command([examine, R]) :-
  examine_object(R);
  true.

%show the map
command([map]) :-
  command([show, map]).
command([show, map]) :-
  inventory(map)->
    printMap;
  writeln('Hard to show a non-existing map. Use your imagination!'), 
  true.

%show the inventory
command(['inventory']) :-
  command([show, 'inventory']).
command([show, 'inventory']) :-
  print_inventory.

tab :-writef('\t').
print_end_message :-
  writeln('End of game. Thank you for playing!').

%read in the user command
read_command(Y) :-
  write('--> '), 
  readln(Y).

% Alles für die Katz :(
%read_word_list(Y) :-read_word(W)->read_word_list(L), append([W], L, Y), Y == [].
%read a word which contains more than one character
%read_word(W) :-read_char_list([], Y), length(Y, L), L =\= 0, writeln(Y), atom_chars(W, Y).
%read a list of chars until a space is found
%read_char_list(X, Y) :-read_char(C), C =\= 32->append(X, [C], Y2), read_char_list(Y2, Y);X = Y.
%read a char which is not the end of the string
%read_char(Y) :-get0(Y), Y =\= 10.
