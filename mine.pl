%(c) David Hildenbrand, Tobias Schoknecht

%static configuration data
%general object information
object(desk).
object(map).
object(flashlight).
object(box).
object(dynamite).
object(pickaxe).
object(mine_chariot).
object(helmet).
object(pouch).
object(goldnugget).
object(key).
object(shovel).
object(bunch_of_rocks).
object(railway_switch).
object(fused_dynamite).
object(fuse_cord).
object(fuse).
object(flute).

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
takeable(fused_dynamite).
takeable(map).
takeable(flashlight).
takeable(pickaxe).
takeable(dynamite).
takeable(helmet).
takeable(pouch).
takeable(goldnuggets).
takeable(key).
takeable(shovel).
takeable(flute).

%usable items
usable(fuse).
usable(key).
usable(flute).
usable(railway_switch).
usable(flashlight).

%combinable items
combinable(dynamite, fuse_cord, fused_dynamite).

bothWayCombinable(X, Y, Z) :-
  combinable(X, Y, Z), 
  !.
bothWayCombinable(X, Y, Z) :-
  combinable(Y, X, Z).

%object stores are containers, locations and inventory
object_store(inventory).
object_store(X) :-
  container(X).
object_store(X) :-
  location(X).

%message for blocked locations and objects
blocked_message(desk) :-
  writeln('The desk is locked. You need a key to open the desk.'), 
  !.
blocked_message(exit) :-
  write('The way is blocked. You can smell'), 
  write(' the fresh air streaming in the tunnel. '), 
  writeln('Find a way to remove the stones.'), 
  writeln('You shall not pass!'), 
  !.
blocked_message(_).

%detailed block reason
blocked_way_message(_, exit) :- 
  blocked_message(exit).
blocked_way_message(exit, _) :- 
  blocked_message(exit).

%init basic game info
init_basic :-
  asserta(light_state(off)),
  asserta(game_state(started)), 
  asserta(position(tunnel1)), 
  asserta(blocked(tunnel1, exit)), 
  asserta(blocked_object(desk)).

%initialize items
init_items :-
  asserta(contains(flashlight, inventory)), 
  asserta(contains(fuse_cord, desk)), 
  asserta(contains(desk, break_chamber)), 
  asserta(contains(map, break_chamber)), 
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
  asserta(contains(flute, tunnel1)), 
  asserta(contains(railway_switch, tunnel2)). 

%easy access to the inventory
inventory(X) :-
  contains(X, inventory).

%add something to the inventory
add_to_inventory(X) :-
  asserta(contains(X, inventory)), 
  write(X), 
  writeln(' was added to the inventory.').

%remove something from the inventory

remove_from_inventory(X) :-
  remove_from_inventory_action(X),
  retract(contains(X, inventory)), 
  write(X), 
  writeln(' was removed from the inventory.').

%light will be turned of when removed from inventory
remove_from_inventory_action(flashlight):-
  set_light_state(off),
  !.
%catch-all for objects without an action
remove_from_inventory_action(_).

%print the content of the inventory
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

%update the game state
set_game_state(X) :- 
  retractall(game_state(_)),
  asserta(game_state(X)).

%update the flashlight state
set_light_state(X) :-
  retractall(light_state(_)),
  asserta(light_state(X)).

%combine two items and add the result to the inventar
combine(X, Y) :-
  inventory(X), 
  inventory(Y), 
  bothWayCombinable(X, Y, Z), 
  add_to_inventory(Z), 
  remove_from_inventory(X), 
  remove_from_inventory(Y), 
  !.
combine(X, _) :-
  not(object(X)), 
  write(X), 
  writeln(' is not an object.'), 
  !.
combine(_, Y) :-
  not(object(Y)), 
  write(Y), 
  writeln(' is not an object.'), 
  !.
combine(X, _) :-
  not(inventory(X)), 
  write(X), 
  writeln(' is not in your inventory.'), 
  !.
combine(_, Y) :-
  not(inventory(Y)), 
  write(Y), 
  writeln(' is not in your inventory.'), 
  !.
combine(X, Y) :-
  inventory(X), 
  inventory(Y), 
  not(bothWayCombinable(X, Y, _)), 
  write('Cannot combine '), 
  write(X), 
  write(' and '), 
  write(Y), 
  writeln('.').

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
  line,
  write('You are exploring an old mine.'), 
  nl, 
  write('A sudden tremor occured and some rocks are falling down.'), 
  nl, 
  write('You are heading for the exit.'), 
  nl, 
  nl, 
  write('It is blocked.'), 
  nl,
  line,
  print_pos->
    true;
  true.

%print position and possible ways
print_pos :-
  light_state(L),
  print_position(L).

print_position(on) :-
  position(X), 
  write('You are in '), 
  write(X), 
  write('.'), 
  nl, 
  print_possible_paths;
    true.

print_position(off) :-
  light_state(off),
  writeln('It is pitch dark in here. You can\'t see anything.').

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
    true;
  true.

%take from any object store except the inventory 
take_from_any(_, X) :- 
  location(X), 
  not(position(X)), 
  writeln('Cannot take objects out of different locations!'), 
  !.
take_from_any(_, X) :-
  blocked_object(X), 
  blocked_message(X), 
  !.
take_from_any(_, inventory) :-
  writeln('Cannot take something which is already in the inventory!'), 
 !.
take_from_any(_, Z) :-
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
take_from_any(X, _) :-
  write(X), 
  writeln(' is not reachable from here!'), 
  !.

%put something back
put(X) :-
  position(Z), 
  put_to_any(X, Z)->
    true;
  true.

put_message(fused_dynamite, Y) :-
  location(Y), 
  writeln('Use the fuse in your inventory to blow the dynamite.'), !.

put_message(X, Y) :-
  write('Put '), 
  write(X), 
  write(' to '), 
  write(Y), 
  writeln('.'). 
 
%put to any position or container
put_to_any(_, X) :- 
  blocked_object(X), 
  blocked_message(X), 
  !.
put_to_any(X, inventory) :-
  contains(X,inventory),
  write(X),
  writeln(' is already in your inventory'),
  !.
put_to_any(X, _) :-
  not(object(X)), 
  write(X), 
  writeln(' is not a valid object.'), 
  !.
put_to_any(_, Y) :-
  not(container(Y)), 
  not(location(Y)), 
  write(Y), 
  writeln(' is not a valid place for an object.'), 
  !.
put_to_any(_, Y) :-
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
  put_message(X, Y), 
  !.
put_to_any(_, Y) :-
  not(position(Y)), 
  writeln('Cannot beam objects to places which are not around.'), 
  !.
put_to_any(X, Y) :-
  position(Y), 
  asserta(contains(X, Y)), 
  remove_from_inventory(X), 
  put_message(X, Y), 
  !.
put_to_any(X, _) :-
  write('Cannot place '), 
  write(X), 
  writeln(' here').

%use an item which is in the inventory or at the current position
use(X) :-
  not(object(X)), 
  write(X), 
  writeln(' is not a valid object.'), 
  !.
use(X) :-
  not(usable(X)), 
  writeln('I really don\'t know what to do with this thing!'), 
  !.
use(X) :-
  not(inventory(X)), 
  position(Z), 
  not(contains(X, Z)), 
  write(X), 
  writeln(' is not reachable from here.'), 
  !.
use(flute) :-
  writeln('You\'re playing the most awesome melody ever!'), 
  !.
use(railway_switch) :-
  writeln('Your trying the best you can but it won\'t move.'),
  !.
%%%%%%%%%%fuse action
use(fuse) :-
  contains(fused_dynamite, Y), 
  location(Y), 
  position(Y), 
  set_game_state(failed),
  writeln('Daaaaaaaaaaaaaamn, you blew yourself! ( ;) )'), 
  !.
use(fuse) :-
  contains(fused_dynamite, Y), 
  location(Y), 
  not(position(Y)), 
  Y \= tunnel1, 
  %remove all objects in the room
  retractall(contains(_, Y)),
  writeln('Wow, that was an explosion! But the exit is still blocked!'), 
  !.
use(fuse) :-
  contains(fused_dynamite, Y), 
  not(position(Y)), 
  Y = tunnel1, 
  retractall(contains(_, Y)), 
  writeln('Kabooooooooooom, what an explosion!'), 
  retract(blocked(tunnel1, exit)), 
  !.
use(fuse) :-
  writeln('Place the fused_dynamite in a room to be able to use the fuse!'), 
  !.
%%%%%%%%%%%

%flashlight will only be usable in the inventory
use(flashlight) :-
  inventory(flashlight)->
    writeln('Wow, light!'), 
    set_light_state(on),
    !;
  writeln('The flashlight needs to be in your inventory to use it!').

%catch all: usable object, but needs something to use it on
use(_) :-
  writeln('You need something to apply this object on.').
  

%use an object onto another object
use_on(X, _) :- 
  not(object(X)), 
  write(X), 
  writeln(' is not a valid object'), 
  !.
use_on(X, _) :- 
  not(inventory(X)), 
  write(X), 
  writeln(' is not in your inventory.'),
  !.
use_on(X, Y) :-
  not(usable(X)), 
  write('I really don\'t know how to use '), 
  write(X), 
  write(' on '), 
  write(Y), 
  writeln('.'), 
  !.
use_on(_, Y) :-
  not(object(Y)), 
  write(Y), 
  writeln(' is not a valid object'), 
  !.
use_on(X, Y) :- 
  position(Z), 
  contains(Y, Z), 
  use_on_object(X, Y), 
  !.
use_on(X, Y) :-
  inventory(Y), 
  use_on_object(X, Y), 
  !.
use_on(_, Y) :-
  write(Y), 
  writeln(' is not reachable from here'), 
  !.

use_on_object(key, desk) :-
  retract(blocked_object(desk))->
    writeln('Desk unlocked!');
  writeln('Desk is already unlocked!').
use_on_object(X, Y) :-
  write('Don\'t know how to use '), 
  write(X), 
  write(' on '), 
  write(Y), 
  writeln('.'), 
  !.

%look around
look_around :-
  light_state(L),
  look_around_ext(L).

look_around_ext(on) :-
  position(X), 
  contains(_, X)->
    writeln('You can see:'), 
    print_viewable_objects;
  writeln('Nothing in here!'), 
  true.

look_around_ext(off) :-
  writeln('Still dark. Can\'t see a thing!').

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
examine_object(X) :-
  blocked_object(X), 
  blocked_message(X), 
  !.
examine_object(Y) :-
  not(container(Y)), 
  write('The '),
  write(Y), 
  writeln(' is really a beautiful object!'), 
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
    true;
  true.
examine(Y) :-
  inventory(Y), 
  examine_object_list(Y)->
    true;
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

%two way movement rules
mv_r(X, Y) :-
  movement_rule(X, Y).
mv_r(X, Y) :-
  movement_rule(Y, X).

single_move(X, Y) :-
  connection(X, Y), 
  mv_r(X, Y)-> 
    writeln('changing the room'), 
    retract(position(X)), 
    asserta(position(Y));
  blocked_way_message(X, Y), fail.

%move action
goto(X) :-
  not(location(X)), 
  write(X), 
  writeln(' is not a valid location.').
goto(X) :-
  position(X),
  write('You are already in '),
  writeln(X),
  !.
goto(X) :-
  location(X), 
  position(Y), 
  not(connection(X, Y)), 
  write('You cannot go to '), 
  write(X), 
  writeln(' directly.').
goto(exit) :-
  position(Y), 
  single_move(Y, exit)->
    set_game_state(won),
    !;
  !.
goto(X) :-
  location(X), 
  position(Y), 
  single_move(Y, X)->
    line, 
    print_pos, 
    !;
  !.

%initialize Game
start_game :-
  init, 
  start_interactive_mode.

%restart the game
restart_game :-
  (retractall(game_state(_));true), 
  (retractall(position(_));true), 
  (retractall(blocked(_, _));true), 
  (retractall(contains(_, _));true), 
  (retractall(blocked_object(_));true), 
  (retractall(light_state(_));true),
  start_game.

%main loop
start_interactive_mode :-
  repeat, 
    line, 
    read_command(Y), 
    line, 
    process_command(Y), 
    ((game_state(end), print_end_message);
    (game_state(failed), print_fail_message);
    (game_state(won), print_success_message)), 
  halt.

%read in the user command
read_command(Y) :-
  write('--> '), 
  readln(Y).

%process a command
process_command(Y) :-
  preprocess_command(Y, Z), 
  command(Z)->
    true;
  writeln('What makes sis command?! Enter help for more information.').

%preprocess the command
preprocess_command(A, B) :-
  command_lowercase(A, T), 
  command_clean(T, B). 
  %write('Processed command: '), 
  %writeln(B).

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
ignored_word('on').
ignored_word('with').

%end the game
command([stop]) :-
  command([end]).
command([exit]) :-
  command([end]).
command([quit]) :-
  command([end]).
command([halt]) :-
  command([end]).
command([end]) :-
  set_game_state(end).

%reload the file - for debugging
%command([reload]) :-
%  writeln('Reloading file "mine.pl"'), 
%  consult(mine), 
%  restart_game.

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
  %writeln('"reload" - Reload the file (debugging purpose)'), 
  %tab, 
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
command([go, R]) :-
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
command([put,A,inventory]) :-
  take(A),
  !.
command([put, A, B]) :-
  put_to_any(A, B);
  true. 

%use something
command([use, R]) :-
  use(R);
  true.

%use something on something
command([use, A, B]) :-
  use_on(A, B);
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
  (light_state(on)->
    writeln('Hard to show a non-existing map. Use your imagination!');
    writeln('Can\'t even see my own hands!') 
  ),
  true.

%show the inventory
command(['inventory']) :-
  command([show, 'inventory']).
command([show, 'inventory']) :-
  print_inventory.

tab :-
  writef('\t').

print_fail_message :-
  writeln('GAME OVER!!!! Please come again!').

print_end_message :-
  writeln('End of game. Thank you for playing!').

print_success_message :-
  writeln('DAAAAMN, you WON!!!! You are a REAL HERO!!! BRACE YOURSELF!!!!').

line :- 
  writeln('---------------------------------------------------------------------------'). 
