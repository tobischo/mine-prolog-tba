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
simpleConn(exit,tunnel1).
simpleConn(tunnel1,tunnel2).
simpleConn(tunnel1,break_chamber).
simpleConn(tunnel2,silver_vein_chamber).
simpleConn(tunnel2,gold_vein_chamber).
simpleConn(tunnel2,tunnel3).
simpleConn(tunnel3,tunnel4).

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

%combinable items
combinable(dynamite,usable_fuse,dynamite_with_fuse).
combinable(fuse,fuse_cord,usable_fuse).
bothWayCombinable(X,Y,Z):-combinable(X,Y,Z),!.
bothWayCombinable(X,Y,Z):-combinable(Y,X,Z).

%object stores are objects,locations an inventory
object_store(inventory).
object_store(X):-object(X).
object_store(X):-location(X).

%init basic game info
initBasic:-asserta(game_state(started)),asserta(position(tunnel4)),asserta(blocked(tunnel1,exit)).

%initialize items
initItems:-asserta(contains(flashlight,inventory)),
  asserta(contains(fuse_cord,break_chamber)),
  asserta(contains(desk,break_chamber)),
  asserta(contains(map,tunnel4)),%todo restore to desk
  asserta(contains(box,gold_vein_chamber)),
  asserta(contains(dynamite,box)),
  asserta(contains(pickaxe,gold_vein_chamber)),
  asserta(contains(fuse,silver_vein_chamber)),
  asserta(contains(mine_chariot,silver_vein_chamber)),
  asserta(contains(helmet,silver_vein_chamber)),
  asserta(contains(pouch,tunnel4)),
  asserta(contains(goldnugget,pouch)),
  asserta(contains(key,pouch)),
  asserta(contains(shovel,tunnel4)),
  asserta(contains(bunch_of_rocks,tunnel3)),
  asserta(contains(railway_switch,tunnel2)).

%easy access to the inventory
inventory(X):-contains(X,inventory).
add_to_inventory(X):-asserta(contains(X,inventory)).
remove_from_inventory(X):-retract(contains(X,inventory)).
print_inventory:-inventory(X)->(writeln('Your inventory contains:'),print_inventory_list;!);writeln('Your inventory is empty.').
print_inventory_list:-inventory(X),tab,writeln(X),fail.

%combine two items and add the result to the inventar
combine(X,Y):-inventory(X),inventory(Y),bothWayCombinable(X,Y,Z),asserta(combination(Z,inventory)),retract(X,inventory),retract(Y,inventory).

checkCombination(X,Y):-combination(X,Y).
checkCombination(X,Y):-combination(Y,X).

%print map
printMap:-writeln('   /--------------|--------------------------|------'),
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
init:-initBasic,initItems,initText.

%start text
initText:-write('You are exploring an old mine.'),nl,
  write('A sudden tremor occured and some rocks are falling down.'),nl,
  write('You are heading for the exit.'),nl,nl,
  write('It is blocked.'),nl,printPos->true;true.

%print position and possible ways
printPos:-position(X),write('You are in '),write(X),write('.'),nl,printPossiblePaths->true;true.
printPossiblePaths:-position(X),writeln('You can go to the following areas: '),connection(X,Y),tab,writeln(Y),fail.

printTaken(X,Y):-write('Took '),write(X),write(' from '),write(Y).

%recursive contains
recContains(X,Y):-contains(X,Y)->true;contains(X,Z),contains(Z,Y)->true;false.

%take from current position
take(X):-position(Z),take_from_any(X,Z)->true:true.

%take from any object store except the inventory 
take_from_any(X,inventory):-writeln('Cannot take something which is already in the inventory!'),!.
take_from_any(X,_):-not(object(X)),write(X),writeln(' is not a valid object!'),!.
take_from_any(X,_):-not(takeable(X)),write(X),writeln(' cannot be carried around!'),!.
take_from_any(X,Z):-not(contains(X,Z)),write(X),write(' not found at '),write(Z),writeln('!'),!.
take_from_any(X,Z):-object(Z),retract(contains(X,Z)),add_to_inventory(X),write(X),writeln(' added to inventory.'),!.
take_from_any(X,Z):-position(Z),retract(contains(X,Z)),add_to_inventory(X),write(X),writeln(' added to inventory.'),!.
take_from_any(X,Z):-write(X),writeln(' is not reachable from here!'),!.

%put something back
%put(X):-position(Here),checkCombination(X,Y),checkCombination(Y,Z),asserta(contains(X,Here)),retract(inventory(X)),asserta(contains(Y,Here)),retract(inventory(Y)),asserta(contains(Z,Here)),retract(inventory(Z)).

%look around
lookAround:-position(X),contains(Y,X)->writeln('You can see:'),printViewableObjects;writeln('Nothing in here!'),true.
printViewableObjects:-position(X),contains(Y,X),tab,writeln(Y),fail.

%examine objects
examine(Y):-position(X),contains(Y,X),contains(Z,Y),write(Y),write(' contains '),write(Z),nl.
examine(Y):-inventory(Y),contains(X,Y),write(Y),write(' contains '),write(X),nl.
examine_object(Y):-examine(Y)->true;writeln('Nothing found.').

%two way connections
connection(X,Y):-simpleConn(X,Y).
connection(X,Y):-simpleConn(Y,X).

%two way block check
way_blocked(X,Y):-blocked(X,Y).
way_blocked(X,Y):-blocked(Y,X).

%movement rules
movementRule(X,Y):-not(way_blocked(X,Y)).
movementRule(X,Y):-way_blocked(X,Y),blocked_reason(X,Y),fail.

%detailed block reason
blocked_reason(tunnel1,exit):-writeln('The way is blocked. You can smell the fresh air streaming in the tunnel. Find a way to remove the stones.'),writeln('You shall not pass!').

%two way movement rules
mvR(X,Y):-movementRule(X,Y).
mvR(X,Y):-movementRule(Y,X).

single_move(X,Y):-connection(X,Y),mvR(X,Y),writeln('changing the room'),retract(position(Y)),asserta(position(X)).

%move action
goto(X):-not(location(X)),write(X),writeln(' is not a valid location.').
goto(X):-location(X),position(Y),not(connection(X,Y)),write('You cannot go to '),write(X),writeln(' directly.').
goto(X):-location(X),position(Y),single_move(X,Y),printPos.

%initialize Game
startGame:-init,startInteractiveMode.

%restart the game
restartGame:-retract(game_state(_)),retract(position(_)),retract(blocked(_,_)),retract(contains(_,_)),startGame.

%main loop
startInteractiveMode:-repeat,read_command(Y),process_command(Y),game_state(finished),print_end_message,halt.

%process a command
process_command(Y):-preprocess_command(Y,Z),command(Z)->true;writeln('What makes sis command?! Enter help for more information.').

%string_to_lower([],[]).
%string_to_lower([U|UT],[L|LT]) :-writeln(U),not(between(65,90,U)),L is U,string_to_lower(U,LT).
%string_to_lower([U|UT],[L|LT]) :-writeln(U),between(65,90,U)->L is U+32,string_to_lower(U,LT).

%preprocess the command
preprocess_command(A,B):-command_lowercase(A,T),command_clean(T,B),write('Processed command: '),writeln(B).

%convert words to lower case
command_lowercase([],[]).
command_lowercase([A|B],[C|D]):-downcase_atom(A,C),command_lowercase(B,D).

%remove words that will be ignored from the list
command_clean([A|B],C):-ignored_word(A),command_clean(B,C).
command_clean([],[]).
command_clean([A|B],[A|C]):-not(ignored_word(A)),command_clean(B,C).

%the words that will be filtered out
ignored_word(';').
ignored_word(',').
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

%end the game
command([stop]):-command([end]).
command([quit]):-command([end]).
command([halt]):-command([end]).
command([end]):-retract(game_state(_)),asserta(game_state(finished)).

%reload the file - for debugging
command([reload]):-writeln('Reloading file "mine.pl"'),consult(mine),restartGame.

%restart the game
command([restart]):-restartGame.

%display the help message
command([help]):-writeln('Available commands are:'),
			tab,writeln('"help" - Display this help'),
			tab,writeln('"end" - End the game'),
			tab,writeln('"go to X" - Go to position X'),
			tab,writeln('"where am I" - Where the hell are you?'),
      tab,writeln('"look around" - Take a look around.'),
			tab,writeln('"reload" - Reload the file (debugging purpose)'),
			tab,writeln('"restart" - Restart the game'),
			tab,writeln('"take X" - Take something'),
			tab,writeln('"drop X" - Drop something'),
			tab,writeln('"use X" - Use something'),
			tab,writeln('"combine X Y" - Combine two things creating a third one.'),
			tab,writeln('"move X" - Move something.'),
			tab,writeln('"enter code X" - Enter the code.'),
			tab,writeln('"examine X" - Examine an object.'),
      tab,writeln('"show map" - Display the map.'),
      tab,writeln('"show inventoy" - Display the inventory.').
command([go|[to,R]]):-goto(R)->true;true.

%display location information
command([where,am,i]):-printPos.
                      
%take a look around
command([look,around]):-lookAround->true;true.

%take something
command([take,R]):-take(R).
command([take,R,from,Z]):-take_from_any(R,Z);true.

%drop something
command([drop,R]):-writeln('Not implemented yet.').

%use something
command([use,R]):-writeln('Not implemented yet.').

%move something
command([move,R]):-writeln('Not implemented yet.').

%enter code something
command([enter,code,R]):-writeln('Not implemented yet.').

%combine objects
command([combine,A,B]):-writeln('Not implemented yet.').

%examine something
command([examine,R]):-examine_object(R).

%show the map
command([map]):-command([show,map]).
command([show,map]):-inventory(map)->printMap;writeln('Hard to show a non-existing map. Use your imagination!'),true.

%show the inventory
command(['inventory']):-command([show,'inventory']).
command([show,'inventory']):-print_inventory.



tab:-writef('\t').
print_end_message:-writeln('End of game. Thank you for playing!').

%read in the user command
read_command(Y):-write('--> '),readln(Y).

% Alles für die Katz :(
%read_word_list(Y):-read_word(W)->read_word_list(L),append([W],L,Y),Y == [].
%read a word which contains more than one character
%read_word(W):-read_char_list([],Y),length(Y,L),L =\= 0,writeln(Y),atom_chars(W,Y).
%read a list of chars until a space is found
%read_char_list(X,Y):-read_char(C),C =\= 32->append(X,[C],Y2),read_char_list(Y2,Y);X = Y.
%read a char which is not the end of the string
%read_char(Y):-get0(Y),Y =\= 10.
