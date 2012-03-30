%(c) David Hildenbrand, Tobias Schoknecht

%init
init:-initItems,initText.

%start text
initText:-write('You are exploring an old mine.'),nl,
  write('A sudden tremor occured and some rocks are falling down.'),nl,
  write('You are heading for the exit.'),nl,nl,
  write('It is blocked.'),nl,printPos->true;true.

%print messages
printPos:-position(X),write('You are in '),write(X),write('.'),nl,printPossiblePaths->true;true.

printPossiblePaths:-position(X),writeln('You can go to the following areas: '),connection(X,Y),tab,writeln(Y),fail.

printTaken(X,Y):-write('Took '),write(X),write(' from '),write(Y).

%initialize items
initItems:-asserta(inventory(flashlight)),
  asserta(contains(dynamite,box1)),
  asserta(contains(box1,chamber3)),
  asserta(contains(fuseCord,chamber2)),
  asserta(contains(fuse,chamber1)).

%recursive contains
recContains(X,Y):-contains(X,Y)->true;contains(X,Z),contains(Z,Y)->true;false.

%combinable items
combinable(dynamite,fuseCord).
combinable(fuse,fuseCord).

bothWayCombinable(X,Y):-combinable(X,Y).
bothWayCombinable(X,Y):-combinable(Y,X).

combine(X,Y):-bothWayCombinable(X,Y),inventory(X),inventory(Y),asserta(combination(X,Y)).

checkCombination(X,Y):-combination(X,Y).
checkCombination(X,Y):-combination(Y,X).

takeable(fuse).
takeable(fuseCord).
takeable(dynamite).

%take action
take(X):-takeable(X)->position(Z),recContains(X,Z),retract(contains(X,Y)),asserta(inventory(X)),printTaken(X,Y);write('You cannot take '),write(X).

put(X):-position(Here),checkCombination(X,Y),checkCombination(Y,Z),asserta(contains(X,Here)),retract(inventory(X)),asserta(contains(Y,Here)),retract(inventory(Y)),asserta(contains(Z,Here)),retract(inventory(Z)).

%look around
lookAround:-position(X),contains(Y,X)->writeln('You can see:'),tab,writeln(Y);writeln('There is nothing here.'),true.

%examine objects
examine(Y):-position(X),contains(Y,X),contains(Z,Y),write(Y),write(' contains '),write(Z),nl.
examine(Y):-inventory(Y),contains(X,Y),write(X),write(' contains '),write(Y),nl.
examine_object(Y):-examine(Y)->true;writeln('Nothing found.').

%general location definition
location(exit).
location(tunnel1).
location(tunnel2).
location(tunnel3).
location(tunnel4).
location(chamber1).
location(chamber2).
location(chamber3).

%connections between locations
simpleConn(exit,tunnel1).
simpleConn(tunnel1,tunnel2).
simpleConn(tunnel1,chamber1).
simpleConn(tunnel2,chamber2).
simpleConn(tunnel2,chamber3).
simpleConn(tunnel2,tunnel3).
simpleConn(tunnel3,tunnel4).

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
startGame:-asserta(game_state(started)),asserta(position(chamber3)),asserta(blocked(tunnel1,exit)),init,startInteractiveMode.

%restart the game
restartGame:-retract(game_state(_)),retract(position(_)),retract(blocked(_,_)),retract(inventory(_)),retract(contains(_,_)),startGame.

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
			tab,writeln('"examine X" - Examine an object.').
command([go|[to,R]]):-goto(R)->true;true.

%display location information
command([where,am,i]):-printPos.
                      
%take a look around
command([look,around]):-lookAround.

%take something
command([take,R]):-writeln('Not implemented yet.').

%drop something
command([drop,R]):-writeln('Not implemented yet.').

%use something
command([use,R]):-writeln('Not implemented yet.').

%move something
command([move,R]):-writeln('Not implemented yet.').

%enter code something
command([enter,code,R]):-writeln('Not implemented yet.').

%combine objects
command([combina,A,B]):-writeln('Not implemented yet.').

%examine something
command([examine,R]):-examine_object(R).

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
