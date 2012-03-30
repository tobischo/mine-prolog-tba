%(c) David Hildenbrand, Tobias Schoknecht

%init
init:-initItems,initText.

%start text
initText:-write('You are exploring an old mine.'),nl,
  write('A sudden tremor occured and some rocks are falling down.'),nl,
  write('You are heading for the exit.'),nl,nl,
  write('It is blocked.'),nl,printPos.

%print messages
printPos:-position(X),write('You are in '),write(X),write('.'),nl,printPossiblePaths.

printPossiblePaths:-position(X),writeln('You can go to the following areas: '),connection(X,Y),writeln(Y),fail.

printTaken(X,Y):-write('Took '),write(X),write(' from '),write(Y).

%initialize items
initItems:-asserta(inventory(flashlight)),
  asserta(contains(dynamite,boxA)),
  asserta(contains(boxA,chamberC)),
  asserta(contains(fuseCord,chamberB)),
  asserta(contains(fuse,chamberA)).

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
take(X):-takeable(X)->position(Z),recContains(X,Z),retract(contains(X,Y)),asserta(inventory(X)),printTaken(X,Y);write('You can not take '),write(X).

put(X):-position(Here),checkCombination(X,Y),checkCombination(Y,Z),asserta(contains(X,Here)),retract(inventory(X)),asserta(contains(Y,Here)),retract(inventory(Y)),asserta(contains(Z,Here)),retract(inventory(Z)).

%look around
lookAround:-position(X),viewable(X,Y)->writeln('You can see:'),writeln(Y);writeln('There is nothing here').

%viewable
viewable(X,Y):-contains(Y,X).

%examine
examine(Y):-position(X),viewable(X,Y),viewable(Y,Z),write(Y),write(' contains '),write(Z).

%general location definition
location(exit).
location(tunnelPartA).
location(tunnelPartB).
location(tunnelPartC).
location(tunnelPartD).
location(chamberA).
location(chamberB).
location(chamberC).

%connections between locations
simpleConn(exit,tunnelPartA).
simpleConn(tunnelPartA,tunnelPartB).
simpleConn(tunnelPartA,chamberA).
simpleConn(tunnelPartB,chamberB).
simpleConn(tunnelPartB,chamberC).
simpleConn(tunnelPartB,tunnelPartC).
simpleConn(tunnelPartC,tunnelPartD).

%two way connections
connection(X,Y):-simpleConn(X,Y).
connection(X,Y):-simpleConn(Y,X).

%movement rules
movementRule(tunnelPartA,exit):-blocked(exit),false.

%two way movement rules
mvR(X,Y):-movementRule(X,Y).
mvR(X,Y):-movementRule(Y,X).

%move action
goto(X):-position(Y),location(X),connection(X,Y),mvR(X,Y)->retract(position(Y)),asserta(position(X)),printPos,true;write('Can not move to '),write(X),false.

%initialize Game
startGame:-asserta(position(tunnelPartA)),asserta(blocked(exit)),init.
startGame:-asserta(game_state(started)),asserta(position(tunnelPartA)),asserta(unblocked(nothing)),init,startInteractiveMode.

%restart the game
restartGame:-retract(game_state(A)),retract(position(B)),retract(unblocked(C)),retract(inventory(D)),retract(contains(E,F)),startGame.

%main loop
startInteractiveMode:-repeat,read_command(Y),process_command(Y),game_state(finished),print_end_message,halt.

%process a command
process_command(Y):-preprocess_command(Y,Z),command(Z)->true;writeln('What makes sis command?! Enter help for more information.').

%string_to_lower([],[]).
%string_to_lower([U|UT],[L|LT]) :-writeln(U),not(between(65,90,U)),L is U,string_to_lower(U,LT).
%string_to_lower([U|UT],[L|LT]) :-writeln(U),between(65,90,U)->L is U+32,string_to_lower(U,LT).

%preprocess the command
preprocess_command(A,B):-command_lowercase(A,T),command_clean(T,B),write('Processed command: '),writeln(B).

command_lowercase([],[]).
command_lowercase([A|B],[C|D]):-downcase_atom(A,C),command_lowercase(B,D).

command_clean([';'|B],C):-command_clean(B,C).
command_clean([','|B],C):-command_clean(B,C).
command_clean(['.'|B],C):-command_clean(B,C).
command_clean(['!'|B],C):-command_clean(B,C).
command_clean(['?'|B],C):-command_clean(B,C).
command_clean([the|B],C):-command_clean(B,C).
command_clean([a|B],C):-command_clean(B,C).
command_clean([an|B],C):-command_clean(B,C).
command_clean([this|B],C):-command_clean(B,C).
command_clean([that|B],C):-command_clean(B,C).
command_clean([hell|B],C):-command_clean(B,C).
command_clean([hack|B],C):-command_clean(B,C).
command_clean([fuck|B],C):-command_clean(B,C).
command_clean([fuckin|B],C):-command_clean(B,C).
command_clean([fucking|B],C):-command_clean(B,C).
command_clean([and|B],C):-command_clean(B,C).
command_clean([],[]).
%multiple solutions possible, include all atoms with inequality here
command_clean([A|B],[A|C]):-command_clean(B,C).

%end the game
command([stop]):-command([end]).
command([quit]):-command([end]).
command([halt]):-command([end]).
command([end]):-retract(game_state(Y)),asserta(game_state(finished)).

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
			tab,writeln('"reload" - Reload the file (debugging purpose)'),
			tab,writeln('"restart" - Restart the game'),
			tab,writeln('"take X" - Take something'),
			tab,writeln('"drop X" - Drop something'),
			tab,writeln('"use X" - Use something'),
			tab,writeln('"combine X Y" - Combine two things creating a third one.'),
			tab,writeln('"move X" - Move something.'),
			tab,writeln('"enter code X" - Enter the code.'),
			tab,writeln('"examine X" - Examine an object.').
command([go|[to,R]]):-go_to(R)->true;true.

%display location information
command([where,am,i]):-printPos.

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
command([examine,R]):-writeln('Not implemented yet.').

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
