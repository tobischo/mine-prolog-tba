%(c) David Hildenbrand, Tobias Schoknecht

%init
init:-initItems;initText.

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

takeable(fuse).
takeable(fuseCord).
takeable(dynamite).

%take action
take(X):-takeable(X)->position(Z),recContains(X,Z),retract(contains(X,Y)),asserta(inventory(X)),printTaken(X,Y);write('You can not take '),write(X).

%look around
lookAround:-position(X),viewable(X,Y),writeln('You can see:'),writeln(Y).

%viewable
viewable(X,Y):-contains(Y,X).

%examine
examine(Y):-position(X),viewable(X,Y),viewable(Y,Z),write(Z).

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
movementRule(tunnelPartA,exit):-unblocked(exit).
movementRule(X,Y).

%two way movement rules
mvR(X,Y):-movementRule(X,Y).
mvR(X,Y):-movementRule(Y,X).

%move action
go_to(X):-position(Y),mvR(X,Y)->retract(position(Y)),asserta(position(X)),printPos;write('Can not move to '),write(X),false.

%initialize Game
startGame:-asserta(position(tunnelPartA)),init,asserta(unblocked(nothing)).
