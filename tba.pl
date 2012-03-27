init:-write('(c) David Hildenbrand, Tobias Schoknecht'),nl,write('Knowledge Based Systems Project'),nl,nl,write('You wake up in a dark room'),true.

printlocation:-position(X),write('You are in the '),write(X).

location('wine cellar').
location('corridorUG').
location('fitnessroom').
location('boiler room').
location('sauna').
location('relaxation room').

door('wine cellar','corridorUG').
door('fitnessroom','corridorUG').
door('relaxation room','corridorUG').
door('sauna','corridorUG').
door('boiler room','corridorUG').
%door(X,Y):-door(Y,X).

movementRule('relaxation room','corridorUG'):-inventory([keyA]).

stairs('corridorUG','corridorEG').
stairs('corridorEG','corridorUG').

go_to(X):-position(Y),movementRule(Y,X)->retract(position(Y)),asserta(position(X));write('failed'),false.

%initialize Game
startGame:-init,assert(position('boiler room')),assert(inventory([])).

