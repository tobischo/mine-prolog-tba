location(weinkeller).
location(flurUG).
location(fitnessraum).
location(heizraum).
location(sauna).
location(entspannungsraum).

door(weinkeller,flurUG).
door(fitnessraum,flurUG).
door(entspannungsraum,flurUG).
door(sauna,flurUG).
door(heizraum,flurUG).
%door(X,Y):-door(Y,X).

movementRule(heizraum,flurUG):-inventory([keyA]).

stairs(flurUG,flurEG).
stairs(flurEG,flurUG).

go_to(X):-position(Y),movementRule(Y,X)->retract(position(Y)),asserta(position(X));write('failed'),false.

%initialize Game
startGame:-assert(position(heizraum)),assert(inventory([])).
