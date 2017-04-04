tipo(electrico).
tipo(agua).
tipo(fuego).
tipo(planta).
tipo(veneno).
tipo(nulo).

pokemon(pikachu, tipo(electrico), tipo(nulo)).
pokemon(squirtle, tipo(agua), tipo(nulo)).
pokemon(bulbasaur, tipo(planta), tipo(veneno)).
pokemon(charmander, tipo(fuego), tipo(nulo)).
pokemon(psyduck, tipo(agua), tipo(nulo)).

fuerte(tipo(electrico), tipo(agua)).
fuerte(tipo(agua), tipo(fuego)).
fuerte(tipo(fuego), tipo(planta)).
fuerte(tipo(planta), tipo(agua)).
fuerte(tipo(veneno), tipo(planta)).

debil(tipo(agua), tipo(electrico)).
debil(tipo(fuego), tipo(agua)).
debil(tipo(planta), tipo(fuego)).
debil(tipo(agua), tipo(planta)).
debil(tipo(planta), tipo(veneno)).

sin_efecto(tipo(electrico), tipo(electrico)).
sin_efecto(tipo(agua), tipo(agua)).
sin_efecto(tipo(fuego), tipo(fuego)).
sin_efecto(tipo(planta), tipo(planta)).
sin_efecto(tipo(veneno), tipo(veneno)).