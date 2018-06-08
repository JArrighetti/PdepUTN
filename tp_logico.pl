quienMira(juan,himym).
quienMira(juan,futurama).
quienMira(juan,got).
quienMira(nico,starWars).
quienMira(maiu,starWars).
quienMira(maiu,onePiece).
quienMira(maiu,got).
quienMira(nico,got).
quienMira(gaston,hoc).

esPopular(got).
esPopular(hoc).
esPopular(starWars).

quiereMirar(juan,hoc).
quiereMirar(aya,got).
quiereMirar(gaston,himym).

cantidadDeEpisodios(got,3,12).
cantidadDeEpisodios(got,2,10).
cantidadDeEpisodios(himym,1,23).
cantidadDeEpisodios(drHouse,8,16).

paso(futurama, 2, 3, muerte(seymourDiera)).
paso(starWars, 10, 9, muerte(emperor)).
paso(starWars, 1, 2, relacion(parentesco, anakin, rey)).
paso(starWars, 3, 2, relacion(parentesco, vader, luke)).
paso(himym, 1, 1, relacion(amorosa, ted, robin)).
paso(himym, 4, 3, relacion(amorosa, swarley, robin)).
paso(got, 4, 5, relacion(amistad, tyrion, dragon)).

leDijo(gaston, maiu, got, relacion(amistad, tyrion, dragon)).
leDijo(nico, maiu, starWars, relacion(parentesco, vader, luke)).
leDijo(nico, juan, got, muerte(tyrion)). 
leDijo(aye, juan, got, relacion(amistad, tyrion, john)).
leDijo(aye, maiu, got, relacion(amistad, tyrion, john)).
leDijo(aye, gaston, got, relacion(amistad, tyrion, dragon)).

esSpoiler(Serie, CosaQuePaso) :- paso(Serie, _, _, CosaQuePaso).

leSpoileo(Persona1,Persona2,Serie) :- 
	not(forall(leDijo(Persona1,Persona2,Serie,CosaQuePaso), not(esSpoiler(Serie,CosaQuePaso)))).