:- begin_tests(spoilers).

	test(la_muerte_de_emperor_es_spoiler_para_starWars, nondet) :- esSpoiler(starWars,muerte(emperor)).
	test(la_muerte_de_pedro_no_es_spoiler_para_starWars, fail) :- esSpoiler(starWars,muerte(pedro)).
	test(la_relacion_de_parentesco_entre_anakin_y_rey_es_spoiler_para_starWars, nondet) :- esSpoiler(starWars,relacion(parentesco,anakin,rey)).
	test(la_relacion_de_parentesco_entre_anakin_y_lavezzi_no_es_spoiler_para_starWars, fail) :- esSpoiler(starWars,relacion(parentesco,anakin,lavezzi)).

:- end_tests(spoilers).

:- begin_tests(quienes_spoilean).

	test(gaston_le_spoileo_a_maiu_algo_de_gameOfThrones, nondet) :- leSpoileo(gaston, maiu, got).
	test(nico_le_spoileo_a_maiu_algo_de_starWars, nondet) :- leSpoileo(nico,maiu,starWars).

:- end_tests(quienes_spoilean).

:- begin_tests(televidentes_responsables).

	test(televidentes_que_son_responsables, set(TelevidentesResponsables == [juan,maiu])) :- televidenteResponsable(TelevidentesResponsables).
	%test(gaston_no_es_un_televidente_responsable, fail) :- televidenteResponsable(gaston).
	%test(nico_no_es_un_televidente_responsable, fail) :- televidenteResponsable(nico).
	%test(aye_no_es_una_televidente_responsable, fail) :- televidenteResponsable(aye).
	test(televidentes_que_son_responsables, set(TelevidentesResponsables == [gaston,nico,aye]), fail) :- televidenteResponsable(TelevidentesResponsables).

:- end_tests(televidentes_responsables).

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
quiereMirar(aye,got).
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

leSpoileo(PersonaQueSpoilea, Victima, Serie) :-
	leDijo(PersonaQueSpoilea,Victima,Serie,UnHecho),
	esSpoiler(Serie, UnHecho).

televidenteResponsable(BuenTelevidente) :- quienMira(BuenTelevidente, _), not(leSpoileo(BuenTelevidente,_,_)).
televidenteResponsable(BuenTelevidente) :- quiereMirar(BuenTelevidente, _), not(leSpoileo(BuenTelevidente,_,_)).