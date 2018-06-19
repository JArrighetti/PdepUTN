:- begin_tests(hechos_spoilers).

	test(la_muerte_de_emperor_es_spoiler_para_starWars, nondet) :- esSpoiler(starWars,muerte(emperor)).
	test(la_muerte_de_pedro_no_es_spoiler_para_starWars, fail) :- esSpoiler(starWars,muerte(pedro)).
	test(la_relacion_de_parentesco_entre_anakin_y_rey_es_spoiler_para_starWars, nondet) :- esSpoiler(starWars,relacion(parentesco,anakin,rey)).
	test(la_relacion_de_parentesco_entre_anakin_y_lavezzi_no_es_spoiler_para_starWars, fail) :- esSpoiler(starWars,relacion(parentesco,anakin,lavezzi)).

:- end_tests(hechos_spoilers).

:- begin_tests(quienes_spoilean).

	test(gaston_le_spoileo_a_maiu_algo_de_gameOfThrones, nondet) :- leSpoileo(gaston, maiu, got).
	test(nico_le_spoileo_a_maiu_algo_de_starWars, nondet) :- leSpoileo(nico,maiu,starWars).

:- end_tests(quienes_spoilean).

:- begin_tests(televidentes_responsables).

	test(televidentes_que_son_responsables, set(TelevidentesResponsables == [juan,maiu,aye])) :- televidenteResponsable(TelevidentesResponsables).
	%test(gaston_no_es_un_televidente_responsable, fail) :- televidenteResponsable(gaston).
	%test(nico_no_es_un_televidente_responsable, fail) :- televidenteResponsable(nico).
	%test(aye_no_es_una_televidente_responsable, fail) :- televidenteResponsable(aye).
	test(televidentes_que_son_responsables, set(TelevidentesResponsables == [gaston,nico]), fail) :- televidenteResponsable(TelevidentesResponsables).

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

%no se agrega a Alf porque al no poner que mira alguna serie en la base de conocimientos, se asume que este no ve
%ninguna por el principio de universo cerrado.

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
cantidadDeEpisodios(futurama,8,16).

%no se puso la serie madMen porque no se sabe cuántos episodios tiene la segunda temporada
%entonces, al no ponerlo se asume que esto no se conoce (Principio de universo cerrado)

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
%se pueden hacer las dos consultas porque es inversible
%podemos hacer consultas individuales como: "esSpoiler(starWars,muerte(emperor)).". Muestra si cumple o no la regla
%O consultas existenciales como: "esSpoiler(starWars,Spoilers).". Muestra todos los Spoilers de starWars

miraOQuiereMirar(Persona, Serie):- quienMira(Persona, Serie).
miraOQuiereMirar(Persona, Serie):- quiereMirar(Persona, Serie).

leSpoileo(PersonaQueSpoilea, Victima, Serie) :-
	miraOQuiereMirar(Victima, Serie),
	leDijo(PersonaQueSpoilea,Victima,Serie,UnHecho),
	esSpoiler(Serie, UnHecho).

%Lo mismo. Es inversible, por lo tanto admite consultas individuales ("leSpoileo(gaston, maiu, got).")
%y existenciales ("leSpoileo(gaston, Victimas, got)." (personas a las que spoileo gaston)).

televidenteResponsable(BuenTelevidente) :-
 miraOQuiereMirar(BuenTelevidente, _),
 not(leSpoileo(BuenTelevidente,_,_)).

pasoAlgoFuerte(Serie):-
 paso(Serie,_,_,muerte(_)).
pasoAlgoFuerte(Serie):-
 paso(Serie,_,_,relacion(amorosa,_,_)).
pasoAlgoFuerte(Serie):-
 paso(Serie,_,_,relacion(parentesco,_,_)).

 pasoAlgoFuerteEnTemporada(Serie,Temporada):-
 paso(Serie,Temporada,_,muerte(_)).
pasoAlgoFuerteEnTemporada(Serie, Temporada):-
 paso(Serie,Temporada,_,relacion(amorosa,_,_)).
pasoAlgoFuerteEnTemporada(Serie, Temporada):-
 paso(Serie,Temporada,_,relacion(parentesco,_,_)).

vieneZafando(Persona,Serie):-
 miraOQuiereMirar(Persona,Serie),
 not(leSpoileo(_,Persona,Serie)),
 paso(Serie,_,_,_),
 forall(paso(Serie,Temporada,_,_), pasoAlgoFuerteEnTemporada(Serie, Temporada)).
/*
 vieneZafando(Persona,Serie):-
 miraOQuiereMirar(Persona,Serie),
 not(leSpoileo(_,Persona,Serie)),
 cantidadDeEpisodios(Serie,_,_),
 forall(cantidadDeEpisodios(Serie,Temporada,_), pasoAlgoFuerteEnTemporada(Serie, Temporada)).
*/
vieneZafando(Persona,Serie):-
 miraOQuiereMirar(Persona,Serie),
 not(leSpoileo(_,Persona,Serie)),
 esPopular(Serie).

televidenteResponsable(BuenTelevidente) :- quiereMirar(BuenTelevidente, _), not(leSpoileo(BuenTelevidente,_,_)).
