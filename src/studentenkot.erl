%%%-------------------------------------------------------------------
%%% @author Pirotte Niels
%%% @copyright (C) 2017, UHasselt
%%% @doc
%%%   implements accountancy functions for a student house
%%% @end
%%% Created : 03. Oct 2017 3:45 PM
%%%-------------------------------------------------------------------
-module(studentenkot).
-author("Pirotte Niels").

-include("datatypes.hrl").

%% API
-export([number_chambers/2, test/0]).

%Test
-define(eigenaar1, #naam{voornaam = "Niels", achternaam = "Pirotte"}).
-define(eigenaar2, #naam{voornaam = "Casper", achternaam = "Vranken"}).

-define(huurder1, #naam{voornaam = "Elon", achternaam = "Musk"}).
-define(huurder2, #naam{voornaam = "Bill", achternaam = "Gates"}).
-define(huurder3, #naam{voornaam = "Mark", achternaam = "Zuckerberg"}).

-define(verrichting1, #verrichting{type = "T", bedrag = 500, nummer = 0}).
-define(verrichting2, #verrichting{type = "K", bedrag = 500, nummer = 0}).
-define(verrichting3, #verrichting{type = "T", bedrag = 500, nummer = 0}).

-define(contract1, #contract{eigenaar = ?eigenaar1, huurder = ?huurder1, huur_pm = 500, voorschot_energie_pm = 500,
        begindatum = {2017, 9, 5}}).
-define(contract2, #contract{eigenaar = ?eigenaar2, huurder = ?huurder2, huur_pm = 600, voorschot_energie_pm = 500,
        begindatum = {2017, 9, 5}, nummer = 1}).
-define(contract3, #contract{eigenaar = ?eigenaar1, huurder = ?huurder3, huur_pm = 500, voorschot_energie_pm = 500,
        begindatum = {2017,9,5}}).

-define(contracten, [?contract1, ?contract2, ?contract3]).
-define(verrichtingen, [?verrichting1, ?verrichting2, ?verrichting3]).

%%Testfuncties
%Geeft het aantal verschillende kamers terug waarvoor een contract is gemaakt
%test() -> number_chambers(?contracten).

%Geeft het aantal verschillende kamers terug van een bepaalde eigenaar
%test()-> number_chambers(?eigenaar2, ?contracten).

%Geeft het aantal maanden terug tussen 2 datums
%test()-> maanden({2017, 7, 5}, {2017, 9, 4}).

%Sorteert de contracten op basis van de Totale inkomsten
%test() -> qsort(?contracten).

%Geeft de huurinkomsten van een contract terug.
%test() -> sum_huur(?contract2).

%Voeg een contract toe aan de lijst met contracten. Dit mag niet voor het zelfde nummer van kamer,
%tenzij het contract al is vervallen
%test() -> addContract(?contract1, ?contracten).

%Map alle inkomsten per contract
%test() -> mapInkomsten(?contracten, ?verrichtingen).

%Return inkomsten van een kamer
%test() -> {sum_inkomsten(?contract1, ?verrichtingen)}.

%Return inkomsten van alle eigenaars
%test() -> getInkomstenPerEigenaar(?contracten, ?verrichtingen).

%Return inkomsten per eigenaar
test() -> getInkomstenEigenaar(?contracten, ?verrichtingen, ?eigenaar1).

number_chambers([X|XS])->
  Lijst = verwijder_dubbels([getNummer(Y)||Y<-[X|XS]]),
  length(Lijst).

%Aantal kamers per eigenaar
number_chambers(A, B) -> number_chambers(A, B, 0).

%%Staartrecursie
number_chambers(_, [], Acc) ->
  Acc;
number_chambers(X = #naam{}, [#contract{eigenaar = Y}|YS], Acc) ->
  case equals(X, Y) of
    true -> number_chambers(X, YS, Acc + 1);
    false -> number_chambers(X, YS, Acc)
  end;
number_chambers(X, [_|YS], Acc) ->
  number_chambers(X, YS, Acc);
number_chambers(_, _, _) ->
  "Verkeerde input".

%Wat zijn de huuropbrengsten van een contract.
sum_huur(#contract{huur_pm = HPM, voorschot_energie_pm = EPM, begindatum = BD, aantal_maanden = AM}) ->
  Verstreken_maanden = maanden(BD, date()) rem AM,
  Verstreken_maanden*(HPM + EPM);
sum_huur(_) ->
  "verkeerde input".

%Totale geldsom van een lijst van verrichtingen.
sum_verrichtingen(Verrichtingen) ->
  sum_verrichtingen(Verrichtingen, 0).

%vermijden van staartrecursie:
sum_verrichtingen([], Acc) ->
  Acc;
sum_verrichtingen([V = #verrichting{}|XS], Acc) ->
  sum_verrichtingen(XS, Acc+bedrag_verrichting(V));
sum_verrichtingen(_, _) ->
  "error: verkeerde input voor functie".

%Wat zijn de totale inkomsten van een contract (-verrichtingen)
sum_inkomsten(C = #contract{nummer = N, begindatum = BD, aantal_maanden = AM}, [X|XS]) ->
  %Betalingen = sum_verrichtingen([B || B <- [X|XS], ((getNummer(B) == N) and (maanden(BD, getBeginDatum(B)) < AM))]),
  Betalingen = sum_verrichtingen([B || B <- [X|XS], (getNummer(B) == N)]),
  sum_huur(C) + Betalingen;
sum_inkomsten(_, _) ->
  "verkeerde input".

%Help functie -> returned aan functie die de totale inkomsten geeft van contract gegeven een lijst van verrichtingen
sum_inkomsten([X|XS])->
  fun(Contract) -> sum_inkomsten(Contract, [X|XS]) end;
sum_inkomsten(_) ->
  "Verkeerde input".

%Maps inkomsten functie op een lijst van contracten [X|XS] gegeven een lijst van verrichtingen [Y|YS].
% => returns totale inkomsten voor de eigenaar per kamer.
mapInkomsten([], _) ->
  [];
mapInkomsten([X|XS], [Y|YS]) ->
  %lists:map(fun(C)->sum_inkomsten(C, [Y|YS]) end, [X|XS]);
  lists:map(sum_inkomsten([Y|YS]), [X|XS]);
mapInkomsten(_, _) ->
  "verkeerde input".

%Returned alle inkomsten van alle kamers van een bepaalde eigenaar
getInkomstenEigenaar([], _, _)->
  [];
getInkomstenEigenaar([X|XS], [Y|YS],  Eigenaar) ->
  InkomstenPerKamer = mapInkomsten(getContractenVanEigenaar([X|XS], Eigenaar), [Y|YS]),
  lists:sum(InkomstenPerKamer);
getInkomstenEigenaar(_, _, _) ->
  "verkeerde input".

%Help function -> returned een functie die voor iedere eigenaar de som van alle inkomsten terug geeft.
getInkomstenEigenaar([X|XS], [Y|YS]) ->
  fun(Eigenaar) -> getInkomstenEigenaar([X|XS], [Y|YS],  Eigenaar) end.

%Returned alle contracten van een bepaalde eigenaar.
getContractenVanEigenaar([], _) ->
  [];
getContractenVanEigenaar([X|XS], Eigenaar)->
  lists:filter(fun(#contract{eigenaar = E})-> equals(E, Eigenaar) end, [X|XS]);
getContractenVanEigenaar(_, _)  ->
  "verkeerde input".

%De inkomsten per eigenaar
getInkomstenPerEigenaar([X|XS], [Y|YS])->
  Eigenaars = getEigenaars([X|XS]),
  InkomstenPerEigenaar = lists:map(getInkomstenEigenaar([X|XS], [Y|YS]), Eigenaars),
  lists:zip(Eigenaars, InkomstenPerEigenaar);
getInkomstenPerEigenaar(_,_) ->
  "Verkeerde input".

%Oude contracten worden automatisch overschreven
addContract(C = #contract{nummer = N}, [X|XS]) ->
  [C| [Y||Y <- [X|XS], ((not is_geldig(Y)) or (getNummer(Y)/= N))]];
addContract(_, _) ->
  "verkeerde input".

addVerrichting(V = #verrichting{}, [X|XS]) ->
  [V|[X|XS]];
addVerrichting(_, _) ->
  "Verkeede input".

%-----------------------------------------------------------------------
%% Help functions
%-----------------------------------------------------------------------

%Bedrag verrichting (Korting of betaling moet worden afgetrokken en toeslag en aanrekening bijgeteld)
bedrag_verrichting(#verrichting{type = T, bedrag = B}) when (T=="B") or (T=="K") ->
  -B;
bedrag_verrichting(#verrichting{type = T, bedrag = B}) when (T=="T") or (T=="A") ->
  B;
bedrag_verrichting(_) ->
  "error: verkeerde input voor functie".

% equals functie
equals(N2, #contract{nummer = N1})  when N1 == N2 ->
  true;
equals(#naam{voornaam = V1, achternaam = A1},#naam{voornaam = V2, achternaam = A2}) when (V1 == V2) and (A1 == A2) ->
  true;
equals(_, _) ->
  false.

% getter voor nummer van zowel verrichting als contract.
getNummer(#verrichting{nummer = N})->
  N;
getNummer(#contract{nummer = N}) ->
  N;
getNummer(_)->false.

%getter voor eigenaar van kamer.
getEigenaar(#contract{eigenaar = E}) ->
  E;
getEigenaar(_) ->
  false.

%returned alle eigenaars van een reeks van contracten als een lijst.
getEigenaars([]) ->
  [];
getEigenaars([X|XS]) ->
  [getEigenaar(X)|getEigenaars([Y||Y<-XS, not equals(getEigenaar(X), getEigenaar(Y))])];
getEigenaars(_) ->
  "Verkeerde input".

%verwijderd alle dubbels uit een lijst op een recursieve manier.
verwijder_dubbels([]) ->
  [];
verwijder_dubbels([X|XS]) ->
  [X | [Y || Y <- verwijder_dubbels(XS), X /= Y]];
verwijder_dubbels(_) ->
  "verkeerde input".

%Quicksort principle
qsort([]) ->
  [];
qsort([X|XS])->
  Smaller = qsort([Y || Y<-XS, st(Y, X)]),
  Bigger = qsort([Y || Y<-XS,  bt(Y, X)]),
  Smaller ++ [X|Bigger].

%Geeft all elementen terug met een gegeven inkomst
get([], _) ->
  [];
get([X|XS], Y)->
  qsort([Z || Z<-[X|XS], equals(Y, Z)]).

%Bigger than function
bt(C1 = #contract{}, C2 = #contract{}) ->
  sum_inkomsten(C1)>sum_inkomsten(C2);
bt(_, _)->
  "verkeerde input".

%Smaller than function
%Returns true if first arg is smaller or eq
st(C1 = #contract{}, C2 = #contract{}) ->
  sum_inkomsten(C1)=<sum_inkomsten(C2);
st(_, _)->
  "verkeerde input".

%Aantal maanden tussen
%datum 1 moet vroeger zijn dan datum 2,anders returnen we standaard 0
maanden({J1, M1, D1}, {J2, M2, D2}) ->
  V_dagen = D2-D1,
  %io:fwrite("1. ~b", [V_dagen]),

  V_maanden = case V_dagen >= 0 of
                false -> M2 - M1 - 1;
                true  -> M2 - M1
              end,
  V_jaren  = J2 - J1,
  Out = case V_jaren  >= 0 of
    false -> 0;
    true  -> V_jaren * 12 + V_maanden
        end,
  is_pos(Out).

%Check als is contract
is_contract(#contract{}) ->
  true;
is_contract(_) ->
  false.

%Kijkt als een contract nog geldig is
is_geldig(#contract{begindatum = BD, aantal_maanden = AM}) ->
  Verstreken_maanden = maanden(BD, date()),
  Verstreken_maanden < AM;
is_geldig(_) ->
  false.

%Geeft de begindatum van een contract terug
getBeginDatum(#contract{begindatum = BD}) ->
  BD;
getBeginDatum(_) ->
  "Verkeerde input".

is_pos(X) when X < 0 ->
  0;
is_pos(X) ->
  X.