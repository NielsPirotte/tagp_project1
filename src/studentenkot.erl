%%%-------------------------------------------------------------------
%%% @author Pirotte Niels
%%% @copyright (C) 2017, UHasselt
%%% @doc
%%%   implements accountancy functions for a student house
%%% @end
%%% Created : 03. Nov 2017 3:45 PM
%%%-------------------------------------------------------------------
-module(studentenkot).
-author("Pirotte Niels").

-include("datatypes.hrl").

%% API
-export([number_chambers/2, test/0]).

%Test
-define(datum1, #datum{jaar = 2017, maand = 10, dag = 6}).
-define(datum2, #datum{jaar = 2018, maand = 11, dag = 24}).

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
-define(contract2, #contract{eigenaar = ?eigenaar1, huurder = ?huurder2, huur_pm = 500, voorschot_energie_pm = 500,
        begindatum = date()}).
-define(contract3, #contract{eigenaar = ?eigenaar1, huurder = ?huurder3, huur_pm = 500, voorschot_energie_pm = 500,
        begindatum = date()}).

-define(contracten, [?contract1, ?contract2, ?contract3]).
-define(verrichtingen, [?verrichting1, ?verrichting2, ?verrichting3]).

%%Testfuncties
%test()-> number_chambers(?eigenaar2, ?contracten).
%test()-> sum_verrichtingen(?verrichtingen, 0).
%test()-> maanden(?datum1, ?datum2).
test()-> sum_inkomsten(?contracten, 0).

%%Variabelen mogen maar 1x worden toegekent
%test()->
%  V = 5,
%  V.
%%%%%%%%%%%%%%


%equals functie
equals(#naam{voornaam = V1, achternaam = A1},#naam{voornaam = V2, achternaam = A2}) when (V1 == V2) and (A1 == A2) -> true;
equals(_, _) -> false.

%aantal kamers per eigenaar
number_chambers(_, []) -> 0;
number_chambers(X = #naam{}, [#contract{eigenaar = Y}|YS]) -> case equals(X, Y) of
                        true -> 1 + number_chambers(X, YS);
                        false -> number_chambers(X, YS) end.

%contracten van een kamer
contracten([], _) -> [];
contracten([C = #contract{nummer = X}|XS], Nummer) when X == Nummer -> C ++ contracten(XS, Nummer);
contracten([#contract{}|XS], Nummer) -> contracten(XS, Nummer).


%huidige inkomsten van een kamer
sum_inkomsten(Contracten, Nummer) -> sum_inkomsten(Contracten, Nummer, 0).

sum_inkomsten([], _, Acc) -> Acc;
sum_inkomsten([#contract{nummer = X, huur_pm = H, voorschot_energie_pm = VE, begindatum = BD}|XS], Nummer, Acc) when X == Nummer
  ->
  Verstreken_maanden = maanden(BD, date()),
  sum_inkomsten(XS, Nummer, Acc + Verstreken_maanden*(H+VE));
sum_inkomsten([#contract{}|XS], Nummer, Acc)
  -> sum_inkomsten(XS, Nummer, Acc);
sum_inkomsten(_, _, _)
  -> "error: verkeerde input voor functie".

%verrichtingen van een kamer
sum_verrichtingen(Verrichtingen, Nummer) -> sum_verrichtingen(Verrichtingen, Nummer, 0).

%vermijden van staartrecursie
sum_verrichtingen([], _, Acc)
            -> Acc;
sum_verrichtingen([V = #verrichting{nummer = X}|XS], Nummer, Acc) when X == Nummer
            -> sum_verrichtingen(XS, Nummer, Acc+bedrag_verrichting(V));
sum_verrichtingen([#verrichting{}|XS], Nummer, Acc)
            -> sum_verrichtingen(XS, Nummer, Acc);
sum_verrichtingen(_, _, _)
            -> "error: verkeerde input voor functie".

%Bedrag verrichting
bedrag_verrichting(#verrichting{type = T, bedrag = B}) when (T=="B") or (T=="K") -> -B;
bedrag_verrichting(#verrichting{type = T, bedrag = B}) when (T=="T") or (T=="A") -> B;
bedrag_verrichting(_) -> "error: verkeerde input voor functie".

%Aantal maanden tussen
%datum 1 moet vroeger zijn dan datum 2,anders returnen we standaard 0
maanden({J1, M1, D1}, {J2, M2, D2})
            -> V_dagen = D2-D1,
               %io:fwrite("1. ~b", [V_dagen]),

            V_maanden = case V_dagen >= 0 of
                          false -> M2 - M1 - 1;
                          true  -> M2 - M1
                        end,
            V_jaren  = J2 - J1,
            case V_jaren  >= 0 of
                false -> 0;
                true  -> V_jaren * 12 + V_maanden
            end.


isPos(X) when X =< 0 -> 0;
isPos(X) -> 1.