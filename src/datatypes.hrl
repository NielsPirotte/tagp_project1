%%%-------------------------------------------------------------------
%%% @author Niels Pirotte
%%% @copyright (C) 2017, UHasselt
%%% @doc
%%% implements necessary datatypes
%%% @end
%%% Created : 03. Nov 2017 3:44 PM
%%%-------------------------------------------------------------------
-author("Niels Pirotte").

%datum (inspiratie uit date() functie)
%We gaan er gewoon van uit dat een datum van hetvolgende type is: {Year, Month, Day}
-record(datum,{jaar=2017, maand = 11, dag = 3}).

%naam
-record(naam, {voornaam = "", achternaam = ""}).

%verrichting

%Er zijn verschillende types van verrichtingen
  %Betaling = "B"
  %Korting = "K"
  %Toeslag = "T"
  %Afrekening = "A"

-record(verrichting, {type = "B", datum = date(), nummer = 0, bedrag = 0, reden = ""}).

%contract
-record(contract, {nummer = 0, eigenaar = #naam{}, huurder = #naam{}, huur_pm = 0,
                   voorschot_energie_pm = 0, begindatum = date(), aantal_maanden = 12}).