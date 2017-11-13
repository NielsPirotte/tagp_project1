%%%-------------------------------------------------------------------
%%% @author Niels Pirotte
%%% @copyright (C) 2017, UHasselt
%%% @doc
%%% implements necessary datatypes
%%% @end
%%% Created : 03. Oct 2017 3:44 PM
%%%-------------------------------------------------------------------
-author("Niels Pirotte").

%naam
-record(naam, {voornaam = "", achternaam = ""}).

%Verrichting
%Er zijn verschillende types van verrichtingen
  %Betaling = "B"
  %Korting = "K"
  %Toeslag = "T"
  %Afrekening = "A"

-record(verrichting, {type = "B", datum = date(), nummer = 0, bedrag = 0, reden = ""}).

%Contract
-record(contract, {nummer = 0, eigenaar = #naam{}, huurder = #naam{}, huur_pm = 0,
                   voorschot_energie_pm = 0, begindatum = date(), aantal_maanden = 12}).

%%zou een map kunnen maken van nummer -> eigenaar, dan moeten we dit niet meer opslaan in contract