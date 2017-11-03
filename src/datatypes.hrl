%%%-------------------------------------------------------------------
%%% @author Niels Pirotte
%%% @copyright (C) 2017, UHasselt
%%% @doc
%%% implements necessary datatypes
%%% @end
%%% Created : 03. Nov 2017 3:44 PM
%%%-------------------------------------------------------------------
-author("Niels Pirotte").


%types van verrichtingen
-record(betaling, {datum = date(), nummer = 0, bedrag = 0}).
-record(korting, {datum = date(), nummer = 0, bedrag = 0, reden = ""}).
-record(toeslag, {datum = date(), nummer = 0, bedrag = 0, reden = ""}).
-record(afrekening, {datum = date(), nummer = 0, bedrag = 0}).