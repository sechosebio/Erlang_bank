%%%----------------------------------------------------------------------
%%% File    :	client.erl
%%% Author  :	José A. L. Sebio 	<jose.antonio.lopez.sebio@udc.es>
%%%			: 	Denis Graña 		<denis.gfernandez@udc.es>
%%% Purpose : 	client
%%% Created : 	24 October 2014
%%%----------------------------------------------------------------------

-module(client).
-export([open_account/3,close_account/2,check_money/2]).
-define(BANK_NODE,'bank@anger-2').

open_account(Id,Pin,Money)->
	{bank,?BANK_NODE} ! {openAccount,Id,Pin,Money,self()},
	receive
		Message -> 	io:format(Message)				
	end.

close_account(Id,Pin)->
	{bank,?BANK_NODE} ! {closeAccount,Id,Pin,self()},
	receive
		Message -> io:format(Message)
	end.
check_money(Id,Pin)->
	{bank,?BANK_NODE} ! {checkMoney,Id,Pin,self()},
	receive
		Message	-> io:format(Message)
	end.
