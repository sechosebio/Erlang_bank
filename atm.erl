%%%----------------------------------------------------------------------
%%% File    : 	atm.erl
%%% Author  :	José A. L. Sebio 	<jose.antonio.lopez.sebio@udc.es>
%%%			: 	Denis Graña 		<denis.gfernandez@udc.es>
%%% Purpose : 	atm client
%%% Created : 	24 October 2014
%%%----------------------------------------------------------------------
-module(atm).
-export([deposit_money/3,withdraw_money/3]).
-define(BANK_NODE,'bank@anger-2').

%connect(d)->

%disconnect()->

withdraw_money(Id,Pin,Money)->
	{bank,?BANK_NODE} ! {withdrawMoney,Id,Pin,Money,self()},
	receive
		Message -> 	io:format(Message)				
	end.
%NO VALE
deposit_money(Id,Pin,Money)->
	{bank,?BANK_NODE} ! {depositMoney,Id,Pin,Money,self()},
	receive
		Message -> 	io:format(Message)				
	end.

%recharge(d)->
