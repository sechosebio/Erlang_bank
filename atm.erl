%%%----------------------------------------------------------------------
%%% File    : 	atm.erl
%%% Author  :	José A. L. Sebio 	<jose.antonio.lopez.sebio@udc.es>
%%%			: 	Denis Graña 		<denis.gfernandez@udc.es>
%%% Purpose : 	atm client
%%% Created : 	24 October 2014
%%%----------------------------------------------------------------------
-module(atm).
-export([deposit_money/3,withdraw_money/3,connect/1,daemon/1,disconnect/0]).
-define(BANK_NODE,'bank@anger-2').

daemon(Money)->
	receive
		{money,Pid} ->	Pid ! Money,
						daemon(Money);
		{disconnect,Pid} -> Pid ! "Atm disconnected"
	end.

connect(Money)->
	case whereis(atm) of
		undefined	->
			case spawn(atm,daemon,[Money]) of
				Pid ->			
					register(atm,Pid),
					{bank,?BANK_NODE} ! {connectAtm,self(),Money,self()},
					receive
						Message -> io:format(Message)
					end
			end;
		_			->	io:format("Atm already connected\n")
	end.

disconnect()->
	{bank,?BANK_NODE} ! {disconnectAtm,self()},
	receive
		Message -> 	io:format(Message),
					atm ! disconnect
	end.

withdraw_money(Id,Pin,Money)->
	{bank,?BANK_NODE} ! {withdrawMoney,Id,Pin,Money,self()},
	receive
		Message -> 	io:format(Message)				
	end.

deposit_money(Id,Pin,Money)->
	{bank,?BANK_NODE} ! {depositMoney,Id,Pin,Money,self()},
	receive
		Message -> 	io:format(Message)				
	end.

%recharge(d)->
