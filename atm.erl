%%%----------------------------------------------------------------------
%%% File    : 	atm.erl
%%% Author  :	José A. L. Sebio 	<jose.antonio.lopez.sebio@udc.es>
%%%			: 	Denis Graña 		<denis.gfernandez@udc.es>
%%% Purpose : 	atm client
%%% Created : 	24 October 2014
%%%----------------------------------------------------------------------
-module(atm).
-export([deposit_money/3,withdraw_money/3,connect/1,daemon/1,disconnect/0,recharge/1]).
-define(BANK_NODE,'bank@anger-2').

daemon(Money)->
	receive
		{money,Pid} 					->	io:format("Atm have ~p€\n",[Money]),
											Pid ! Money,
											daemon(Money);
		{disconnect,Pid} 				-> 	Pid ! 	"Atm disconnected\n";
		{update,AddedMoney,Pid}			->	Pid ! 	"Added Money to Atm\n",
											daemon(Money+AddedMoney)
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
	case whereis(atm) of 
		undefined -> io:format("Atm is not connected\n");
		_		  ->
						{bank,?BANK_NODE} ! {disconnectAtm,self(),self()},
						receive
							Message -> 	io:format(Message),
										atm ! {disconnect,self()},
										receive
											Message2	-> io:format(Message2)
										end
						end
	end.

withdraw_money(Id,Pin,Money)->
	case whereis(atm) of 
		undefined	->	io:format("Atm is Offline\n");
		_			->
						atm ! {money,self()},
						receive
							CurrentMoney 	->	case CurrentMoney >= Money of
													true ->
														{bank,?BANK_NODE} ! {withdrawMoney,Id,Pin,Money,self()},
														receive
															{success,Message}	-> 	io:format(Message),
																					atm ! {update,-Money,self()},
																					receive
																						Message2 -> io:format(Message2)
																					end;

															Message 			-> 	io:format(Message)				
														end;
													false -> io:format("Atm don't have enough Cash\n")
												end
						end
	end.

deposit_money(Id,Pin,Money)->
	case whereis(atm) of 
		undefined	->	io:format("Atm is Offline\n");
		_			->
						{bank,?BANK_NODE} ! {depositMoney,Id,Pin,Money,self()},
						receive
							{success,Message}	->	io:format(Message),
													atm ! {update,Money,self()},
													receive
														Message2 -> io:format(Message2)
													end;
							Message 			-> 	io:format(Message)				
						end
	end.

recharge(Money)->
	case whereis(atm) of 
		undefined	->	io:format("Atm is Offline\n");
		_			->
						atm ! {update,Money,self()},
						receive
							Message2 -> io:format(Message2)
						end
	end.
