%%%----------------------------------------------------------------------
%%% File    : 	bank.erl
%%% Author  :	José A. L. Sebio 	<jose.antonio.lopez.sebio@udc.es>
%%%			:	Denis Graña 		<denis.gfernandez@udc.es>
%%% Purpose : 	bank
%%% Created : 	24 October 2014
%%%----------------------------------------------------------------------

-module(bank).
-export([start/0,bank/1,open_account/4,close_account/3]).

%%
% Initiallize the bank process, register the processes with name bank
%
start()->
	%registering bank process for node comunications
	case whereis(bank) of
		undefined	->	register(bank,self()),
						io:format("Bank Registered\n"),
						bank(
							{[],[]}
						);
		_			->	io:format("Bank Already Registered\n"),
						bank({[],[]})
	end.

%%
% Is the main function of the program, It listen for messages
%
bank(Data)->
	receive
		{openAccount,ClientId,Pin,Money,Pid}->
			case open_account(Data,ClientId,Pin,Money) of
				user_exists ->		io:format("Can't create account, User Exists\n"),
									Pid ! "Can't create account, User Exists\n",
									bank(Data);
				Tuple -> 	io:format("Account Created\n"),
							Pid ! "Account Created\n",
							bank(Tuple)
			end;
		{closeAccount,ClientId,Pin,Pid}->
			case close_account(Data,ClientId,Pin) of
				user_doesnt_exists 	->	io:format("Can't Close Account because user doesn't exists\n"),
										Pid ! "Can't Close Account because user doesn't exists\n",
										bank(Data);
				wrong_pin			->	io:format("Can't Close Account because Pin is not correct\n"),
										Pid ! "Can't Close Account because Pin is not correct\n",
										bank(Data);
				Tuple -> 	io:format("Account Deleted\n"),
							Pid ! "Account Deleted\n",
							bank(Tuple)
			end;
		{withdrawMoney,ClientId,Pin,Money,Pid}->			
			case withdraw_money(Data,ClientId,Pin,Money) of
				user_doesnt_exists 	->	io:format("User doesn't exists\n"),
										Pid ! "User doesn't exists\n",
										bank(Data);
				wrong_ping 			->	io:format("Wrong Pin\n"),
										Pid ! "Wrong Pin\n",
										bank(Data);
				wrong_quantity		->	io:format("Withdrawed Money must be > 0€\n"),
										Pid ! "Withdrawed Money must be > 0€\n",
										bank(Data);
				not_enough_money	->	io:format("Not enough money to withdraw\n"),
										Pid ! "Not enough money to withdraw\n",
										bank(Data);
				UpdatedData 		->	io:format("Withdrawed ~p€ of User Account\n",[Money]),
										Pid ! lists:concat(["Withdrawed ",Money,"€ of User Account\n"]),
										bank(UpdatedData)
			end;
		{depositMoney,ClientId,Pin,Money,Pid}->			
			case deposit_money(Data,ClientId,Pin,Money) of
				user_doesnt_exists 	->	io:format("User doesn't exists\n"),
										Pid ! "User doesn't exists\n",
										bank(Data);
				wrong_ping 			->	io:format("Wrong Pin\n"),
										Pid ! "Wrong Pin\n",
										bank(Data);
				wrong_quantity		->	io:format("Deposited Money must be > 0€\n"),
										Pid ! "Deposited Money must be > 0€\n",
										bank(Data);
				UpdatedData 		->	io:format("Added ~p€ to User Account\n",[Money]),
										Pid ! lists:concat(["Added ",Money,"€ to User Account\n"]),
										bank(UpdatedData)
			end;
		{checkMoney,ClientId,Pin,Pid}->
			case check_money(Data,ClientId,Pin) of
				user_doesnt_exists 	->	io:format("User doesn't exists\n"),
										Pid ! "User doesn't exists\n",
										bank(Data);
				wrong_ping 			->	io:format("Wrong Pin\n"),
										Pid ! "Wrong Pin\n",
										bank(Data);
				Money 				->	io:format("User have ~p€\n",[Money]),
										Pid ! lists:concat(["User have ",Money,"€\n"]),
										bank(Data)
			end;

		%FUNCTION FOR DEBUGGING
		stop -> Data;
		%END FUNCTION FOR DEBUGGING
		_ -> 		io:format("Uknown Message\n"),
					bank(Data)
	end.

% ------------------------------------------------------ %
% -------------------BANK FUNCTUIONS-------------------- %
% ------------------------------------------------------ %
open_account({ClientList,AtmList},ClientId,Pin,Money) ->
	%keyfind(Key, N, TupleList) -> Tuple | false 
	%N is the position of the key in the tuple
	case lists:keyfind(ClientId,1,ClientList) of
		false ->
			%Create new Tuple in the Client List that represents the Client data
			{lists:append(ClientList,[{ClientId,Pin,Money}]),AtmList};
		_->
			%Client exist, we do nothing
			user_exists	
	end.


close_account({ClientList,AtmList},ClientId,Pin)->
	case lists:keyfind(ClientId,1,ClientList) of
		false->
			%User doesn't exists
			user_doesnt_exists;
		Tuple->
			%Check the pin
			case Tuple of
				{_,Pin,_} 	->	%We have to delete this Client
								{lists:delete(Tuple,ClientList),AtmList};
				_			->	%The pin is incorrect
								wrong_pin
			end
	end.


%connect_atm(Data,Atm,Money)->
%disconnect_atm(Data,Atm)->
withdraw_money({ClientList,AtmList},ClientId,Pin,Money)->
	case Money > 0 of
		true 	->
			case lists:keyfind(ClientId,1,ClientList) of
				false			-> user_doesnt_exists;
				{_,Pin,OldMoney}-> 
					case OldMoney>=Money of
						true ->
							{
								lists:keyreplace(ClientId, 1, ClientList, {ClientId,Pin,OldMoney-Money}),
								AtmList
							};
						false -> not_enough_money
					end;
				_				-> wrong_ping
			end; 
		false	-> wrong_quantity
	end.
deposit_money({ClientList,AtmList},ClientId,Pin,Money)->
	case Money > 0 of
		true ->
			case lists:keyfind(ClientId,1,ClientList) of
				false			-> user_doesnt_exists;
				{_,Pin,OldMoney}-> {
										lists:keyreplace(ClientId, 1, ClientList, {ClientId,Pin,OldMoney+Money}),
										AtmList
									};
				_				-> wrong_ping 
			end;
		false ->	wrong_quantity
	end.
check_money({ClientList,_},ClientId,Pin)-> 
	case lists:keyfind(ClientId,1,ClientList) of
		false			-> user_doesnt_exists;
		{_,Pin,Money}	-> Money;
		_				-> wrong_ping 
	end.

