%%%----------------------------------------------------------------------
%%% File    : bank.erl
%%% Author  :	José A. L. Sebio <jose.antonio.lopez.sebio@udc.es>
%%%					: Denis Graña 		 <denis.gfernandez@udc.es>
%%% Purpose : bank
%%% Created : 24 October 2014
%%%----------------------------------------------------------------------

-module(bank).
-export([bank/1,open_account/4,close_account/3]).

bank(Data)->
	receive
		{openAccount,ClientId,Pin,Money}->
			open_account(Data,ClientId,Pin,Money);
		{closeAccount,ClientId,Pin}->
			close_account(Data,ClientId,Pin)
		%{withdrawMoney,ClientId,Pid,Pin,Money}->
		%	withdraw_money(Data,ClientId,Pin,Money);
		%{depositMoney,ClientId,Pid,Pin,Money}->
		%	deposity_money(Data,ClientId,Pin,Money);
		%{checkMoney,ClientId,Pid,Pin}->
		%	check_money(Data,ClientId,Pin)
	end.

open_account({ClientList,AtmList},ClientId,Pin,Money) ->
	%keyfind(Key, N, TupleList) -> Tuple | false 
	%N is the position of the key in the tuple
	case lists:keyfind(ClientId,1,ClientList) of
		false ->
			%Create new Tuple in the Client List that represents the Client data
			{lists:append(ClientList,[{ClientId,Pin,Money}]),AtmList};
		_->
			%Client exist, we do nothing
			{ClientList,AtmList}
		
	end.


close_account({ClientList,AtmList},ClientId,Pin)->
	case lists:keyfind(ClientId,1,ClientList) of
		false->
			%We don't have to delete this Client, bcos he don't exist
			{ClientList,AtmList};
		Tuple->
			%We have to delete this Client
			{lists:delete(Tuple,ClientList),AtmList}

	end.


%connect_atm(Data,Atm,Money)->
%disconnect_atm(Data,Atm)->
%withdraw_money(Data,ClientId,Pin,Money)->
%deposit_money(Data,ClientId,Pin,Money)->
%check_money(Data,ClientId,Pin)->

