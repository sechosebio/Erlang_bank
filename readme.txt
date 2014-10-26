%%%----------------------------------------------------------------------
%%% File    : 	readme.txt
%%% Authors  :	José A. L. Sebio 	<jose.antonio.lopez.sebio@udc.es>
%%%							Denis Graña 			<denis.gfernandez@udc.es>
%%% Created : 	26 October 2014
%%%----------------------------------------------------------------------


EXECUTION:

	To execute, is necessary run 3 nodes in 3 differente erlang consoles:

	erl -sname bank
	erl -sname client
	erl -sname atm 

	In the bank console
	>	c(bank).
	> node(). 
	Copy the result of the last command, and copy it in the file atm.erl and client.erl 
	in the define
	ex. -define(BANK_NODE,'bank@anger-2').
	
	> bank:start().
	
	In the user console
	> c(user).
	Now you can use all the functions of User.


	In the atm console
	> c(atm).
	Now you can use all the functions of User.

EXECUTION EXAMPLE:

	Bank console
		>bank:start().
		Bank Registered
		Atm connected with success
		Account Created
		User have 1000
		ok
		Withdrawed 999 of User Account
		User have 1
		Account Deleted
		User doesn't exists

	Atm console
		>atm:connect(1990).
		Atm connected with success
		ok
		>atm:withdraw_money(1,1234,999).
		Withdrawed 999 of User Account
		ok

	Client console
		>client:open_account(1,1234,1000).
		Account Created
		ok
		>client:check_money(1,1234).
		User have 1000
		ok
		>client:check_money(1,1234).
		User have 1
		ok
		>client:close_account(1,1234).
		Account Deleted
		ok
		client:check_money(1,1234).
		User doesn't exists
		ok

COMMENTS ABOUT IMPLEMENTATION:

	Function names:

		Banco																								 BANK
		abrir_conta(c: IDCliente,p:PIN,d:Cartos)					-> open_account(ID,PIN,MONEY)
		pechar_conta(c:IDCliente,p:PIN):Cartos						-> close_account(ID,PIN)
		conectar_caixeiro(atm:IDCaixeiro,d:Cartos)				-> connect_atm(ID,MONEY)
		desconectar_caixeiro(atm:IDCaixeiro)							-> disconnect_atm(ID)
		recargar_caixeiro(atm:IDCaixeiro,d:Cartos)				-> recharge_atm(ID,MONEY)
		retirar_cartos(c:IDCliente,p:PIN,d:Cartos):Cartos	-> withdraw_money(ID,PIN,MONEY)
		depositar_cartos(c:IDCliente,p:PIN,d:Cartos)			-> deposit_money(ID,PIN,MONEY)
		consultar_saldo(c:IDCliente,p:PIN):Cartos					-> check_money(ID,PIN)


		Caixeiro																							ATM
		conectar(d:Cartos)																-> connect(MONEY)
		descoenctar()																			-> disconnect()
		retirar_cartos(c:IDCliente,p:PIN,d:Cartos)				-> withdraw_money(ID,PIN,MONEY)
		depositar_cartos(c:IDcliente,p:PIN,d:Cartos)			-> deposit_money(ID,PIN,MONEY)
		recargar(d:Cartos)																-> recharge(MONEY)
