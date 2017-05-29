with GNAT.Sockets;             use GNAT.Sockets;

with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;

-- Simple Client.
-- 1) Send it's hostname to the server, get ACK.
-- 2) Send the numbers from 0 to 10 to the server
-- 3) Get ACK after each data sended.
procedure client_cheddar_command is
	Address : Sock_Addr_Type;
	Socket  : Socket_Type;
	Channel : Stream_Access;
	Data    : Unbounded_String;

	procedure Read_Channel (
		Channel : in out Stream_Access;
		Data	:    out Unbounded_String)
	is
	begin
		Data := To_Unbounded_String (String'Input (Channel));
	end Read_Channel;

	procedure Write_Channel (
		Channel : in out Stream_Access;
		Data    : in     Unbounded_String)
	is
	begin
		String'Output (Channel, To_String(Data));
	end Write_Channel;

	procedure Open_Connection (
		Port    : in Port_Type;
		IpAddr  : in String;
		Socket  : in out Socket_Type)
	is
	begin
		Address.Addr := Inet_Addr (IpAddr);
		Address.Port := Port;
		Create_Socket (Socket);

		Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
		Connect_Socket (Socket, Address);
	end Open_Connection;

begin
	Open_Connection (8902, "192.168.1.34", Socket);
	Channel := Stream (Socket);
	--Data    := To_Unbounded_String (Host_Name);

	-- Write_Channel (Channel, Data);
	-- Put_Line (String'Input (Channel));

	while (True) loop
		Put_Line("Enter command:");
		Write_Channel(Channel, To_Unbounded_String(Get_Line));
		--Read_Channel(Channel, Data);

		exit when To_String(Data) = "END";
		Put_Line (Data);
		
	end loop;

end client_cheddar_command;
