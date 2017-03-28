with GNAT.Sockets;             use GNAT.Sockets;

with Ada.Text_IO;              use Ada.Text_IO;
with Ada.Exceptions;           use Ada.Exceptions;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO.Unbounded_IO; use Ada.Text_IO.Unbounded_IO;


-- Simple Server.
-- 1) Receive client hostname and send ACK.
-- 2) Get client numbers, do calculations on it and
--    send back the result.
procedure server is
	Address : Sock_Addr_Type;
	Server  : Socket_Type;
	Socket  : Socket_Type;
	Channel : Stream_Access;

	Message_Received : Unbounded_String;
	Message_To_Send  : Unbounded_String;
	Number_Data      : Integer;

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
begin
	Address.Addr := Addresses (Get_Host_By_Name (Host_Name), 1);
	Address.Port := 5432;
	Create_Socket (Server);

	Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
	Bind_Socket (Server, Address);
	Listen_Socket (Server);
	Accept_Socket (Server, Socket, Address);

	Channel := Stream (Socket);

	Read_Channel (Channel, Message_Received);
	Put_Line (Message_Received & " Received by server.");

	Message_To_Send := "Server receided: " & Message_Received;
	Write_Channel (Channel, Message_To_Send);
	
	while(True) loop
		Read_Channel (Channel, Message_Received);
		Put_Line ("Received:" & Message_Received);

		Number_Data     := Integer'Value (To_String (Message_Received)) * 10;
		Message_To_Send := To_Unbounded_String (Number_Data'Img);
		
		Put_Line ("Sending" & Number_Data'Img);
		Write_Channel (Channel, Message_To_Send);
	end loop;

	Close_Socket (Server);
	Close_Socket (Socket);
end server;