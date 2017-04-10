with GNAT.Sockets;                  use GNAT.Sockets;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with unbounded_strings;             use unbounded_strings;

package body Sockets_Overlay is
  -- Envoyer en ligne de commande a cheddar les infos
  -- -cheddar_d -f file.xmlv3 -n data
  -- send on socket continue, etc..
  -- receive play, if play unlock the simulation

    procedure Read_Channel (
    Channel : in out GNAT.Sockets.Stream_Access;
    Data    : out Unbounded_String)
    is
    begin
      Data := To_Unbounded_String (String'Input (Channel));
    end Read_Channel;

    procedure Write_Channel (
      Channel : in out GNAT.Sockets.Stream_Access;
      Data    : in     Unbounded_String)
    is
    begin
      String'Output (Channel, To_String(Data));
    end Write_Channel;

    procedure Server_Start_Listening (
      Addr    : in    String;
      Port    : in    Port_Type;
      Socket  : in out Socket_Type;
      Channel : in out GNAT.Sockets.Stream_Access)
    is
    begin
      Address.Addr := Inet_Addr (Addr); --127.0.0.1
      Address.Port := Port; --8901
      Create_Socket (Server);
      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Server, Address);
      Listen_Socket (Server);
      Accept_Socket (Server, Socket, Address);
  
      Channel := GNAT.Sockets.Stream (Socket);
    end Server_Start_Listening;

end Sockets_Overlay;

