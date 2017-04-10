with GNAT.Sockets;          use GNAT.Sockets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_Io;
with Text_Io;

package Sockets_Overlay is
  
    Address         : Sock_Addr_Type;
    Server          : Socket_Type;
    Data_Socket     : Socket_Type;
    Command_Socket  : Socket_Type;
    Data_Channel    : GNAT.Sockets.Stream_Access;
    Command_Channel : GNAT.Sockets.Stream_Access;

    procedure Read_Channel (
        Channel : in out GNAT.Sockets.Stream_Access;
        Data    :    out Unbounded_String);

    procedure Write_Channel (
        Channel : in out GNAT.Sockets.Stream_Access;
        Data    : in     Unbounded_String);

    procedure Server_Start_Listening (
      Addr    : in     String;
      Port    : in     Port_Type;
      Socket  : in out Socket_Type;
      Channel : in out GNAT.Sockets.Stream_Access);
    
end Sockets_Overlay;