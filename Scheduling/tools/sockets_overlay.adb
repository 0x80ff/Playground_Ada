with Ada.Text_IO;                   use Ada.Text_IO;
with GNAT.Sockets;                  use GNAT.Sockets;
with Ada.Strings.Maps;              use Ada.Strings.Maps;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with unbounded_strings;             use unbounded_strings;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with Ada.Streams;
with Ada.Streams.Stream_IO;

use type Ada.Streams.Stream_Element_Count;

---------------------------------------------------------------------
-- Package Sockets_Overlay
-- Purpose: Contrains types definitions, functions and procedures
--          usefull for sockets manipulations such as IO and configuration.
-- Extra: Documentations on methods are putting in the specification file.
---------------------------------------------------------------------
package body Sockets_Overlay is

    CRLF: constant String := ASCII.CR & ASCII.LF;

    --------------------
    -- Connect_Socket --
    --------------------

    procedure Connect_Socket (
      Serv    : in out Socket_Type;
      Client  : in out Socket_Type;
      Addr    : in     Sock_Addr_Type;
      Channel : in out GNAT.Sockets.Stream_Access)
    is
    begin
      Listen_Socket (Serv);
      Accept_Socket (Serv, Client, Address);
      Channel := GNAT.Sockets.Stream (Client);
    end Connect_Socket;

    -----------------------
    -- Get_Command_Value --
    -----------------------

    function Get_Command_Value (
      Command : Unbounded_String) 
      return    String
    is
    begin
      return To_String(Substring (Command,
                                (Index (Command, "[") + 1),
                                (Index (Command, "]") - 1)));
    end Get_Command_Value;

    -----------------------
    -- Initialize_Socket --
    -----------------------

    procedure Initialize_Socket (
      Address : in     Sock_Addr_Type;
      Serv    : in out Socket_Type)
    is
    begin
      Create_Socket (Serv);
      Set_Socket_Option (Serv, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Serv, Address);
    end Initialize_Socket;

    ----------
    -- Read --
    ----------

    function Read (Client  : in out Socket_Type;
                   Channel : in out Stream_Access) 
    return Unbounded_String is
      Offset     : Ada.Streams.Stream_Element_Count;
      Buffer     : String (1..256);
      Result     : Unbounded_String := To_Unbounded_String("");
      Byte_Count : Natural;
    begin
      Byte_Count := 0 ;
      for Index in Buffer'Range loop
        Byte_Count := Byte_Count + 1 ;
        Character'Read (Channel, Buffer (Byte_Count));
        if Buffer(Byte_Count) = ASCII.LF then
          if Buffer(Byte_Count - 1) = ASCII.CR then
            Result := Result & Buffer (1 .. Byte_Count-2);
            return Result;
          end if;
        end if;
      end loop;
      return Result;
    end;

    ------------------
    -- Read_Channel --
    ------------------

    procedure Read_Channel (
    Channel : in out GNAT.Sockets.Stream_Access;
    Data    : out Unbounded_String)
    is
      Size : Integer;
    begin
      Data := To_Unbounded_String (String'Input (Channel));
    end Read_Channel;

    ----------------------------
    -- Server_Start_Listening --
    ----------------------------

    procedure Server_Start_Listening (
      Addr    : in    String;
      Port    : in    Port_Type;
      Socket  : in out Socket_Type;
      Channel : in out GNAT.Sockets.Stream_Access)
    is
    begin
      Address.Addr := Inet_Addr (Addr);
      Address.Port := Port;
      Create_Socket (Server);
      Set_Socket_Option (Server, Socket_Level, (Reuse_Address, True));
      Bind_Socket (Server, Address);
      Listen_Socket (Server);
      Accept_Socket (Server, Socket, Address);
  
      Channel := GNAT.Sockets.Stream (Socket);
    end Server_Start_Listening;

    -------------------
    -- Write_Channel --
    -------------------

    procedure Write_Channel (
      Channel : in out GNAT.Sockets.Stream_Access;
      Data    : in     Unbounded_String)
    is
    begin
      String'Output (Channel, To_String(Data));
    end Write_Channel;

end Sockets_Overlay;

