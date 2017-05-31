with Ada.Text_IO;                   use Ada.Text_IO;
with GNAT.Sockets;                  use GNAT.Sockets;
with Ada.Strings.Maps;              use Ada.Strings.Maps;
with Ada.Strings.Fixed;             use Ada.Strings.Fixed;
with unbounded_strings;             use unbounded_strings;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

with Ada.Streams;
with Ada.Streams.Stream_IO;

with Debug; use Debug;

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
      -- Offset     : Ada.Streams.Stream_Element_Count;
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
      -- Size : Integer;
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

    -------------------------------
    -- Start_State_Communication --
    -------------------------------

    procedure Start_State_Communication (
      Message_Received : in out Unbounded_String;
      Message_To_Send  : in out Unbounded_String;
      Slice_Size       : in out Natural;
      Last_Time_Mod    : in out Natural;
      Speed            : in out Duration;
      SpeedFactor      : in     Duration)
    is
    begin
     -- Initialization ended, waiting for a socket message:
    Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);

    Put_Debug("Message_Received::" & Message_Received);
      Put_Debug("__INFO__ :: Simulation_Start_Communication");
      while(Index(Message_Received, "simulator play") <= 0) loop
        if (Index(Message_Received, "slice") > 0) then
          Slice_Size    := Integer'Value (Get_Command_Value(Message_Received));
          Last_Time_Mod := Slice_Size;
        elsif (Index(Message_Received, "speed") > 0) then
          Message_To_Send := To_Unbounded_String("Change time between tick OK :*");
          Write_Channel (Ack_Channel, Message_To_Send & Character'Val(10));
          Speed := SpeedFactor * Duration'Value (Get_Command_Value(Message_Received));
        elsif (Index(Message_Received, "get_tasks_capacities") > 0) then
          Put_Debug("Request for table of tasks capacities");
          -- Message_To_Send := Encode_Task_Capacities(Si);
        end if;
        Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);
      end loop;
    end Start_State_Communication;

    -----------------------------
    -- Run_State_Communication --
    -----------------------------

    procedure Run_State_Communication (
      Message_Received : in out Unbounded_String;
      Message_To_Send  : in out Unbounded_String;
      Slice_Size       : in out Natural;
      Last_Time_Mod    : in out Natural;
      Speed            : in out Duration;
      SpeedFactor      : in     Duration;
      Exit_Simulation  : in out Boolean)
    is
    begin
      Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);
      if(To_String(Message_Received) = "simulator pause") then
        Message_To_Send := To_Unbounded_String("pause OK");
        Write_Channel (Ack_Channel, Message_To_Send & Character'Val(10));
        while(To_String(Message_Received) /= "simulator resume") loop
          Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);
          if (Index(Message_Received, "slice") > 0) then
            Slice_Size := Integer'Value (Get_Command_Value(Message_Received));
            Last_Time_Mod := Last_Time_Mod + Slice_Size;
          elsif (Index(Message_Received, "speed") > 0) then
            Message_To_Send := To_Unbounded_String("Change time between tick OK :*");
            Write_Channel (Ack_Channel, Message_To_Send & Character'Val(10));
            Speed := SpeedFactor * Duration'Value (Get_Command_Value(Message_Received));
          end if;
        end loop;
        Message_To_Send := To_Unbounded_String("resume OK");
        Write_Channel (Ack_Channel, Message_To_Send & Character'Val(10));
      elsif (Index(Message_Received, "slice") > 0) then
        Slice_Size := Integer'Value (Get_Command_Value(Message_Received));
        Last_Time_Mod := Last_Time_Mod + Slice_Size;
      elsif (Index(Message_Received, "speed") > 0) then
        Message_To_Send := To_Unbounded_String("Change time between tick OK :*");
        Write_Channel (Ack_Channel, Message_To_Send & Character'Val(10));
        Speed := SpeedFactor * Duration'Value (Get_Command_Value(Message_Received));
      elsif To_String(Message_Received) = "simulator stop" then Exit_Simulation := True; end if;
    end Run_State_Communication;

    -----------------------------
    -- End_State_Communication --
    -----------------------------
    
    procedure End_State_Communication (
      Message_Received : in out Unbounded_String;
      Message_To_Send  : in out Unbounded_String;
      Slice_Size       : in out Natural;
      Last_Time_Mod    : in out Natural;
      Speed            : in out Duration;
      SpeedFactor      : in     Duration;
      Exit_Simulation  : in out Boolean)
    is
    begin
      Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);
      --if (To_String(Message_Received) = "play" or Index(Message_Received, "slice") > 0 or Index(Message_Received, "speed") > 0) then
      while(To_String(Message_Received) /= "simulator play") loop
        if (Index(Message_Received, "slice") > 0) then
          Slice_Size := Integer'Value (Get_Command_Value(Message_Received));
        end if;
        if (Index(Message_Received, "speed") > 0) then
          Message_To_Send := To_Unbounded_String("Change time between tick OK :*");
          Write_Channel (Ack_Channel, Message_To_Send & Character'Val(10));
          Speed := SpeedFactor * Duration'Value (Get_Command_Value(Message_Received));
        end if;
        if To_String(Message_Received) = "simulator stop" then Exit_Simulation := True; end if;
          exit when To_String(Message_Received) = "simulator stop";
        Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);
      end loop;

      Last_Time_Mod := Last_Time_Mod + Slice_Size;
    end End_State_Communication;

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

