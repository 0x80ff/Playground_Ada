with Ada.Streams;
with Ada.Streams.Stream_IO;

with Tasks;                             use Tasks;
with Systems;                           use Systems;
with Task_Set;                          use Task_Set;
with Resources;                         use Resources;
with Ada.Text_IO;                       use Ada.Text_IO;
with Resource_Set;                      use Resource_Set;
with GNAT.Sockets;                      use GNAT.Sockets;
with Marzhin_Utils;                     use Marzhin_Utils;
with Ada.Strings.Maps;                  use Ada.Strings.Maps;
with Time_Unit_Events;                  use Time_Unit_Events;
with Ada.Strings.Fixed;                 use Ada.Strings.Fixed;
with unbounded_strings;                 use unbounded_strings;
with Ada.Strings.Unbounded;             use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO;     use Ada.Strings.Unbounded.Text_IO;
with Multiprocessor_Services_Interface; use Multiprocessor_Services_Interface;

use type Ada.Streams.Stream_Element_Count;
use type Time_Unit_Events.Time_Unit_Range;
use type Multiprocessor_Services_Interface.Scheduling_Table_Range;

--#[debug]
with Debug; use Debug;


---------------------------------------------------------------------
-- Package Sockets_Overlay
-- Purpose: Contrains types definitions, functions and procedures
--          usefull for sockets manipulations such as IO and configuration.
-- Extra: # Documentations on methods are puts in the specification file.
--        # AADLInspector related procedures could be factorized and
--          added to a new package `AADLInspector_Communication`.
---------------------------------------------------------------------
package body Sockets_Overlay is

    -- ASCII strings:
    LF   : constant Character := ASCII.LF;
    CRLF : constant String    := ASCII.CR & ASCII.LF;

    --------------------------------------
    -- AADLInspector_Data_Communication --
    --------------------------------------

    procedure AADLInspector_Data_Communication(
      Sys              : in     System;
      Result           : in     Scheduling_Table_Ptr;
      J                : in     Scheduling_Table_Range;
      Current_Time     : in     Natural;
      Last_Time_Mod    : in out Natural;
      SpeedFactor      : in     Duration;
      Speed            : in out Duration;
      Slice_Size       : in out Natural;
      Exit_Simulation  : in out Boolean)
    is
      Event_XML          : Unbounded_String;
      Event_String       : Unbounded_String;
      Task_Id_String     : Unbounded_String;
      Message_To_Send    : Unbounded_String;
      Message_Received   : Unbounded_String;
      Resource_Id_String : Unbounded_String;
      Current_Task       : Generic_Task_Ptr;
      Current_Resource   : Generic_Resource_Ptr;
    begin
      -- For each scheduler entrey check the event entries at current time
      -- and construct the server response with current events data:
      for Current_Entry in 0 .. Result.entries (J).data.result.nb_entries - 1 loop
        
        -- Check for arrival of a socket commmand:
        GNAT.Sockets.Set(Input_Set, Command_Socket);
        Check_Selector(Input_Selector, Input_Set, WSet, Input_Status, 0.0);
        if Input_Status = Completed then
          AADLInspector_Communication_State := Running;
          AADLInspector_Command_Communication(Slice_Size, Last_Time_Mod, Speed, SpeedFactor, Exit_Simulation);
          if Exit_Simulation then return; end if;
        end if;

        -- Process the time unit event data to be sent on socket:
        if Result.entries (J).data.result.entries (Current_Entry).item = Current_Time then
           -- Initialization value for current entry:
          Event_XML       := xml_string (Result.entries (J).data.result.entries (Current_Entry).data);
          Message_To_Send := To_Unbounded_String("");

          -- Get the Cheddar's event name:
          Get_Event_String_From_XML(Event_XML, Event_String);

          -- Get the current resource of the event:
          if Is_Resource_Event(Event_String) then
            Get_Resource_Id_From_XML(Event_XML, Event_String, Resource_Id_String);
            Current_Resource := Search_Resource_by_id(Sys.Resources, Resource_Id_String);
          end if;

          -- Get the task linked to the current event:
          Get_Task_Id_From_XML(Event_XML, Event_String, Task_Id_String);
          Current_Task := Search_Task_by_id(Sys.Tasks, Task_Id_String);

          -- Transform the Cheddar EventString to a Marzhin EventString:
          Set_To_Marzhin_Event(Event_String);

          -- Output the event on socket:
          Message_To_Send := To_Marzhin_Output_Format(Current_Time, Event_String, Current_Task.name) & Message_To_Send;
          Write_Channel (Data_Channel, Message_To_Send & LF);
--#[debug]
          Put_Debug("__DEBUG__ :: " & Message_To_Send & LF);

          -- Output resource event on socket (if exist):
          if Is_Resource_Event(Event_String) then
            Set_To_Marzhin_Resource_Event(Event_String);
            Message_To_Send := To_Marzhin_Output_Format(Current_Time, Event_String, Current_Resource.name);
            Write_Channel (Data_Channel, Message_To_Send & LF);
--#[debug]
            Put_Debug("__DEBUG__ :: " & Message_To_Send & LF);
          end if;
--#[debug]
          Put_Debug("__DEBUG__ :: " & Event_XML);
        end if;
      end loop;

      -- Output end of tick event:
      Message_To_Send := To_Marzhin_Output_Format(Current_Time, To_Unbounded_String("PROCESS_END_TICK"), Current_Task.address_space_name);
      Write_Channel (Data_Channel, Message_To_Send & LF);
--#[debug]
      Put_Debug("__DEBUG__ :: " & Message_To_Send & LF);

      delay Speed;
    end AADLInspector_Data_Communication;

    -------------------
    -- Close_Sockets --
    -------------------

    procedure Close_Sockets is
    begin
      Close_Socket(Ack_Socket);
      Close_Socket(Data_Socket);
      Close_Socket(Command_Socket);
    end Close_Sockets;

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

    procedure AADLInspector_Command_Communication (
      Slice_Size       : in out Natural;
      Last_Time_Mod    : in out Natural;
      Speed            : in out Duration;
      SpeedFactor      : in     Duration;
      Exit_Simulation  : in out Boolean
      )
    is 
    begin
      case AADLInspector_Communication_State is
        when Starting => AADLInspector_Command_Start_State_Communication (Slice_Size, Last_Time_Mod, Speed, SpeedFactor, Exit_Simulation);
        when Running  => AADLInspector_Command_Run_State_Communication   (Slice_Size, Last_Time_Mod, Speed, SpeedFactor, Exit_Simulation);
        when Ending   => AADLInspector_Command_End_State_Communication   (Slice_Size, Last_Time_Mod, Speed, SpeedFactor, Exit_Simulation);
        when others   => null;
      end case;
    end AADLInspector_Command_Communication;

    -------------------------------
    -- Start_State_Communication --
    -------------------------------

    procedure AADLInspector_Command_Start_State_Communication (
      Slice_Size       : in out Natural;
      Last_Time_Mod    : in out Natural;
      Speed            : in out Duration;
      SpeedFactor      : in     Duration;
      Exit_Simulation  : in out Boolean)
    is
      Message_Received : Unbounded_String;
      Message_To_Send  : Unbounded_String;
    begin
      -- Initialization ended, waiting for a socket message:
      Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);

      Put_Debug("__INFO__ :: Message_Received:" & Message_Received);
      while(Index(Message_Received, "simulator play") <= 0) loop
        if (Index(Message_Received, "slice") > 0) then
          Slice_Size    := Integer'Value (Get_Command_Value(Message_Received));
          Last_Time_Mod := Slice_Size;
        elsif (Index(Message_Received, "speed") > 0) then
          Message_To_Send := To_Unbounded_String("Change time between tick OK :*");
          Write_Channel (Ack_Channel, Message_To_Send & Character'Val(10));
          Speed := SpeedFactor * Duration'Value (Get_Command_Value(Message_Received));
        elsif (Index(Message_Received, "get_tasks_capacities") > 0) then
          Put_Debug("__INFO__ :: Request for table of tasks capacities");
          -- Message_To_Send := Encode_Task_Capacities(Si);
        end if;
        Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);
      end loop;
    end AADLInspector_Command_Start_State_Communication;

    -----------------------------
    -- Run_State_Communication --
    -----------------------------

    procedure AADLInspector_Command_Run_State_Communication (
      Slice_Size       : in out Natural;
      Last_Time_Mod    : in out Natural;
      Speed            : in out Duration;
      SpeedFactor      : in     Duration;
      Exit_Simulation  : in out Boolean)
    is
      Message_Received : Unbounded_String;
      Message_To_Send  : Unbounded_String;
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
    end AADLInspector_Command_Run_State_Communication;

    -----------------------------
    -- End_State_Communication --
    -----------------------------
    
    procedure AADLInspector_Command_End_State_Communication (
      Slice_Size       : in out Natural;
      Last_Time_Mod    : in out Natural;
      Speed            : in out Duration;
      SpeedFactor      : in     Duration;
      Exit_Simulation  : in out Boolean)
    is
      Message_Received : Unbounded_String;
      Message_To_Send  : Unbounded_String;
    begin
      Message_Received := Sockets_Overlay.Read(Command_Socket, Command_Channel);
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
    end AADLInspector_Command_End_State_Communication;

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

