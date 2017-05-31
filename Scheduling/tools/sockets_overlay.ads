
with Text_Io;
with Ada.Strings.Unbounded.Text_Io;

with Systems;               use Systems;
with GNAT.Sockets;          use GNAT.Sockets;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Multiprocessor_Services_Interface; use Multiprocessor_Services_Interface;
use Multiprocessor_Services_Interface.Scheduling_Result_Per_Processor_Package;

use type Multiprocessor_Services_Interface.Scheduling_Table_Range;


---------------------------------------------------------------------
-- Package Sockets_Overlay
-- Purpose: Contrains types definitions, functions and procedures
--   usefull for sockets manipulations such as IO and configuration.
--
-- Extra: Currently the sockets and channels are declared as global
--   variables in the package since we know that we only have 1 client
--   and 1 server using specifically 3 sockets: simple -> better
--   Implementation should be changed if tasks are added / multiple
--   connections clients. (clients list)
---------------------------------------------------------------------
package Sockets_Overlay is
  Address               : Sock_Addr_Type; 

  Server                : Socket_Type;  
  Ack_Socket            : Socket_Type;
  Data_Socket           : Socket_Type;
  Command_Socket        : Socket_Type; 

  Ack_Channel           : GNAT.Sockets.Stream_Access;
  Data_Channel          : GNAT.Sockets.Stream_Access;
  Command_Channel       : GNAT.Sockets.Stream_Access;

  Socket_Mode_Activated : Boolean := False;

  -- Sockets selector identifiers:
  Input_Status          : Selector_Status;
  Input_Selector        : Selector_Type;
  Input_Set             : Socket_Set_Type;
  WSet                  : Socket_Set_Type;

  type Communication_State is (Starting, Running, Ending);
  AADLInspector_Communication_State : Communication_State := Starting;

  procedure AADLInspector_Data_Communication(
      Sys              : in     System;
      Result           : in     Scheduling_Table_Ptr;
      J                : in     Scheduling_Table_Range;
      Current_Time     : in     Natural;
      Last_Time_Mod    : in out Natural;
      SpeedFactor      : in     Duration;
      Speed            : in out Duration;
      Slice_Size       : in out Natural;
      Exit_Simulation  : in out Boolean);

  ---------------------------------------------------------------------
  -- Close_Sockets
  -- Purpose: Close the three sockets used: Ack, Command, Data.
  ---------------------------------------------------------------------
  procedure Close_Sockets;

  ---------------------------------------------------------------------
  -- Connect_Socket
  -- Purpose: Used to make a simple connection between a client and the server.
  --          - Only a listen and accept statement.
  ---------------------------------------------------------------------
  procedure Connect_Socket (
    Serv    : in out Socket_Type;
    Client  : in out Socket_Type;
    Addr    : in     Sock_Addr_Type;
    Channel : in out GNAT.Sockets.Stream_Access);

  ---------------------------------------------------------------------
  -- Get_Command_Value
  -- Purpose: Return the value passed as argument of a socket command.
  --   The value of a command is in this format:
  --   "my_command [my_value]"
  ---------------------------------------------------------------------
  function Get_Command_Value (
    Command : Unbounded_String) 
    return    String;  

  ---------------------------------------------------------------------
  -- Initialize_Socket
  -- Purpose: Used to initialize a socket.
  --      - Only a bind and accept statement.
  ---------------------------------------------------------------------
  procedure Initialize_Socket (
    Address : in     Sock_Addr_Type;
    Serv    : in out Socket_Type);

  ---------------------------------------------------------------------
  -- Read
  -- Purpose: Read data on the socket channel.
  --
  -- Extra: At first, Read_Channel was used to get the input on sockets,
  --   tests was made between 2 ADA programs and no problem was detected.
  --   Since the connection is made between an ADA program (Cheddar) and an 
  --   other one (AADLInspector) the Read_Channel procedure wasn't functionnal
  --   anymore.
  --  
  --   The procedure was locked in a waiting state on reading Inputs from
  --   AADLInspector. After multiples tests and modifications, no solution was
  --   spotted. After internet research on GNAT.Socket documentation and
  --   code documentation of specification and body files no clue on the issue
  --   was found. A network analysis showed unspecifed data before the message
  --   sended from an ADA program.
  --  
  --   Indeed, it was found later that GNAT.Socket is adding a layer on top of
  --   the message. As said before, this layer was not specified on the
  --   GNAT.Socket documentation and was only spotted in a book: lien du livre.
  --   GNAT.Socket uses a specific communication protocol over the TCP stack,
  --   while sending a string with String'Output(s), the string is transmitted
  --   under the following form:
  --       * an integer with value 1 on 4 bytes in binary followed by,
  --       * an integer on 4 bytes in binary wich is the string length,
  --       * and then the characters of the string s.
  --   The function String'Input only accept this format too.
  --  
  --   So, this is a issue that you need to be aware if you try to communicate
  --   between an Ada program and another program written in another language.
  ---------------------------------------------------------------------
  function Read (Client  : in out Socket_Type;
                 Channel : in out Stream_Access) 
  return Unbounded_String;

  ---------------------------------------------------------------------
  -- Read_Channel
  -- Purpose: Read data on the specified socket channel. 
  -- Extra: This procedure assume that the communication is between
  --   Ada programs or betwenn programs in Ada and in different languages
  --   but implementing the protocol used by Gnat.Sockets. 
  --    
  --   [See 'Read' procedure Documentation for Issue]
  ---------------------------------------------------------------------
  procedure Read_Channel (
    Channel : in out GNAT.Sockets.Stream_Access;
    Data    :    out Unbounded_String);

  ---------------------------------------------------------------------
  -- Server_Start_Listening
  -- Purpose: Use to make the full connection on the three sockets used
  --   for AADLInspector<>Cheddar communication (ACK,DATA,COMMAND).
  --   Use Connect and Initialize procedures, bind the socket IO to
  --   the specified channel.
  ---------------------------------------------------------------------
  procedure Server_Start_Listening (
    Addr    : in     String;
    Port    : in     Port_Type;
    Socket  : in out Socket_Type;
    Channel : in out GNAT.Sockets.Stream_Access);

  ---------------------------------------------------------------------
  -- Start_State_Communication
  -- Run_State_Communication
  -- End_State_Communication
  --
  -- Purpose: They're aiming to receive commands from the client and
  --   send an acknowledgment message on receive. 
  --   Three functions are used currently because between the three
  --   states (Before simulation, During simulation and at the end of 
  --   the simulation when the last time is reached) differents
  --   commands can be received and are handled differently.

  -- TODO::Factoriser
  --------------------------------------------------------------------
  procedure AADLInspector_Command_Communication (
    Slice_Size       : in out Natural;
    Last_Time_Mod    : in out Natural;
    Speed            : in out Duration;
    SpeedFactor      : in     Duration;
    Exit_Simulation  : in out Boolean);

  procedure AADLInspector_Command_Start_State_Communication (
    Slice_Size       : in out Natural;
    Last_Time_Mod    : in out Natural;
    Speed            : in out Duration;
    SpeedFactor      : in     Duration;
    Exit_Simulation  : in out Boolean);

  procedure AADLInspector_Command_Run_State_Communication (
    Slice_Size       : in out Natural;
    Last_Time_Mod    : in out Natural;
    Speed            : in out Duration;
    SpeedFactor      : in     Duration;
    Exit_Simulation  : in out Boolean);

  procedure AADLInspector_Command_End_State_Communication (
    Slice_Size       : in out Natural;
    Last_Time_Mod    : in out Natural;
    Speed            : in out Duration;
    SpeedFactor      : in     Duration;
    Exit_Simulation  : in out Boolean);

  ---------------------------------------------------------------------
  -- Write_Channel
  -- Purpose: Write data on the specified socket channel.
  -- Extra: This procedure assume that the communication is between
  --   Ada programs or betwenn programs in Ada and in different languages
  --   but implementing the protocol used by Gnat.Sockets. 
  --  
  --   [See 'Read' procedure Documentation for Issue]
  ---------------------------------------------------------------------
  procedure Write_Channel (
    Channel : in out GNAT.Sockets.Stream_Access;
    Data    : in     Unbounded_String);

end Sockets_Overlay;