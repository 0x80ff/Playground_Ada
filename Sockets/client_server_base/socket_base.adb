--
-- TCP/IP Echo Service Daemon, based on RFC 342 and RFC 862
-- Normally, a Sender program that accepts a word or quoted
-- sentence from command line is transmitted to the server and
-- the return message is displayed.
--
-- This Daemon was tested using "Ada Dual TCP/IP Stacks" using
-- IPv6 and Linux IPv4,
--
--
-- Protocol: TCP
-- Protocol: UDP not setup at this time.
--
-- Usage: Listener
--
-- Tester: Telnet <hostname> -port 7
-- Example: Telnet 127.0.0.1 7
--
-- While telnet operational every character including control
-- characters will be echoed
--
with Ada.Characters.Latin_9 ;
with Ada.Exceptions ;
with Ada.Text_IO ;
with GNAT.Sockets ; -- GNAT to reg OS
-- with Sockets ; -- Ada TCP/IP Stacks

use Ada.Characters.Latin_9 ;
use Ada.Exceptions ;
use Ada.Text_IO ;
use GNAT.Sockets ;
-- use Sockets ; -- Ada TCP/IP Stacks

procedure Listener is

--
-- Operational options
--
-- Set to True to use IPv6, But some GNAT.Socket packages, are
-- not setup to handle IPv6 yet. One way to check is to look at
-- the body for the "Bind" function. Because the "Bind" function
-- raises a exception, if IPv6 is use, for those packages that
-- are IPv4 only.
--
IP_V6 : constant Boolean := False ;
--
-- Displays a logging message if True
--
LOGGING : constant Boolean := False ;

task type Echo is
entry Start ( Incoming : Socket_Type ) ;
end Echo ;

type Echo_Access is access Echo ;
type Sock_Addr_Access is access all Sock_Addr_Type ;

--
-- Echo -- the main processthat preforms the echo operation
-- -- one problem is there is no recovery of memory use
-- by this task once the task ends.
--

task body Echo is
Data : Character ;

Channel : Stream_Access ;
Socket : Socket_Type ;

begin
accept Start ( Incoming : Socket_Type ) do
Socket := Incoming ;
end Start ;

Channel := Stream ( Socket ) ;
loop
Data := Character ' Input ( Channel ) ;
exit when Data = ASCII.Nul ;
Character ' Output ( Channel, Data ) ;
end loop ;
Close_Socket ( Socket ) ;
exception
when Socket_Error =>
Put_Line ( "Connection closed" ) ;
Close_Socket ( Socket ) ;
end Echo ;

--
Accepting_Socket : Socket_Type ;
Incoming_Socket : Socket_Type ;
Address : Sock_Addr_Access ;

Dummy : Echo_Access ;

TCP_Error : exception ;

begin
--
-- Create Socket and sets stacks. With error checking to insure
-- stacks is valid because some system only have IPv4 and other
-- have remove IPv4. If both stacks are installed and GNAT
-- allows both, then use IPv6.
--
if IP_V6 then
begin
--
-- set IPv6
--
Create_Socket ( Accepting_Socket, Family_Inet6, Socket_Stream ) ;
Address := new Sock_Addr_Type ( Family_Inet6 );
exception
when Socket_Error =>
Put_Line ( "Error: IP version 6 is not supported" ) ;
raise TCP_Error ;
end ;
else
begin
--
-- set Default IPv4
--
Create_Socket ( Accepting_Socket ) ;
Address := new Sock_Addr_Type ;
exception
when Socket_Error =>
Put_Line ( "Error: IP version 4 is not supported" ) ;
raise TCP_Error ;
end ;
end if ;
--
-- Address.Addr is current host can be localhost
-- Address.Port is 7 based on RFC 342 update RFC 862
--
Address.all.Addr := Addresses ( Get_Host_By_Name ( Host_Name ), 1 ) ;
Address.all.Port := 7 ;
--
-- Bind Address to socket
--
Bind_Socket ( Accepting_Socket, Address.all ) ;
--
-- Set stacks to receive connect events
--
Listen_Socket ( Accepting_Socket ) ;
--
-- Handle connections
--
loop
--
-- Wait until client request connect then accept connection
--
Accept_Socket ( Accepting_Socket,
Incoming_Socket, Address.all ) ;
--
-- Log message, if required
--
if LOGGING then
Put ( "Received From: " ) ;
Put ( Image ( Address.all ) ) ;
end if ;
--
-- Create a single usage task to handle daemon process
-- task will die once connection is ended. In this design
-- there is a possible memory leak because once the task
-- dies there is no memory recover of the dead task.
--
Dummy := new Echo ;
Dummy.Start ( Incoming_Socket ) ;
end loop ;

exception
when TCP_Error =>
Put_Line ( "Error: Server was not initialized" ) ;

when others =>
Put_Line ( "Error: Server is beening shutdown" ) ;
Shutdown_Socket ( Accepting_Socket, Shut_Read_Write ) ;
end Listener ;