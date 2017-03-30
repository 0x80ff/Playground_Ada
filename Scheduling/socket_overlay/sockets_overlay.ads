with GNAT.Sockets;                  use GNAT.Sockets;
with Ada.Strings.Unbounded;			use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_Io;
with Text_Io;

package Sockets_Overlay is
    procedure Read_Channel (
      	Channel : in out GNAT.Sockets.Stream_Access;
      	Data    :    out Unbounded_String);

    procedure Write_Channel (
        Channel : in out GNAT.Sockets.Stream_Access;
        Data    : in     Unbounded_String);

    function Substring (
    	Str  : in Unbounded_String;
    	From : in Natural;
    	To   : in Natural)
    	return    Unbounded_String;

    function To_Marzin_Input_Format (
    	Tick  : in Natural;
    	Event : in Unbounded_String;
    	Data  : in Unbounded_String)
    	return     Unbounded_String;

    procedure Get_Event_String_From_XML (
      XML_String   : in     Unbounded_String;
      Event_String : in out Unbounded_String);
    
end Sockets_Overlay;