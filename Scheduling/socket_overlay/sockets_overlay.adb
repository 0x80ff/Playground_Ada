with GNAT.Sockets;                  use GNAT.Sockets;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with unbounded_strings;             use unbounded_strings;

package body Sockets_Overlay is
    procedure Read_Channel (
    Channel : in out GNAT.Sockets.Stream_Access;
    Data    :    out Unbounded_String)
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

    function Substring (
      Str  : in Unbounded_String;
      From : in Natural;
      To   : in Natural)
    return Unbounded_String is
    begin
      return To_Unbounded_String (To_String (Str) (From..To));
    end Substring;

    function To_Marzin_Input_Format (
      Tick  : Natural;
      Event : Unbounded_String;
      Data  : Unbounded_String)
    return Unbounded_String is
      Return_String : Unbounded_String;
    begin
      Return_String := To_Unbounded_String (Tick'Img);
      Return_String := Trim(Return_String, Ada.Strings.Left);
      Return_String := "tick=" & Return_String & " " & Event & " " & Data;
      return Return_String;
    end To_Marzin_Input_Format;

    procedure Get_Event_String_From_XML (
      XML_String   : in     Unbounded_String;
      Event_String : in out Unbounded_String)
    is
      XML_Start_Tag_Time_Unit_Event : constant String := "<time_unit_event>";
      XML_End_Tag_Time_Unit_Event   : constant String := "</time_unit_event>";
      XML_Start_Tag_Type_Event      : constant String := "<type_of_event>";
      XML_End_Tag_Type_Event        : constant String := "</type_of_event>";
      XML_Start_Tag_Position        : Natural;
      XML_End_Tag_Position          : Natural;
      XML_Substring                 : Unbounded_String;
    begin
      -- Get time_unit_event tag start and end positions in the string
      XML_Start_Tag_Position := Index (XML_String, XML_Start_Tag_Time_Unit_Event) + XML_Start_Tag_Time_Unit_Event'Length;
      XML_End_Tag_Position   := Index (XML_String, XML_End_Tag_Time_Unit_Event) - 1;

      -- Get substring without time_unit_event tag
      XML_Substring := Substring (XML_String, XML_Start_Tag_Position, XML_End_Tag_Position);

      -- Get type_of_event tag start and end positions in the substring
      XML_Start_Tag_Position := Index (XML_Substring, XML_Start_Tag_Type_Event) + XML_Start_Tag_Type_Event'Length;
      XML_End_Tag_Position   := Index (XML_Substring, XML_End_Tag_Type_Event) - 1;

      -- Get the type of event string
      Event_String := Substring (XML_Substring, XML_Start_Tag_Position, XML_End_Tag_Position);
    end Get_Event_String_From_XML;
end Sockets_Overlay;