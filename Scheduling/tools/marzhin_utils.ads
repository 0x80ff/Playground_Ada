with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with unbounded_strings;             use unbounded_strings;

package Marzhin_Utils is
    type Port_Data is
    	record
    		Task_Data     : Unbounded_String;
    		Resource_Data : Unbounded_String;
    		State_Data    : Unbounded_String;
    	end record;

    type My_Array is array(Natural range <>) of Port_Data;

    type Port_List is
      	record
        	Port_Nb    : Natural := 0;
        	Port_Array : My_Array (0..10);
      	end record;

    Ports : Port_List;

    procedure Add_Port (
    	Task_Name     : in Unbounded_String;
    	Resource_Name : in Unbounded_String;
    	State_Value   : in Unbounded_String);
    
	procedure Set_To_Marzin_Event (Event_String : in out Unbounded_String);

    function To_Marzin_Input_Format (
    	Tick  : in Natural;
    	Event : in Unbounded_String;
    	Data  : in Unbounded_String)
    	return     Unbounded_String;

	procedure Get_Event_String_From_XML (
      XML_String   : in     Unbounded_String;
      Event_String : in out Unbounded_String);

    procedure Get_Task_Id_From_XML (
      XML_String     : in     Unbounded_String;
      Event_String   : in     Unbounded_String;
      Task_Id_String : in out Unbounded_String);

    procedure Get_Resource_Id_From_XML (
      XML_String         : in     Unbounded_String;
      Event_String       : in     Unbounded_String;
      Resource_Id_String : in out Unbounded_String);

    function Get_Resource_Name (
    	Resource_Name : in Unbounded_String)
    	return             Unbounded_String;

end Marzhin_Utils;