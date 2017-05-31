with unbounded_strings;             use unbounded_strings;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;
with Scheduler;                     use Scheduler;

---------------------------------------------------------------------
-- This package is used to provide a translation of Cheddar events
-- format to Marzhin events format since cheddar is currently for
-- test and study purpose replacing Marzhin in AADLInspector,
-- avoiding modifications on AADLInspector side.
---------------------------------------------------------------------
package Marzhin_Utils is

  ---------------------------------------------------------------------
  -- This type can be used in order to simulate the require resource 
  -- event from Marzhin.
  ---------------------------------------------------------------------
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

  ---------------------------------------------------------------------
  -- Purpose: Add a marzhin port event to the port list.
  ---------------------------------------------------------------------
  procedure Add_Port (
    Task_Name     : in Unbounded_String;
    Resource_Name : in Unbounded_String;
    State_Value   : in Unbounded_String);

  function Encode_Task_Capacities (
    Si : Scheduling_Information)
  return Unbounded_String;
  
  ---------------------------------------------------------------------
  -- Get_Event_String_From_XML
  -- Purpose: Extract the cheddar event name from an xml formatted string.
  ---------------------------------------------------------------------
  procedure Get_Event_String_From_XML (
    XML_String   : in     Unbounded_String;
    Event_String : in out Unbounded_String);

  ---------------------------------------------------------------------
  -- Get_Resource_Id_From_XML
  -- Purpose: Extract the cheddar id from a resource from an xml
  --   formatted string.
  ---------------------------------------------------------------------
  procedure Get_Resource_Id_From_XML (
    XML_String         : in     Unbounded_String;
    Event_String       : in     Unbounded_String;
    Resource_Id_String : in out Unbounded_String);

  ---------------------------------------------------------------------
  -- Get_Task_Id_From_XML
  -- Purpose: Extract the cheddar id from a task from an xml
  --   formatted string.
  ---------------------------------------------------------------------
  procedure Get_Task_Id_From_XML (
    XML_String     : in     Unbounded_String;
    Event_String   : in     Unbounded_String;
    Task_Id_String : in out Unbounded_String);

  ---------------------------------------------------------------------
  -- Is_Resource_Event
  -- Purpose: Check if the event concern's a resource.
  ---------------------------------------------------------------------
  function Is_Resource_Event (Event_String : in Unbounded_String)
  return Boolean;

  ---------------------------------------------------------------------
  -- Set_To_Marzhin_Event
  -- Purpose: Transform a cheddar event name to a marzhin event name.
  ---------------------------------------------------------------------
  procedure Set_To_Marzhin_Event (Event_String : in out Unbounded_String);

  ---------------------------------------------------------------------
  -- Set_To_Marzhin_Resource_Event
  -- Purpose: Transform a resource event to a marzhin lock/unlock 
  -- event.
  ---------------------------------------------------------------------
  procedure Set_To_Marzhin_Resource_Event (Event_String : in out Unbounded_String);

  ---------------------------------------------------------------------
  -- To_Marzhin_Output_Format
  -- Purpose: Since we are replacing Marzhin simulator in AADLInspector
  --   and trying to reduce the code modifications on the AADLInspector
  --   side, the message provided to AADLInspector will have the same
  --   format protocol that the Marzhin output.
  ---------------------------------------------------------------------
  function To_Marzhin_Output_Format (
    Tick  : in Natural;
    Event : in Unbounded_String;
    Data  : in Unbounded_String)
    return     Unbounded_String;    

end Marzhin_Utils;