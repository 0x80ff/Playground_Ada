with unbounded_strings;             use unbounded_strings;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Strings.Unbounded.Text_IO; use Ada.Strings.Unbounded.Text_IO;

package body Marzhin_Utils is
    
  --------------
  -- Add_Port --
  --------------

  procedure Add_Port (
    Task_Name     : in Unbounded_String;
    Resource_Name : in Unbounded_String;
    State_Value   : in Unbounded_String)
  is
  begin
    Ports.Port_Array(Ports.Port_Nb).Task_Data     := Task_Name;
    Ports.Port_Array(Ports.Port_Nb).Resource_Data := Resource_Name;
    Ports.Port_Array(Ports.Port_Nb).State_Data    := State_Value;
    Ports.Port_Nb := Ports.Port_Nb + 1;
  end Add_Port;

  -------------------------------
  -- Get_Event_String_From_XML --
  -------------------------------

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

  ------------------------------
  -- Get_Resource_Id_From_XML --
  ------------------------------

  procedure Get_Resource_Id_From_XML (
    XML_String         : in     Unbounded_String;
    Event_String       : in     Unbounded_String;
    Resource_Id_String : in out Unbounded_String)
  is
    XML_Start_Tag          : Unbounded_String := To_Unbounded_String("ref=""");
    XML_End_Tag            : constant  String := """ />";
    XML_Start_Tag_Position : Natural;
    XML_End_Tag_Position   : Natural;
  begin
    if Event_String = To_Unbounded_String("WAIT_FOR_RESOURCE") then
      XML_Start_Tag := To_Unbounded_String("<wait_for_resource ref=""");
    elsif Event_String = To_Unbounded_String("ALLOCATE_RESOURCE") then
      XML_Start_Tag := To_Unbounded_String("<allocate_resource ref=""");
    elsif Event_String = To_Unbounded_String("RELEASE_RESOURCE") then
      XML_Start_Tag := To_Unbounded_String("<release_resource ref=""");
    end if;

    XML_Start_Tag_Position := Index (XML_String, To_String(XML_Start_Tag)) + To_String(XML_Start_Tag)'Length;
    XML_End_Tag_Position   := Index (XML_String, XML_End_Tag, Ada.Strings.Backward) - 1;

    Resource_Id_String := Substring (XML_String, XML_Start_Tag_Position, XML_End_Tag_Position);
  end Get_Resource_Id_From_XML;

  --------------------------
  -- Get_Task_Id_From_XML --
  --------------------------

  procedure Get_Task_Id_From_XML (
    XML_String     : in     Unbounded_String;
    Event_String   : in     Unbounded_String;
    Task_Id_String : in out Unbounded_String)
  is
    XML_Start_Tag          : Unbounded_String := To_Unbounded_String("ref=""");
    XML_End_Tag            : constant  String := """ />";
    XML_Start_Tag_Position : Natural;
    XML_End_Tag_Position   : Natural;
  begin
    if Event_String = To_Unbounded_String("PREEMPTION") then
      XML_Start_Tag := To_Unbounded_String("<preempted_task ref=""");
    elsif Event_String = To_Unbounded_String("WAIT_FOR_RESOURCE") then
      XML_Start_Tag := To_Unbounded_String("<wait_for_resource_task ref=""");
    elsif Event_String = To_Unbounded_String("ALLOCATE_RESOURCE") then
      XML_Start_Tag := To_Unbounded_String("<allocate_task ref=""");
    elsif Event_String = To_Unbounded_String("RELEASE_RESOURCE") then
      XML_Start_Tag := To_Unbounded_String("<release_task ref=""");
    end if;

    XML_Start_Tag_Position := Index (XML_String, To_String(XML_Start_Tag)) + To_String(XML_Start_Tag)'Length;
    XML_End_Tag_Position   := Index (XML_String, XML_End_Tag) - 1;

    Task_Id_String := Substring (XML_String, XML_Start_Tag_Position, XML_End_Tag_Position);
  end Get_Task_Id_From_XML;

  --------------------------
  -- Set_To_Marzhin_Event --
  --------------------------

  procedure Set_To_Marzhin_Event (Event_String : in out Unbounded_String) is
  begin
    if      Event_String = To_Unbounded_String("RUNNING_TASK")           then Event_String := To_Unbounded_String("THREAD_STATE_RUNNING");
      elsif Event_String = To_Unbounded_String("TASK_ACTIVATION")        then Event_String := To_Unbounded_String("THREAD_STATE_READY");
      elsif Event_String = To_Unbounded_String("PREEMPTION")             then Event_String := To_Unbounded_String("THREAD_STATE_SUSPENDED");
      elsif Event_String = To_Unbounded_String("START_OF_TASK_CAPACITY") then Event_String := To_Unbounded_String("THREAD_DISPATCH");
      elsif Event_String = To_Unbounded_String("END_OF_TASK_CAPACITY")   then Event_String := To_Unbounded_String("THREAD_STATE_SUSPENDED");
      elsif Event_String = To_Unbounded_String("WAIT_FOR_RESOURCE")      then Event_String := To_Unbounded_String("THREAD_STATE_AWAITING_RESOURCE");
      elsif Event_String = To_Unbounded_String("ALLOCATE_RESOURCE")      then Event_String := To_Unbounded_String("THREAD_GET_RESOURCE");
      elsif Event_String = To_Unbounded_String("RELEASE_RESOURCE")       then Event_String := To_Unbounded_String("THREAD_RELEASE_RESOURCE");
    end if;
  end Set_To_Marzhin_Event;

  -----------------------------
  -- To_Marzhin_Output_Format --
  -----------------------------

  function To_Marzhin_Output_Format (
    Tick  : Natural;
    Event : Unbounded_String;
    Data  : Unbounded_String)
  return Unbounded_String is
    Return_String : Unbounded_String;
  begin
    Return_String := To_Unbounded_String (Tick'Img);
    Return_String := Trim(Return_String, Ada.Strings.Left);
    Return_String := "tick=" & Return_String & " " & Event & " " & "root." & Data;
    return Return_String;
  end To_Marzhin_Output_Format;

end Marzhin_Utils;