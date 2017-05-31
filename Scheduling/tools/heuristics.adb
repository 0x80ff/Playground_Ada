with sets;
with Task_Set; use Task_Set;

use type Task_Set.Tasks_Range;

--#[debug]
with Debug; use Debug;


---------------------------------------------------------------------
-- Package Heuristics
-- Purpose: Contrains types definitions, functions and procedures
--          usefull for heuristics manipulations.
-- Extra: # Documentations on methods are puts in the specification file.
---------------------------------------------------------------------
package body Heuristics is

  --------------------------
  -- Heuristic_1::Can_Run --
  --------------------------

  function Can_Run(This : in out Heuristic_1; Si : in Scheduling_Information) return Boolean is
  begin
    Put_Debug("__DEBUG__ :: Can_Run -> Heuristic_1 [Not Implemented]");
    return True;
  end Can_Run;

  --------------------------
  -- Heuristic_2::Can_Run --
  --------------------------

  function Can_Run (This : in out Heuristic_2; Si : in Scheduling_Information) return Boolean is
    Number_Of_Tasks_Ended : Tasks_Range := 0;
    Task_Still_Active     : Tasks_Range := 0;
    Safe_Units            : constant Natural := 2;
  begin
    for num_task in 0 .. Si.Number_Of_Tasks - 1 loop
      if Si.Tcbs(num_task).Wake_Up_Time > This.Current_Time and
         Si.Tcbs(num_task).Rest_Of_Capacity = Si.Tcbs(num_task).Tsk.capacity
      then
        Number_Of_Tasks_Ended := Number_Of_Tasks_Ended + 1;
      else
        Task_Still_Active := num_task;
      end if;
    end loop;

    if Number_Of_Tasks_Ended = Si.Number_Of_Tasks - 1 then
      if Si.Tcbs(Task_Still_Active).Rest_Of_Capacity > 
         Si.Tcbs(Task_Still_Active).Tsk.capacity - Safe_Units
      then
        This.Active_Task_Safety_Unit := Si.Tcbs(Task_Still_Active).Tsk.capacity - Safe_Units;
      end if;

      if Si.Tcbs(Task_Still_Active).Wake_Up_Time <= This.Current_Time and
         Si.Tcbs(Task_Still_Active).Rest_Of_Capacity <= This.Active_Task_Safety_Unit
      then
        return True;
      end if;
    end if;
    return False;
  end Can_Run;

  ------------------------------------
  -- Heuristic_1::Update_Idle_Times --
  ------------------------------------

  procedure Update_Idle_Times (This : in out Heuristic_1; No_Task : in Boolean; Core_Id : in Natural) is
  begin
    if No_Task then
      Increase_Idle_Times (This, Core_Id);
    else
      Decrease_Idle_Times (This, Core_Id);
    end if;
  end Update_Idle_Times;

  -----------------------------------
  -- Heuristic_1::Reset_Idle_Times --
  -----------------------------------

  procedure Reset_Idle_Times (This : in out Heuristic_1; Core_Id : in Natural) is
  begin
    This.CNTI(Natural(core_id)) := 0;
    This.Number_Of_Valid_Idle_Times := 0;
  end Reset_Idle_Times;

  --------------------------------------
  -- Heuristic_1::Decrease_Idle_Times --
  --------------------------------------

  procedure Decrease_Idle_Times (This : in out Heuristic_1; Core_Id : in Natural) is
  begin
    This.CNTI(Natural(core_id)) := 0;
    This.Number_Of_Valid_Idle_Times := 
      (if This.Number_Of_Valid_Idle_Times = 0 then 0 else This.Number_Of_Valid_Idle_Times - 1);
  end Decrease_Idle_Times;

  --------------------------------------
  -- Heuristic_1::Increase_Idle_Times --
  --------------------------------------

  procedure Increase_Idle_Times (This : in out Heuristic_1; Core_Id : in Natural) is
  begin
    This.CNTI(Natural(core_id)) := This.CNTI(Natural(core_id)) + 1;
    if This.CNTI(Natural(core_id)) = 2 then
      This.Number_Of_Valid_Idle_Times := This.Number_Of_Valid_Idle_Times + 1;
    end if;
  end Increase_Idle_Times;

  ----------------------------
  -- Heuristic_1::Calculate --
  ----------------------------

  function Calculate (This : in out Heuristic_1; Si : in Scheduling_Information) return Natural is
      Nearest_Wake_Up_Time : Natural := Natural'Last;
  begin
    Put_Debug("__INFO__ :: Number_Of_Valid_Idle_Times -> TRUE");
    for num_task in 1 .. Si.Number_Of_Tasks - 1 loop
      if Si.Tcbs(num_task).Wake_Up_Time < Nearest_Wake_Up_Time then
        Nearest_Wake_Up_Time := Si.Tcbs(num_task).Wake_Up_Time;
      end if;
    end loop;
    Put_Debug("__INFO__ :: Nearest_Wake_Up_Time -> " & Nearest_Wake_Up_Time'Img);
    Return Nearest_Wake_Up_Time;
  end Calculate;

  ----------------------------
  -- Heuristic_2::Calculate --
  ----------------------------

  function Calculate (This : in out Heuristic_2; Si : in Scheduling_Information) return Natural is
  begin
    Put_Debug("=== IN THE VOID :: " & This.Name);
    return 1000000;
  end Calculate;

  -- procedure TestCalculation(Heuristic : Concrete_Heuristics'Class) is
  -- begin
  --     Put_Debug( "Testing an heuristic" );
  --     Put_Debug( Calculate(Heuristic)'Img' );
  -- end TestCalculation;

  -----------------------
  -- Can_Run_Heuristic --
  -----------------------

  function Can_Run_Heuristic (
    Heuristic : in out Concrete_Heuristics'Class;
    Si        : in     Scheduling_Information)
  return Boolean is
  begin
    return Can_Run(Heuristic, Si);
  end Can_Run_Heuristic;

  -------------------------
  -- Calculate_Heuristic --
  -------------------------

  function Calculate_Heuristic (
    Heuristic : in out Concrete_Heuristics'Class;
    Si        : in     Scheduling_Information)
  return Natural is
  begin
    return Calculate(Heuristic, Si);
  end Calculate_Heuristic;
end Heuristics;
