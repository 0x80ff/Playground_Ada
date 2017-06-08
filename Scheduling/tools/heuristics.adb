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
  begin
    Put_Debug("__DEBUG__ :: Can_Run -> Heuristic_2 [Not Implemented]");
    return True;
  end Can_Run;

  -------------------------
  -- Heuristic_1::Update --
  -------------------------

  procedure Update_Values (This : in out Heuristic_1; No_Task : in Boolean; Core_Id : in Natural) is
  begin
    if No_Task then
      Increase_Idle_Times (This, Core_Id);
    else
      Decrease_Idle_Times (This, Core_Id);
    end if;
  end Update_Values;

  -------------------------------
  -- Heuristic_1::Reset_Values --
  -------------------------------

  procedure Reset_Values (This : in out Heuristic_1; Core_Id : in Natural) is
  begin
    This.CNTI(Natural(core_id)) := 0;
    This.Number_Of_Valid_Idle_Times := 0;
  end Reset_Values;

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
    Jump_Time : Natural;
    Nearest_Wake_Up_Time : Natural;
  begin
    Jump_Time := This.Current_Time + Si.Tcbs(This.Remaining_Task_Id).Rest_Of_Capacity;
    Nearest_Wake_Up_Time := Get_Nearest_Wake_Up_Time(This, Si);

    Si.Tcbs(This.Remaining_Task_Id).Rest_Of_Capacity := Jump_Time - Nearest_Wake_Up_Time + 1;
    Si.Tcbs(This.Remaining_Task_Id).Used_Capacity := Si.Tcbs(This.Remaining_Task_Id).Tsk.capacity - (Jump_Time - Nearest_Wake_Up_Time) - 1;

    if Nearest_Wake_Up_Time < Jump_Time then
      Jump_Time := Nearest_Wake_Up_Time;
    end if;

    return Jump_Time;
  end Calculate;

  -- procedure TestCalculation(Heuristic : Concrete_Heuristics'Class) is
  -- begin
  --     Put_Debug( "Testing an heuristic" );
  --     Put_Debug( Calculate(Heuristic)'Img' );
  -- end TestCalculation;

  ----------------------------------------
  -- Heuristic_2::Verify_Pre_Conditions --
  ----------------------------------------

  function Verify_Pre_Conditions (
    This              : in out Heuristic_2;
    Si                : in     Scheduling_Information)
  return Boolean is
  begin
    -- Test number of remaining jobs:
    if not Get_Remaining_Task(This, Si, This.Current_Time) then
      return False;
    end if;

    if (Si.Tcbs(This.Remaining_Task_Id).Rest_Of_Capacity > Si.Tcbs(This.Remaining_Task_Id).Tsk.capacity - 2) then
      This.Commit_Unit := Si.Tcbs(This.Remaining_Task_Id).Tsk.capacity - 2;
    end if;

    -- Test the capacity of the remaining job:
    if Si.Tcbs(This.Remaining_Task_Id).Tsk.capacity <= 2 then
      return False;
    end if;

    -- Test the starting event commit:
    if Si.Tcbs(This.Remaining_Task_Id).Wake_Up_Time > This.Current_Time
    or Si.Tcbs(This.Remaining_Task_Id).Rest_Of_Capacity >= This.Commit_Unit
    then
      return False;
    end if;

    -- Test the nearest wake up time:
    if (Get_Nearest_Wake_Up_Time(This, Si) - This.Current_Time) <= 2
    then
      return False;
    end if;

    -- Test the jump time:
    if ((This.Current_Time + Si.Tcbs(This.Remaining_Task_Id).Rest_Of_Capacity) - This.Current_Time) <= 2 then
      return False;
    end if;

    return True;
  end Verify_Pre_Conditions;

  -------------------------------------
  -- Heuristic_2::Get_Remaining_Task --
  -------------------------------------

  function Get_Remaining_Task (
    This         : in out Heuristic_2;
    Si           : in     Scheduling_Information;
    Current_Time : in     Natural)
  return Boolean is
  begin
    This.Number_Of_Tasks_Ended := 0;
    for num_task in 0 .. Si.Number_Of_Tasks - 1 loop
      if (Si.Tcbs(num_task).Wake_Up_Time > Current_Time) and 
         (Si.Tcbs(num_task).Rest_Of_Capacity = Si.Tcbs(num_task).Tsk.capacity)
      then
        This.Number_Of_Tasks_Ended := This.Number_Of_Tasks_Ended + 1;
      else
        This.Remaining_Task_Id := num_task;
      end if;
    end loop;

    if This.Number_Of_Tasks_Ended = Natural(Si.Number_Of_Tasks) - 1 then
      return True;
    end if;

    return False;
  end Get_Remaining_Task;

  -------------------------------------------
  -- Heuristic_2::Get_Nearest_Wake_Up_Time --
  -------------------------------------------

  function Get_Nearest_Wake_Up_Time (
    This              : in out Heuristic_2;
    Si                : in Scheduling_Information)
  return Natural is
    Nearest_Wake_Up_Time : Natural := This.Current_Time + Si.Tcbs(This.Remaining_Task_Id).Rest_Of_Capacity;
  begin
    for num_task in 0 .. Si.Number_Of_Tasks - 1 loop
      if Si.Tcbs(num_task).Tsk.name /= Si.Tcbs(This.Remaining_Task_Id).Tsk.name then
        if Si.Tcbs(num_task).Wake_Up_Time < Nearest_Wake_Up_Time then
            Nearest_Wake_Up_Time := Si.Tcbs(num_task).Wake_Up_Time;
        end if;
      end if;
    end loop;

    return Nearest_Wake_Up_Time;
  end Get_Nearest_Wake_Up_Time;

  -------------------------------
  -- Heuristic_2::Reset_Values --
  -------------------------------

  procedure Reset_Values (This : in out Heuristic_2; Si : in Scheduling_Information) is
  begin
    if (Si.Tcbs(This.Remaining_Task_Id).Rest_Of_Capacity > 1) then
      This.Commit_Unit := Si.Tcbs(This.Remaining_Task_Id).Rest_Of_Capacity - 2;
    else
      This.Commit_Unit := 0;
      Put_Debug("INFO SAFETY PUT TO ZERO");
    end if;

    Heuristic_2(Last_Job_Heuristic.all).Remaining_Task_Id     := 0;
    Heuristic_2(Last_Job_Heuristic.all).Number_Of_Tasks_Ended := 0;
  end Reset_Values;

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
