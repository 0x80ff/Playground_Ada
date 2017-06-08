with Ada.Containers.Doubly_Linked_Lists;

with Task_Set;              use Task_Set;
with Scheduler;             use Scheduler;
with Ada.Text_IO;           use Ada.Text_IO;
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;


---------------------------------------------------------------------
-- Package Heuristics
-- Purpose: Contains the components that allows the modelisation 
-- of the heuristics use to jump in time.
---------------------------------------------------------------------
package Heuristics is


---------------------- HEURISTIC TYPE DEFINITION -------------------------------
  type Heuristic is interface;
  
  -- Abstract functions and procedures:
  function Can_Run(This : in out Heuristic; Si : in Scheduling_Information) return Boolean is abstract;
  function Calculate(This : in out Heuristic; Si : in Scheduling_Information) return Natural is abstract;

  -- Abstract type of an Heuristic:
  type Concrete_Heuristics is abstract new Heuristics.Heuristic
    with record
      Name : Unbounded_String;
    end record;

  type Heuristic_Ptr is access Concrete_Heuristics'Class;


---------------------- TYPES OF HEURISTICS -------------------------------------
  type Core_No_Task_Info is array (0 .. 255) of Natural;
  type Heuristic_1 is new Concrete_Heuristics
    with record
      CNTI : Core_No_Task_Info := (others => 0);
      Number_Of_Valid_Idle_Times : Natural := 0;
      Current_Time : Natural := 0;
    end record;

  type Heuristic_2 is new Concrete_Heuristics
    with record
      Commit_Unit : Natural := 0;
      Current_Time : Natural := 0;
      Remaining_Task_Id : Tasks_Range := 0;
      Number_Of_Tasks_Ended : Natural := 0;
    end record;


---------------------- METHODS -------------------------------------------------
  -- Heuristics functions and procedures:
  function Can_Run (This : in out Heuristic_1; Si : in Scheduling_Information) return Boolean;
  function Can_Run (This : in out Heuristic_2; Si : in Scheduling_Information) return Boolean;

  function Calculate (This : in out Heuristic_1; Si : in Scheduling_Information) return Natural;
  function Calculate (This : in out Heuristic_2; Si : in Scheduling_Information) return Natural;

  -- Proper to Heuristic_1::Idle_Heuristic:
  procedure Update_Values   (This : in out Heuristic_1; No_Task : in Boolean; Core_Id : in Natural);
  procedure Reset_Values    (This : in out Heuristic_1; Core_Id : in Natural);
  --
  procedure Decrease_Idle_Times (This : in out Heuristic_1; Core_Id : in Natural);
  procedure Increase_Idle_Times (This : in out Heuristic_1; Core_Id : in Natural);


  -- Proper to Heuristic_2::Last_Job_Remaining_Heuristic:
  function  Verify_Pre_Conditions   (This : in out Heuristic_2; Si : in Scheduling_Information) return Boolean;
  procedure Reset_Values            (This : in out Heuristic_2; Si : in Scheduling_Information);
  --
  function Get_Remaining_Task       (This : in out Heuristic_2; Si : in Scheduling_Information; Current_Time : in Natural) return Boolean;
  function Get_Nearest_Wake_Up_Time (This : in out Heuristic_2; Si : in Scheduling_Information) return Natural;
---------------------- HEURISTICS LIST -----------------------------------------
  -- Heuristic list:
  package Heuristic_Lists is new Ada.Containers.Doubly_Linked_Lists (Heuristic_Ptr);
  use Heuristic_Lists;
  Heuristic_List : List;


---------------------- INTERFACE METHODS ---------------------------------------
  -- procedure TestCalculation(Heuristic : Concrete_Heuristics'Class);

  -- Interface methods:
  function Can_Run_Heuristic (
    Heuristic : in out Concrete_Heuristics'Class;
     Si       : in     Scheduling_Information)
  return Boolean;

  function Calculate_Heuristic (
    Heuristic : in out Concrete_Heuristics'Class;
    Si        : in Scheduling_Information)
  return Natural;


---------------------- HEURISTICS DECLARATIONS ---------------------------------
  -- Heuristics declarations:
  Idle_Heuristic     : Heuristic_Ptr := new Heuristic_1;
  Last_Job_Heuristic : Heuristic_Ptr := new Heuristic_2;
end Heuristics;
