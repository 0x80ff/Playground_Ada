with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Text_IO;                   use Ada.Text_IO;
with Scheduler;                     use Scheduler;
with Task_Set; use Task_Set;

---------------------------------------------------------------------
-- Package Heuristics
-- Purpose: Contains the components that allows the modelisation 
-- of the heuristics.
---------------------------------------------------------------------
package Heuristics is
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

  type Core_No_Task_Info is array (0 .. 255) of Natural;

  -- Heuristics types defined:
  type Heuristic_1 is new Concrete_Heuristics
    with record
      CNTI : Core_No_Task_Info := (others => 0);
      Number_Of_Valid_Idle_Times : Natural := 0;
    end record;

  type Heuristic_2 is new Concrete_Heuristics
    with record
      Task_Id_Still_Active    : Tasks_Range;
      Active_Task_Safety_Unit : Natural;
      Current_Time : Natural;
    end record;

  -- Heuristics functions and procedures:
  function Can_Run(This : in out Heuristic_1; Si : in Scheduling_Information) return Boolean;
  function Can_Run(This : in out Heuristic_2; Si : in Scheduling_Information) return Boolean;

  function Calculate(This : in out Heuristic_1; Si : in Scheduling_Information) return Natural;
  function Calculate(This : in out Heuristic_2; Si : in Scheduling_Information) return Natural;


  -- Idle_Heuristic
  procedure Update_Idle_Times   (This : in out Heuristic_1; No_Task : in Boolean; Core_Id : in Natural);
  procedure Reset_Idle_Times    (This : in out Heuristic_1; Core_Id : in Natural);
  procedure Decrease_Idle_Times (This : in out Heuristic_1; Core_Id : in Natural);
  procedure Increase_Idle_Times (This : in out Heuristic_1; Core_Id : in Natural);

  -- Heuristic list:
  package Heuristic_Lists is new Ada.Containers.Doubly_Linked_Lists (Heuristic_Ptr);
  use Heuristic_Lists;

  Heuristic_List : List;

  -- procedure TestCalculation(Heuristic : Concrete_Heuristics'Class);

  -- Interface methods:
  function Can_Run_Heuristic (Heuristic : in out Concrete_Heuristics'Class; Si : in Scheduling_Information) return Boolean;

  function Calculate_Heuristic (
    Heuristic : in out Concrete_Heuristics'Class;
    Si        : in Scheduling_Information)
  return Natural;

  Idle_Heuristic : Heuristic_Ptr := new Heuristic_1;
  HR_2 : Heuristic_Ptr := new Heuristic_2;

end Heuristics;
