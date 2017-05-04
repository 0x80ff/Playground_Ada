with Ada.Containers.Doubly_Linked_Lists;
with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with Ada.Text_IO;                   use Ada.Text_IO;
with Scheduler;                     use Scheduler;
with Task_Set; use Task_Set;

package Heuristics is
  type Heuristic is interface;
  function Calculate(Self : Heuristic; Si : Scheduling_Information) return Natural is abstract;

  type Concrete_Heuristics is abstract new Heuristics.Heuristic
    with record
      Name : Unbounded_String;
    end record;

  type Heuristic_Ptr is access Concrete_Heuristics'Class;

  type Heuristic_1 is new Concrete_Heuristics with null record;
  type Heuristic_2 is new Concrete_Heuristics with null record;

  function Calculate(Self : Heuristic_1; Si : in Scheduling_Information) return Natural;
  function Calculate(Self : Heuristic_2; Si : in Scheduling_Information) return Natural;

  package Heuristic_Lists is new Ada.Containers.Doubly_Linked_Lists (Heuristic_Ptr);
  use Heuristic_Lists;

  Heuristic_List : List;

  -- procedure TestCalculation(Heuristic : Concrete_Heuristics'Class);

  function Calculate_Heuristic (
    Heuristic : in Concrete_Heuristics'Class;
    Si        : in Scheduling_Information)
  return Natural;

end Heuristics;
