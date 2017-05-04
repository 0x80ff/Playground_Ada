with Debug;    use Debug;
with Task_Set; use Task_Set;
with sets;

use type Task_Set.Tasks_Range;

package body Heuristics is
  function Calculate(Self : Heuristic_1; Si : in Scheduling_Information) return Natural is
      Nearest_Wake_Up_Time : Natural;
      Number_Of_Tasks : Tasks_Range := Si.Number_Of_Tasks - 1;
  begin
    Nearest_Wake_Up_Time := Natural'Last;
    for num_task in 1 .. Number_Of_Tasks loop
      if Si.Tcbs(num_task).Wake_Up_Time < Nearest_Wake_Up_Time then
        Nearest_Wake_Up_Time := Si.Tcbs(num_task).Wake_Up_Time;
      end if;
    end loop;

    return Nearest_Wake_Up_Time;
  end Calculate;

  function Calculate(Self : Heuristic_2; Si : in Scheduling_Information) return Natural is
  begin
    Put_Debug("=== IN THE VOID :: " & Self.Name);
    return 1000000;
  end Calculate;

  -- procedure TestCalculation(Heuristic : Concrete_Heuristics'Class) is
  -- begin
  --     Put_Debug( "Testing an heuristic" );
  --     Put_Debug( Calculate(Heuristic)'Img' );
  -- end TestCalculation;

  function Calculate_Heuristic (
    Heuristic : in Concrete_Heuristics'Class;
    Si        : in Scheduling_Information)
  return Natural is
  begin
    return Calculate(Heuristic, Si);
  end Calculate_Heuristic;
end Heuristics;
