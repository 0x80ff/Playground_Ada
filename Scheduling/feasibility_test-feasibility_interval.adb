with Ada.Exceptions;                    use Ada.Exceptions;
with Resources;                         use Resources;
use Resources.Resource_accesses;
with Time_Unit_Events;                  use Time_Unit_Events;
use Time_Unit_Events.Time_Unit_Package;
with natural_util;                      use natural_util;
with double_util;                       use double_util;
with integer_util;                      use integer_util;
with Ada.Numerics.Elementary_Functions; use Ada.Numerics.Elementary_Functions;
with Objects;                           use Objects;
with Objects.extended;                  use Objects.extended;
with Translate;                         use Translate;
with Multi_precision_integers;          use Multi_precision_integers;
with Multi_precision_integers_IO;       use Multi_precision_integers_IO;
with multi_int_util;                    use multi_int_util;
with initialize_framework; 		use initialize_framework;

with Systems;                           use Systems;
with Processor_Interface;               use Processor_Interface;
with Processors;			use Processors;
with Processor_Set;			use Processor_Set;
with Task_Set;				use Task_Set;
with Ada.Text_IO;			use Ada.Text_IO;
with Core_Units;			use Core_Units;
with Task_Dependencies;			use Task_Dependencies;
with Scheduler_Interface;		use Scheduler_Interface;
with sets;

-- TODO: Controle sur type de dépendances.
-- TODO: Vérifier les types, risques de dépassements etc..
-- TODO: Créer ou ajouter dans un package approprié les fonctions utilitaires
-- TODO: ajouter les autres algorithmes d'ordonnancement concernés

-- Erreur sur scénario 5_1 Array Too Small


------------------------------------------------------------------------
-- Autolayout
-- Implementation Notes:
-- - This package implements multiples algorithms in order to provide a
-- feasibility interval for a given system as well as a validation of
-- the calculation result. It uses a package multi_precision_integers in a
-- way to manipulate big integers during calculation without loss of precision.

-- Issues:
-- - The task set 5.1 raise the following exception:
-- MULTI_PRECISION_INTEGERS.ARRAY_TOO_SMALL
-- - The use of the big numbers package may not be relevant to the
-- current implementation (cf: Find a better way of use)
-- - Coordinate_Type below could be changed from integer to float

-- Anticipated Changes:
-- - Add precise tests on dependencies as well as preemptivity
-- - Add verifications/exceptions on range violations etc.
-- - Move utility functions in their proper packages
--   (cf: Max_Start_Time_Of_TaskSet in task_set package)
-- - Add others algorithms to proper Is_XX_Scheduler functions.
---------------------------------------------------------
------------------------------------------------------------------------
package body feasibility_test.feasibility_interval is

   ---------------------------------------------------------------------
   -- Is_Fixed_Job_Scheduler
   -- Implementation Notes:
   -- -
   ---------------------------------------------------------------------
   function Is_Fixed_Job_Scheduler ( Scheduler_Type : in Schedulers_type )
   return Boolean
   is
   begin
      if Scheduler_Type = Earliest_Deadline_First_Protocol then
         return True;
      else
         return False;
      end if;
   end Is_Fixed_Job_Scheduler;

   ---------------------------------------------------------------------
   -- Is_Fixed_Task_Scheduler
   -- Implementation Notes:
   -- -
   ---------------------------------------------------------------------
   function Is_Fixed_Task_Scheduler ( Scheduler_Type : in Schedulers_type )
   return Boolean
   is
   begin
     case Scheduler_Type is
        when Rate_Monotonic_Protocol                    => return True;
        when Deadline_Monotonic_Protocol                => return True;
        when Posix_1003_Highest_Priority_First_Protocol => return True;
        when others                                     => return False;
     end case;
   end Is_Fixed_Task_Scheduler;

   ---------------------------------------------------------------------
   -- Is_Work_Conserving_Scheduler
   -- Implementation Notes:
   -- -
   ---------------------------------------------------------------------
   function Is_Work_Conserving_Scheduler ( Scheduler_Type : in Schedulers_type )
   return Boolean
   is
   begin
      if Is_Fixed_Job_Scheduler  ( Scheduler_Type ) or else
         Is_Fixed_Task_Scheduler ( Scheduler_Type )
      then
         return True;
      else
         return False;
      end if;
   end Is_Work_Conserving_Scheduler;

   ---------------------------------------------------------------------
   -- Max_Start_Time_Of_TaskSet
   -- Implementation Notes:
   -- -
   ---------------------------------------------------------------------
   function Max_Start_Time_Of_TaskSet ( My_Tasks : in Tasks_Set )
   return Natural
   is
      Max_Start_Time  : Natural := 0;
      My_Iterator     : Tasks_Iterator;
      A_Task 	      : Generic_Task_Ptr;
   begin
      reset_iterator ( My_Tasks, My_Iterator );
      loop
         current_element ( My_Tasks, A_Task, My_Iterator );
         Max_Start_Time := Natural'Max ( Max_Start_Time, A_Task.start_time );

         exit when is_last_element ( My_Tasks, My_Iterator );

         next_element ( My_Tasks, My_Iterator );
      end loop;

      return Max_Start_Time;
   end Max_Start_Time_Of_TaskSet;

   ---------------------------------------------------------------------
   -- Calculate_Feasibility_Interval_Top
   -- Implementation Notes:
   -- - Calls the Calculate_Feasibility_Interval function and convert
   --   the interval into Double type. Use in order to have 2 choices
   --   for the user.
   ---------------------------------------------------------------------
   procedure Calculate_Feasibility_Interval_Top ( Sys	   : in     System;
                                                  Validate :    out Boolean;
                                                  Interval :    out Double )
   is
       Local_Interval : multi_int_ptr;
   begin
      Calculate_Feasibility_Interval ( Sys, Validate, Local_Interval );
      Interval := convert2double ( Local_Interval.all );
   end Calculate_Feasibility_Interval_Top;

   ---------------------------------------------------------------------
   -- Calculate_Feasibility_Interval
   -- Implementation Notes:
   -- - This routine calls the function needed for the feasibility
   --   interval calculation based on the characteristics of the provided
   --   system. A boolean was added in order to know if the procedure
   --   finded a corresponding algorithms for the calculation or not and
   --   so if the interval is valid for the user or not.
   ---------------------------------------------------------------------
   procedure Calculate_Feasibility_Interval ( Sys      : in     System;
                                              Validate :    out Boolean;
                                              Interval :    out multi_int_ptr )
   is
      Independent_Tasks     : Boolean;
      Constrained_Deadlines : Boolean;
      Processor_Type        : Processors_type;
      Scheduler_Type        : Schedulers_Type;
      A_Core                : Core_Unit_Ptr;
      Core_Iterator         : core_units_Iterator;
      A_Processor           : Generic_Processor_Ptr;
   begin
      Independent_Tasks    := Is_Empty ( Sys.Resources ) and
                              Is_Empty ( Sys.Dependencies.Depends );
      Constrained_Deadlines := Deadline_Inferior_To_Period ( Sys.Tasks );

      -- Get Processor Type
      get_element_number ( Sys.Processors, A_Processor, Processors_Range (0) );
      Processor_Type := A_Processor.processor_type;

      -- Get Scheduler Type
      reset_iterator ( Sys.Core_units, Core_Iterator );

      loop
         current_element ( Sys.Core_units, A_Core, Core_Iterator );
         exit when is_last_element (Sys.Core_units, Core_Iterator);
         next_element ( Sys.Core_units, Core_Iterator );
      end loop;

      Validate       := True;
      Scheduler_Type := A_Core.scheduling.scheduler_type;

      -- Scheduling Interval Dispatch
      case Processor_Type is
         when Monocore_type =>
            if Constrained_Deadlines then
               if Independent_Tasks then
                  if Is_Fixed_Task_Scheduler(Scheduler_Type) then
                     Put_Line("Leung and Merill (1980)");
                     Interval := new multi_int'(Scheduling_Period_With_Offset(Sys.Tasks, A_Processor.name));
                     return;
                  end if;
                  if Is_Fixed_Job_Scheduler(Scheduler_Type) then
                     Put_Line("Goossens and Devillers (1997)");
                     Interval := new multi_int'(Scheduling_Period_1997(Sys.Tasks, A_Processor.name, False));
                     return;
		  end if;
               else
                  if Is_Work_Conserving_Scheduler(Scheduler_Type) then
                     Put_Line("Choquet-Geniet and Grolleau (2004), Bado et al. (2012)");
                     Interval := new multi_int'(Scheduling_Period_With_Offset(Sys.Tasks, A_Processor.name));
                     return;
                  end if;
               end if;
            else
               if Is_Fixed_Job_Scheduler(Scheduler_Type) and
                  Independent_Tasks
               then
                  Put_Line("Goossens and Devillers (1999)");
                  Interval := new multi_int'(Scheduling_Period_With_Offset(Sys.Tasks, A_Processor.name));
                  return;
               end if;
            end if;
         when Uniform_Multicores_Type =>
            Put_Line("Uniform");
            if Independent_Tasks     and
               Constrained_Deadlines and
               Is_Fixed_Task_Scheduler(Scheduler_Type)
            then
               Put_Line("Cucu and Goossens (2006)");
               Interval := new multi_int'(Scheduling_Period_1997(Sys.Tasks, A_Processor.name, False));
               return;
            end if;
         when Unrelated_Multicores_Types =>
            Put_Line("Unrelated");
            if Independent_Tasks     and
               Constrained_Deadlines and
               Is_Fixed_Task_Scheduler(Scheduler_Type)
            then
               Put_Line("Cucu-Grosjean and Goossens (2011)");
               Interval := new multi_int'(Scheduling_Period_1997(Sys.Tasks, A_Processor.name, False));
               return;
            end if;
         when Identical_Multicores_Type =>
            Put_Line("Identical");
            if Constrained_Deadlines then
               Put_Line("Baro et al. (2012), Nélis et al. (2013)");
               Put_Line("Baro et al. (2012)");
               Interval := new multi_int'(Scheduling_Period_2012_2013(Sys.Tasks, A_Processor.name));
               return;
            else
               if Independent_Tasks and
                  Is_Fixed_Task_Scheduler(Scheduler_Type)
               then
                  Put_Line("Cucu and Goossens (2007)");
                  Interval := new multi_int'(Scheduling_Period_1997(Sys.Tasks, A_Processor.name, True));
                  return;
               end if;
               Put_Line("Goossens-Cucu-Grolleau (2016)");
               Interval := new multi_int'(Scheduling_Period_2016(Sys.Tasks, A_Processor.name));
               return;
            end if;
         when others =>
            Put_Line("Unknown Processors Type");
            Validate := False;
            Interval := new multi_int'(Scheduling_Period_With_Offset(Sys.Tasks, A_Processor.name));
            return;
      end case;

      -- No corresponding feasibility interval algorithm, use of classical Omax + 2H interval,
      -- Invalidated interval.
      Validate := False;
      Interval := new multi_int'(Scheduling_Period_With_Offset(Sys.Tasks, A_Processor.name));
   end Calculate_Feasibility_Interval;

   ---------------------------------------------------------------------
   -- Scheduling_Period_1997
   -- @param AddPeriod Switch between interval types
   -- Implementation Notes:
   -- -
   ---------------------------------------------------------------------
   function Scheduling_Period_1997 ( My_Tasks 	    : in Tasks_Set;
                                     Processor_Name : in Unbounded_String;
                                     AddPeriod      : in Boolean )
   return multi_int is
      H               : multi_int_ptr;
      Sn              : multi_int_ptr;
      Op1             : multi_int_ptr;
      Op2             : multi_int_ptr;
      Op3             : multi_int_ptr;
      Period          : multi_int_ptr;
      Start_Time      : multi_int_ptr;
      Tmp_Tasks       : Tasks_Set;
      My_Iterator     : Tasks_Iterator;
      A_Task          : Generic_Task_Ptr;
      First_Task_Used : Boolean := True;
   begin
      Current_Processor_Name := Processor_Name;
      H                      := new multi_int'(Multi(1));

      select_and_copy ( My_Tasks, Tmp_Tasks, Select_Cpu'Access );
      sort ( Tmp_Tasks, Decreasing_Priority'Access );

      Periodic_Control ( Tmp_Tasks, Processor_Name );

      reset_iterator ( Tmp_Tasks, My_Iterator );

      loop
         current_element (Tmp_Tasks, A_Task, My_Iterator);

         if A_Task.cpu_name  = Processor_Name and
            A_Task.task_type = Periodic_Type
         then
            Start_Time := new multi_int'( Multi(Integer(A_Task.start_time)) );
            Period     := new multi_int'( Multi(Periodic_Task_Ptr(A_Task).period) );
            H          := lcm( H, Period );

            if First_Task_Used then
               Sn := new multi_int'( Start_Time.all );
               First_Task_Used := False;
            else
               Op1 := new multi_int'( Sn.all - Start_Time.all );
               Op2 := new multi_int'( Multi(
                                        Integer(
                                          Float'Ceiling(
                                            convert ( Op1.all ) / convert(Period.all) ))));
               Op1 := new multi_int'( Op2.all * Period.all + Start_Time.all );

               if Start_Time.all > Op1.all then
                  Sn := new multi_int'( Start_Time.all );
               else
                  Sn := new multi_int'( Op1.all );
               end if;

               if AddPeriod then
                  Op3  := new multi_int'( Sn.all );
                  Sn   := new multi_int'( Op3.all + H.all );
                end if;
            end if;
         end if;

         exit when is_last_element ( Tmp_Tasks, My_Iterator );

         next_element ( Tmp_Tasks, My_Iterator );
      end loop;

      Op3 := new multi_int'( Sn.all + H.all );
      return Op3.all;
   end Scheduling_Period_1997;

   ---------------------------------------------------------------------
   -- Scheduling_Period_2012_2013
   -- Implementation Notes:
   -- -
   ---------------------------------------------------------------------
   function Scheduling_Period_2012_2013 ( My_Tasks	     : in Tasks_Set;
                                          Processor_Name     : in Unbounded_String )
   return multi_int is
      H                  : multi_int_ptr;
      Op1                : multi_int_ptr;
      Op2                : multi_int_ptr;
      Op3                : multi_int_ptr;
      Period             : multi_int_ptr;
      Capacity           : multi_int_ptr;
      Maximum_Start_Time : multi_int_ptr;
      My_Iterator        : Tasks_Iterator;
      A_Task             : Generic_Task_Ptr;
   begin
      Periodic_Control (My_Tasks, Processor_Name);

      H   := new multi_int'( Multi(1) );
      Op1 := new multi_int'( Multi(1) );
      Op2 := new multi_int'( Multi(1) );

      reset_iterator ( My_Tasks, My_Iterator );
      Maximum_Start_Time := new multi_int'( Multi (Max_Start_Time_Of_TaskSet (My_Tasks)) );

      loop
         current_element (My_Tasks, A_Task, My_Iterator);

         if A_Task.cpu_name  = Processor_Name and
            A_Task.task_type = Periodic_Type
         then
            Period   := new multi_int'( Multi(Periodic_Task_Ptr(A_Task).period) );
            Capacity := new multi_int'( Multi(A_Task.capacity) );

            H   := lcm( H, Period );
            Op3 := new multi_int'( Op1.all );
            Op1 := new multi_int'( Op3.all * ( Capacity.all + Op2.all ) );
         end if;

         exit when is_last_element (My_Tasks, My_Iterator);

         next_element (My_Tasks, My_Iterator);
      end loop;

      Op3 := new multi_int'( Maximum_Start_Time.all + H.all * Op1.all );
      return Op3.all;
   end Scheduling_Period_2012_2013;

   ---------------------------------------------------------------------
   -- Scheduling_Period_2016
   -- Implementation Notes:
   -- -
   ---------------------------------------------------------------------
   function Scheduling_Period_2016 ( My_Tasks	    : in Tasks_Set;
                                     Processor_Name : in Unbounded_String )
   return multi_int is
      H           : multi_int_ptr;
      Op1         : multi_int_ptr;
      Op2         : multi_int_ptr;
      Op3         : multi_int_ptr;
      Op4         : multi_int_ptr;
      Period      : multi_int_ptr;
      Deadline    : multi_int_ptr;
      Start_Time  : multi_int_ptr;
      My_Iterator : Tasks_Iterator;
      A_Task      : Generic_Task_Ptr;
   begin
      Periodic_Control (My_Tasks, Processor_Name);

      H   := new multi_int'( Multi(1) );
      Op1 := new multi_int'( Multi(1) );
      Op2 := new multi_int'( Multi(1) );

      reset_iterator (My_Tasks, My_Iterator);

      loop
         current_element (My_Tasks, A_Task, My_Iterator);

         if A_Task.cpu_name  = Processor_Name and
            A_Task.task_type = Periodic_Type
         then
            Period     := new multi_int'( Multi(Periodic_Task_Ptr(A_Task).period) );
            Deadline   := new multi_int'( Multi(A_Task.deadline) );
            Start_Time := new multi_int'( Multi(A_Task.start_time) );

            H := lcm( H, Period );
            Op3 := new multi_int'( Start_Time.all + Deadline.all - Period.all + Op2.all );
            Op4 := new multi_int'( Op1.all );
            Op1 := new multi_int'( Op4.all * Op3.all );
         end if;

         exit when is_last_element (My_Tasks, My_Iterator);

         next_element (My_Tasks, My_Iterator);
      end loop;

      Op4 := new multi_int'( H.all );
      H   := new multi_int'( Op1.all * Op4.all );
      return H.all;
   end Scheduling_Period_2016;

   ---------------------------------------------------------------------
   -- Scheduling_Period_With_Offset
   -- Implementation Notes:
   -- -
   ---------------------------------------------------------------------
   function Scheduling_Period_With_Offset ( My_Tasks       : in Tasks_Set;
                                            Processor_Name : Unbounded_String )
   return multi_int is
      Start_Time   : Natural := 0;
      Offset_Value : Natural := 0;
      A_Task       : Generic_Task_Ptr;
      My_Iterator  : Tasks_Iterator;
      H            : multi_int_ptr;
      Period       : multi_int_ptr;
      Max_Offset   : multi_int_ptr;
   begin
      Periodic_Control ( My_Tasks, Processor_Name );

      reset_iterator ( My_Tasks, My_Iterator );

      H := new multi_int'( Multi (1) );

      loop
         current_element ( My_Tasks, A_Task, My_Iterator );

         Offset_Value := 0;
         Start_Time   := A_Task.start_time;
         Period       := new multi_int'( Multi(Periodic_Task_Ptr (A_Task).period) );
         Max_Offset   := new multi_int'( Multi(0) );

         for I in 0 .. A_Task.offsets.nb_entries - 1 loop
            if A_Task.offsets.entries(I).activation = 0 then
               Offset_Value := A_Task.offsets.entries(I).offset_value;
               exit;
            end if;
         end loop;

         if Max_Offset.all > ( Multi(Start_Time) + Multi(Offset_Value) ) then
            Max_Offset := new multi_int'( Max_Offset.all );
         else
            Max_Offset := new multi_int'( Multi(Start_Time) + Multi(Offset_Value) );
         end if;

         H := lcm ( H, Period );

         exit when is_last_element ( My_Tasks, My_Iterator );
         next_element ( My_Tasks, My_Iterator );
      end loop;

      Period := new multi_int'( ( H.all * Multi(2) ) + Max_Offset.all );
      return Period.all;
   end Scheduling_Period_With_Offset;

--------------------------------------------------------------------------------
--------------------------------------------------------------------------------

   function Scheduling_Period
     (My_Tasks       : in Tasks_Set;
      Processor_Name : in Unbounded_String)
      return           Double
   is
      Period       : Double  := 1.0;
      Start_Time   : Integer := 0;
      A_Task       : Generic_Task_Ptr;
      My_Iterator  : Tasks_Iterator;
      Res, Current : multi_int_ptr;
   begin

      -- Check if Tasks Are Periodic
      --
      Periodic_Control (My_Tasks, Processor_Name);

      Res := new multi_int'(Multi (1));

      reset_iterator (My_Tasks, My_Iterator);

      loop
         current_element (My_Tasks, A_Task, My_Iterator);

         if (A_Task.cpu_name = Processor_Name and
             A_Task.task_type = Periodic_Type)
         then
            Start_Time := Natural'Max (Start_Time, A_Task.start_time);
            Current    :=
              new multi_int'(Multi (Periodic_Task_Ptr (A_Task).period));
            Res        := lcm (Res, Current);
         end if;

         exit when is_last_element (My_Tasks, My_Iterator);

         next_element (My_Tasks, My_Iterator);
      end loop;

      if Start_Time /= 0 then
         Current     := new multi_int'(Multi (2));
         Res.all     := Res.all * Current.all;
         Current.all := Multi (Start_Time);
         Res.all     := Res.all + Current.all;
      end if;

      Period := Double (convert (Res.all));

      return Period;

   end Scheduling_Period;


   function Scheduling_Period
     (My_Tasks       : in Tasks_Set;
      Processor_Name : in Unbounded_String)
      return           Natural
   is
      Period     : Natural := 1;
      Start_Time : Natural := 0;

      A_Task      : Generic_Task_Ptr;
      My_Iterator : Tasks_Iterator;

   begin

      -- Check if tasks are periodic
      --
      Periodic_Control (My_Tasks, Processor_Name);

      reset_iterator (My_Tasks, My_Iterator);

      loop
         current_element (My_Tasks, A_Task, My_Iterator);

         if (A_Task.cpu_name = Processor_Name and
             A_Task.task_type = Periodic_Type)
         then
            Start_Time := Natural'Max (Start_Time, A_Task.start_time);
            Period     :=
               natural_util.lcm (Period, Periodic_Task_Ptr (A_Task).period);
         end if;

         exit when is_last_element (My_Tasks, My_Iterator);

         next_element (My_Tasks, My_Iterator);
      end loop;

      if Start_Time /= 0 then
         Period := (Period * 2) + Start_Time;
      end if;

      return Period;

   end Scheduling_Period;

end feasibility_test.feasibility_interval;


