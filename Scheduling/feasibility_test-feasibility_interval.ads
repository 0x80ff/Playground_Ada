with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with unbounded_strings;     use unbounded_strings;
with Framework_Config;      use Framework_Config;
with Resource_Set;          use Resource_Set;
with Tasks;                 use Tasks;
with Task_Groups;           use Task_Groups;
with Parameters;            use Parameters;
with Parameters.extended;   use Parameters.extended;
use Parameters.User_Defined_Parameters_Table_Package;
with Offsets;               use Offsets;
with Offsets.extended;      use Offsets.extended;
use Offsets.Offsets_Table_Package;
with Ada.Numerics.Aux;      use Ada.Numerics.Aux;
with sets;

with Systems;               use Systems;
with Processor_Interface;   use Processor_Interface;

with Multi_precision_integers;          use Multi_precision_integers;
with Multi_precision_integers_IO;       use Multi_precision_integers_IO;
with multi_int_util;                    use multi_int_util;

------------------------------------------------------------------------
-- AUTOLAYOUT
-- Purpose:
-- This package computes feasibility intervals for simulation.
-- It encapsulates multiples algorithms which are designed to
-- calculate various feasibility intervals depending on the
-- considered system characteristics as well as providing
-- the validity of the calculation.
-- Effects:
-- - The expected usage is:
-- 1. Call Calculate_Feasibility_Interval to obtain the feasibility
--    interval of the considered system in multi_int type(Big Integers) type.
-- 2. Call Calculate_Feasibility_Interval_Top to obtain the
--    feasibility interval of the considered system in Double type.
-- - The others functions are supposed to be called within 1. and 2.
----------------------------------------------------------

package feasibility_test.feasibility_interval is

   ---------------------------------------------------------------------
   -- Is_Fixed_Task_Scheduler
   -- Purpose:
   -- This function determine if the scheduler use a fixed-task
   -- scheduling algorithm.
   ---------------------------------------------------------------------
   function Is_Fixed_Task_Scheduler
     ( Scheduler_Type : in Schedulers_type )
       return              Boolean;

   ---------------------------------------------------------------------
   -- Is_Fixed_Job_Scheduler
   -- Purpose:
   -- This function determine if the scheduler use a fixed-job
   -- scheduling algorithm.
   ---------------------------------------------------------------------
   function Is_Fixed_Job_Scheduler
     ( Scheduler_Type : in Schedulers_type )
       return              Boolean;

   ---------------------------------------------------------------------
   -- Is_Work_Conserving_Scheduler
   -- Purpose:
   -- This function determine if the scheduler use a work-conserving
   -- scheduling algorithm.
   ---------------------------------------------------------------------
   function Is_Work_Conserving_Scheduler
     ( Scheduler_Type : in Schedulers_type )
       return              Boolean;

   ---------------------------------------------------------------------
   -- Max_Start_Time_Of_TaskSet
   -- Purpose:
   -- This function determine the hightest starting time of a task in
   -- the provided tasks set.
   ---------------------------------------------------------------------
   function Max_Start_Time_Of_TaskSet
     ( My_Tasks : in Tasks_Set )
       return        Natural;


   ---------------------------------------------------------------------
   -- Calculate_Feasibility_Interval_Top
   -- Purpose:
   -- This procedure calculate the feasibility interval of the system
   -- in Double type and provide a boolean information stating true
   -- if the feasibility interval can be used as schedulability proof
   -- else false.
   ---------------------------------------------------------------------
   procedure Calculate_Feasibility_Interval_Top
     ( Sys	: in     System;
       Validate :    out Boolean;
       Interval :    out Double );

   ---------------------------------------------------------------------
   -- Calculate_Feasibility_Interval_Top
   -- Purpose:
   -- This procedure calculate the feasibility interval of the system
   -- in multi_int(Big Integers) type and provide a boolean information
   -- stating true if the feasibility interval can be used as
   -- schedulability proof else false.
   ---------------------------------------------------------------------
   procedure Calculate_Feasibility_Interval
     ( Sys	: in     System;
       Validate :    out Boolean;
       Interval :    out multi_int_ptr );

   ---------------------------------------------------------------------
   -- Scheduling_Period_1997
   -- Purpose: Calculation of simulation interval based on Goossens and
   -- Devillers paper from 1997 as well as 2006, 2011 and 2007 papers.
   --  *Used for independents and constrained tasks systems with:
   --	  Uniprocessor with fixed-job priority. (1997)
   --	  Uniform and Unrelated processors resp.(2006) and (2011)
   --     with Global fixed-task priority.
   --  *Identical processors with arbitrary deadlines, independent tasks
   --   with global fixed-task priority algorithm. (2007)
   -- The simulation interval of (2007) paper beeing slightly different,
   -- a boolean AddPeriod provide information for the calculation:
   -- If true, 2007 paper interval used else others.
   ---------------------------------------------------------------------
   function Scheduling_Period_1997
     ( My_Tasks	      : in Tasks_Set;
       Processor_Name : in Unbounded_String;
       AddPeriod      : in Boolean )
       return              multi_int;

   ---------------------------------------------------------------------
   -- Scheduling_Period_2012_2013
   -- Purpose: Calculation of simulation interval based on Baro et al. (2012)
   -- and Nélis et al. (2013) papers.
   -- 	Used for identical processor with constrained deadlines and
   -- 	independent or simple precedencies tasks systems with any scheduling
   -- 	algorithm.
   ---------------------------------------------------------------------
   function Scheduling_Period_2012_2013
     ( My_Tasks	      : in Tasks_Set;
       Processor_Name : in Unbounded_String )
       return 	           multi_int;

   ---------------------------------------------------------------------
   -- Scheduling_Period_2016
   -- Purpose: Calculation of simulation interval based on Goossens
   -- and Grolleau, Cucu-Grosjean (2016) paper.
   --	Used for identical processor with structural constraint and arbitrary
   --   deadlines with any scheduling algorithm.
   ---------------------------------------------------------------------
   function Scheduling_Period_2016
     ( My_Tasks	      : in Tasks_Set;
       Processor_Name : in Unbounded_String )
       return		   multi_int;

   ---------------------------------------------------------------------
   -- Scheduling_Period_With_Offset
   -- Purpose: Calculation of simulation interval based on Leung and Merill
   -- (1980) and Goossens and Devillers (1999) papers.
   --   Used for monoprocessor with constrained deadlines, independent tasks
   --   and fixed-task priority schheduling algorithm. (1980)
   --   Used for monoprocessor with arbitrary deadlines, independent tasks and
   --   fixed-job priority scheduling algorithm. (1999)
   -- Extra: Modification of the already implemented function but improved
   -- with handling of big integers calculation.
   ---------------------------------------------------------------------
   function Scheduling_Period_With_Offset
     ( My_Tasks       : in Tasks_Set;
       Processor_Name : in Unbounded_String )
      return              multi_int;

   -----------------------------------------------------------------------------
   function Scheduling_Period
     ( My_Tasks       : in Tasks_Set;
       Processor_Name : in Unbounded_String )
       return              Natural;

   function Scheduling_Period
     ( My_Tasks       : in Tasks_Set;
       Processor_Name : in Unbounded_String )
       return              Double;

end feasibility_test.feasibility_interval;
