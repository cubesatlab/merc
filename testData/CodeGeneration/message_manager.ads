--------------------------------------------------------------------------------
-- FILE    : message_manager.adb
-- SUBJECT : Package holding the mailboxes used by CubedOS message passing.
-- AUTHOR  : (C) Copyright 2018 by Vermont Technical College
--
--------------------------------------------------------------------------------
pragma SPARK_Mode(On);
pragma Profile(Jorvik);
pragma Partition_Elaboration_Policy(Sequential);

with CubedOS.Generic_Message_Manager;
pragma Elaborate_All(CubedOS.Generic_Message_Manager);
with Name_Resolver;


  --  type Domain_Metadata(Module_Count : Positive) is
  --      record
  --         ID : Domain_ID_Type;
  --         Module_IDs : Module_ID_Set(1 .. Module_Count);
  --      end record;
package Message_Manager is
  new CubedOS.Generic_Message_Manager
    (Domain => Name_Resolver.Domain);
