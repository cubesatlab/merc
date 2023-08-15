--------------------------------------------------------------------------------
-- FILE    : name_resolver.ads
-- SUBJECT : Specification of a package holding Domain IDs and Module IDs
-- AUTHOR  : (C) Copyright 2023 by Vermont Technical College
--------------------------------------------------------------------------------
pragma SPARK_Mode (On);
with CubedOS.Message_Types; use CubedOS.Message_Types;

package Name_Resolver is

   m0001          : constant Module_ID_Type := 1;
   m0002          : constant Module_ID_Type := 2;
   m0003          : constant Module_ID_Type := 3;
   m0004          : constant Module_ID_Type := 4;
   m0005          : constant Module_ID_Type := 5;
   m0006          : constant Module_ID_Type := 6;


   -- Once every module used in the project has been assigned a unique ID, define
   -- one or more Domains, each with one or more modules and a unique domain ID.
   Domain : aliased constant Domain_Metadata := (5, 1,
                                                 (m0001,
                                                  m0002,
                                                  m0003,
                                                  m0004,
                                                  m0005));
end Name_Resolver;
