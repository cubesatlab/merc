-------------------------------------
-- Tests:
--   numeric types, ranges on integral types
--   fixed array sizes

pragma SPARK_Mode (On);

with CubedOS.Message_Types; use CubedOS.Message_Types;
with CubedOS.m0004.API; use CubedOS.m0004.API;
with Name_Resolver; use Name_Resolver;
with Ada.Text_IO; use Ada.Text_IO;

-- The program should complete successfully.
procedure Integration_Test_m0004 is
   Message : Message_Record;
   s1_element : s1_Struct;
begin
   -- Type checking

   pragma Assert(f1'First = 1);
   pragma Assert(f1'Last = 100);
   pragma Assert(f2'Length = 5);
   -- TODO: Pick a lower bound for arrays
   pragma Assert(f3'First = 1);
   pragma Assert(f3'Last = 100);

   pragma Assert(f4'Length = 5);
   declare
      f4Instance : f4 := (1 => 1, 2 => 100, 3 => 60, others => 10);
      pragma Unreferenced(f4Instance);
   begin
      null;
   end;

   pragma Assert(f5'First = 0);
   pragma Assert(f5'Last = 69);
   pragma Assert(f6'Length = 5);

   pragma Assert(f7'First = 11);
   pragma Assert(f7'Last = 100);

   -- For some reason Float type bound introspection doesn't work
   -- pragma Assert(f9'First = (-1.12));
   pragma Assert(f9'Last = 100.0);

   -- pragma Assert(f11'First = 1.23);
   -- pragma Assert(f11'Last = 100.67);

   pragma Assert(red in e1_Enum);
   pragma Assert(blue in e1_Enum);
   pragma Assert(green in e1_Enum);

   pragma Assert(e1'Length = 2);
   pragma Assert(e1'First = 1);

   pragma Assert(b'First = False);

   pragma Assert(int_var'Length = 2);

   pragma Assert(s1'Length = 2);


   s1_element := (
                  f2'(1 => 15, 2 => 20, others => 22),
                  f4'(others => 11),
                  f6'(others => 13),
                  f8'(others => 12),
                  f10'(others => 10.0),
                  f12'(others => 10.0),
                  e1'(blue, red),
                  (1 => 1, 2 => 3),
                  s1_Struct_i'(others => 5),
                  s1_Struct_hyp1'(others => 1)
                 );


   -- Encoder/decoder checking

   ms_Encode((0, m0004), (0, m0004), 0,
             mss1 => (others => s1_element),
             e3 => (red, blue),
             b2 => (False, True),
             h1 => (1, 7),
             Result => Message
            );

   declare
      Read_mss1 : s1;
      Read_e3 : e1;
      Read_b2 : b1;
      Read_h1 : ms_h1;
      Decode_Status : Message_Status_Type;
   begin
      ms_Decode(Message, Read_mss1, Read_e3, Read_b2, Read_h1, Decode_Status);
      pragma Assert(Decode_Status = Success);

    --  type s1_Struct is
    --    record
    --       fs1 : f2;
    --       fs2 : f4;
    --       fs3 : f6;
    --       fs4 : f8;
    --       fs5 : f10;
    --       fs6 : f12;
    --       e2 : e1;
    --       iv : int_var;
    --       i : s1_Struct_i;
    --       hyp1 : s1_Struct_hyp1;
    --    end record;

      -- For the first and second s1, each element of fs1 is correct
      Put_Line(f1'Image(Read_mss1(1).fs1(1)) & ": " & f1'Image(s1_element.fs1(1)));
      Put_Line(f1'Image(Read_mss1(1).fs1(2)) & ": " & f1'Image(s1_element.fs1(2)));
      Put_Line(f1'Image(Read_mss1(1).fs1(3)) & ": " & f1'Image(s1_element.fs1(3)));
      Put_Line(f1'Image(Read_mss1(1).fs1(4)) & ": " & f1'Image(s1_element.fs1(4)));
      Put_Line(f1'Image(Read_mss1(1).fs1(5)) & ": " & f1'Image(s1_element.fs1(5)));

      Put_Line(f1'Image(Read_mss1(2).fs1(1)) & ": " & f1'Image(s1_element.fs1(1)));
      Put_Line(f1'Image(Read_mss1(2).fs1(2)) & ": " & f1'Image(s1_element.fs1(2)));
      Put_Line(f1'Image(Read_mss1(2).fs1(3)) & ": " & f1'Image(s1_element.fs1(3)));
      Put_Line(f1'Image(Read_mss1(2).fs1(4)) & ": " & f1'Image(s1_element.fs1(4)));
      Put_Line(f1'Image(Read_mss1(2).fs1(5)) & ": " & f1'Image(s1_element.fs1(5)));

      -- For the first and second s1, each element of fs2 is correct
      Put_Line(f3'Image(Read_mss1(1).fs2(1)) & ": " & f3'Image(s1_element.fs2(1)));
      Put_Line(f3'Image(Read_mss1(1).fs2(2)) & ": " & f3'Image(s1_element.fs2(2)));
      Put_Line(f3'Image(Read_mss1(1).fs2(3)) & ": " & f3'Image(s1_element.fs2(3)));
      Put_Line(f3'Image(Read_mss1(1).fs2(4)) & ": " & f3'Image(s1_element.fs2(4)));
      Put_Line(f3'Image(Read_mss1(1).fs2(5)) & ": " & f3'Image(s1_element.fs2(5)));

      Put_Line(f3'Image(Read_mss1(2).fs2(1)) & ": " & f3'Image(s1_element.fs2(1)));
      Put_Line(f3'Image(Read_mss1(2).fs2(2)) & ": " & f3'Image(s1_element.fs2(2)));
      Put_Line(f3'Image(Read_mss1(2).fs2(3)) & ": " & f3'Image(s1_element.fs2(3)));
      Put_Line(f3'Image(Read_mss1(2).fs2(4)) & ": " & f3'Image(s1_element.fs2(4)));
      Put_Line(f3'Image(Read_mss1(2).fs2(5)) & ": " & f3'Image(s1_element.fs2(5)));

      -- For the first and second s1, each element of fs3 is correct
      Put_Line(f5'Image(Read_mss1(1).fs3(1)) & ": " & f5'Image(s1_element.fs3(1)));
      Put_Line(f5'Image(Read_mss1(1).fs3(2)) & ": " & f5'Image(s1_element.fs3(2)));
      Put_Line(f5'Image(Read_mss1(1).fs3(3)) & ": " & f5'Image(s1_element.fs3(3)));
      Put_Line(f5'Image(Read_mss1(1).fs3(4)) & ": " & f5'Image(s1_element.fs3(4)));
      Put_Line(f5'Image(Read_mss1(1).fs3(5)) & ": " & f5'Image(s1_element.fs3(5)));

      Put_Line(f5'Image(Read_mss1(2).fs3(1)) & ": " & f5'Image(s1_element.fs3(1)));
      Put_Line(f5'Image(Read_mss1(2).fs3(2)) & ": " & f5'Image(s1_element.fs3(2)));
      Put_Line(f5'Image(Read_mss1(2).fs3(3)) & ": " & f5'Image(s1_element.fs3(3)));
      Put_Line(f5'Image(Read_mss1(2).fs3(4)) & ": " & f5'Image(s1_element.fs3(4)));
      Put_Line(f5'Image(Read_mss1(2).fs3(5)) & ": " & f5'Image(s1_element.fs3(5)));

      -- For the first and second s1, each element of fs4 is correct
      Put_Line(f7'Image(Read_mss1(1).fs4(1)) & ": " & f7'Image(s1_element.fs4(1)));
      Put_Line(f7'Image(Read_mss1(1).fs4(2)) & ": " & f7'Image(s1_element.fs4(2)));
      Put_Line(f7'Image(Read_mss1(1).fs4(3)) & ": " & f7'Image(s1_element.fs4(3)));
      Put_Line(f7'Image(Read_mss1(1).fs4(4)) & ": " & f7'Image(s1_element.fs4(4)));
      Put_Line(f7'Image(Read_mss1(1).fs4(5)) & ": " & f7'Image(s1_element.fs4(5)));

      Put_Line(f7'Image(Read_mss1(2).fs4(1)) & ": " & f7'Image(s1_element.fs4(1)));
      Put_Line(f7'Image(Read_mss1(2).fs4(2)) & ": " & f7'Image(s1_element.fs4(2)));
      Put_Line(f7'Image(Read_mss1(2).fs4(3)) & ": " & f7'Image(s1_element.fs4(3)));
      Put_Line(f7'Image(Read_mss1(2).fs4(4)) & ": " & f7'Image(s1_element.fs4(4)));
      Put_Line(f7'Image(Read_mss1(2).fs4(5)) & ": " & f7'Image(s1_element.fs4(5)));

      -- For the first and second s1, each element of fs5 is correct
      Put_Line(f9'Image(Read_mss1(1).fs5(1)) & ": " & f9'Image(s1_element.fs5(1)));
      Put_Line(f9'Image(Read_mss1(1).fs5(2)) & ": " & f9'Image(s1_element.fs5(2)));
      Put_Line(f9'Image(Read_mss1(1).fs5(3)) & ": " & f9'Image(s1_element.fs5(3)));
      Put_Line(f9'Image(Read_mss1(1).fs5(4)) & ": " & f9'Image(s1_element.fs5(4)));
      Put_Line(f9'Image(Read_mss1(1).fs5(5)) & ": " & f9'Image(s1_element.fs5(5)));

      Put_Line(f9'Image(Read_mss1(2).fs5(1)) & ": " & f9'Image(s1_element.fs5(1)));
      Put_Line(f9'Image(Read_mss1(2).fs5(2)) & ": " & f9'Image(s1_element.fs5(2)));
      Put_Line(f9'Image(Read_mss1(2).fs5(3)) & ": " & f9'Image(s1_element.fs5(3)));
      Put_Line(f9'Image(Read_mss1(2).fs5(4)) & ": " & f9'Image(s1_element.fs5(4)));
      Put_Line(f9'Image(Read_mss1(2).fs5(5)) & ": " & f9'Image(s1_element.fs5(5)));

      Put_Line(e1_Enum'Image(Read_mss1(1).e2(1)) & ": " &  e1_Enum'Image(s1_element.e2(1)));
      Put_Line(e1_Enum'Image(Read_mss1(1).e2(2)) & ": " &  e1_Enum'Image(s1_element.e2(2)));
      Put_Line(e1_Enum'Image(Read_mss1(2).e2(1)) & ": " &  e1_Enum'Image(s1_element.e2(1)));
      Put_Line(e1_Enum'Image(Read_mss1(2).e2(2)) & ": " &  e1_Enum'Image(s1_element.e2(2)));

      pragma Assert(Read_mss1(1) = s1_element);
      pragma Assert(Read_mss1(2) = s1_element);

      Put_Line(e1_Enum'Image(Read_e3(1)) & ": " & e1_Enum'Image(red));
      Put_Line(e1_Enum'Image(Read_e3(2)) & ": " & e1_Enum'Image(blue));

      pragma Assert(Read_b2 = (False, True));
      pragma Assert(Read_h1 = (1, 7));

   end;

end Integration_Test_m0004;
