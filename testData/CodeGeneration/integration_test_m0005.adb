-------------
-- This test includes strings and variable length opaque data.

pragma SPARK_Mode (On);

with CubedOS.m0005.API; use CubedOS.m0005.API;
with CubedOS.Message_Types; use CubedOS.Message_Types;
with Message_Manager; use Message_Manager;
with Name_Resolver; use Name_Resolver;
with Ada.Text_IO; use Ada.Text_IO;
with CubedOS.Lib; use CubedOS.Lib;

-- Tests that data is correctly encoded and decoded.
-- The program should complete successfully.
procedure Integration_Test_m0005 is
   Message : Message_Record;
begin

   -- Open_Request

   Open_Request_Encode((This_Domain.ID, m0005), (0, 1), 0,
                       Mode => Read,
                       Name => "filename.txt",
                       Result => Message
                      );
   declare
      Mode : Mode_Type;
      Name : File_Name_Type_Ptr;
      Status : Message_Status_Type;
   begin
      Open_Request_Decode(Message, Mode, Name, Status);
      pragma Assert(Mode = Read);
      Put_Line("Expected filename.txt: " & Name.all);
      pragma Assert(Name.all = "filename.txt");
      pragma Assert(Status = Success);
   end;

   Open_Request_Encode((This_Domain.ID, m0005), (0, 1), 0,
                       Mode => Write,
                       Name => "filename2.txt",
                       Result => Message
                      );
   declare
      Mode : Mode_Type;
      Name : File_Name_Type_Ptr;
      Status : Message_Status_Type;
   begin
      Open_Request_Decode(Message, Mode, Name, Status);
      pragma Assert(Mode = Write);
      Put_Line("Expected filename2.txt: " & Name.all);
      pragma Assert(Name.all = "filename2.txt");
      pragma Assert(Status = Success);
   end;

   -- Open_Reply

   Open_Reply_Encode((This_Domain.ID, m0005), (This_Domain.ID, m0005), 0,
                     Handle => 0,
                     Result => Message
                    );
   declare
      Handle : File_Handle_Type;
      Status : Message_Status_Type;
   begin
      Open_Reply_Decode(Message, Handle, Status);
      pragma Assert(Handle = 0);
      pragma Assert(Status = Success);
   end;

   Open_Reply_Encode((This_Domain.ID, m0005), (This_Domain.ID, m0005), 0,
                     Handle => 64,
                     Result => Message
                    );
   declare
      Handle : File_Handle_Type;
      Status : Message_Status_Type;
   begin
      Open_Reply_Decode(Message, Handle, Status);
      pragma Assert(Handle = 64);
      pragma Assert(Status = Success);
   end;

   -- Read
   Read_Reply_Encode((This_Domain.ID, m0005), (This_Domain.ID, m0005), 0,
                     Handle => 16,
                     File_Data => (0 => 0, 1 => 14, 2 => 255),
                     Result => Message);

   declare
      Handle : Valid_File_Handle_Type;
      File_Data : Octet_Array_Ptr;
      Status : Message_Status_Type;
   begin
      Read_Reply_Decode(Message, Handle, File_Data, Status);
      pragma Assert(Handle = 16);
      Put_Line("Expected Data length 3: " & Integer'Image(File_Data'Length));
      pragma Assert(File_Data'Length = 3);
      Put_Line("First byte 0: " & Octet'Image(File_Data(0)));
      pragma Assert(File_Data(0) = 0);
      Put_Line("Middle byte 14: " & Octet'Image(File_Data(1)));
      pragma Assert(File_Data(1) = 14);
      Put_Line("Last byte 255: " & Octet'Image(File_Data(2)));
      pragma Assert(File_Data(2) = 255);
      pragma Assert(Status = Success);
   end;

end Integration_Test_m0005;
