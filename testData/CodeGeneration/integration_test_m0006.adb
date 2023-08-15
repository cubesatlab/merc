------------------------------------------------------
-- Tests the encoding/decoding of Time and Time_Span

pragma SPARK_Mode (On);

with CubedOS.m0006.API; use CubedOS.m0006.API;
with CubedOS.Message_Types; use CubedoS.Message_Types;
with Name_Resolver; use Name_Resolver;
with Ada.Real_Time; use Ada.Real_Time;
with Ada.Text_IO; use Ada.Text_IO;

-- Should complete successfully
procedure Integration_Test_m0006 is
   Message : Message_Record;
   time0 : constant Time := Ada.Real_Time.Time_Of(0, Seconds(51));
   time1 : constant Time := Ada.Real_Time.Time_Of(0, Milliseconds(101));
   time2 : constant Time := Ada.Real_Time.Time_Of(0, Microseconds(350));
   time3 : constant Time := Ada.Real_Time.Time_Of(0, Nanoseconds(450));

   function To_Str(T : Time) return String is
      S : Seconds_Count;
      TS : Time_Span;
   begin
      Split(T, S, TS);
      return Seconds_Count'Image(S) & "s and " & Duration'Image(To_Duration(TS)) & "s";
   end;

   span0 : constant Time_Span := Ada.Real_Time.Seconds(19990);
   span1 : constant Time_Span := Ada.Real_Time.Milliseconds(27187);
   span2 : constant Time_Span := Ada.Real_Time.Microseconds(8927394);
   span3 : constant Time_Span := Ada.Real_Time.Nanoseconds(5368221);
   span4 : constant Time_Span := Ada.Real_Time.Seconds(21000);
   span5 : constant Time_Span := Ada.Real_Time.Seconds(0); -- Min supported

   function To_Str(S : Time_Span) return String is
   begin
      return Duration'Image(To_Duration(S)) & "s";
   end;
begin
   -- Time
   Time_Message_Encode((0, m0006), (0, m0006), 0,
                       time1,
                       time2,
                       Pizza_Time(time3),
                       Result => Message);

   declare
      Read_time1 : Time;
      Read_time2 : Time;
      Read_time3 : Pizza_Time;
      Decode_Status : Message_Status_Type;

   begin
      Time_Message_Decode(Message, Read_time1, Read_time2, Read_time3, Decode_Status);
      pragma Assert(Decode_Status = Success);

      Put_Line(To_Str(Read_time1) & ": " & To_Str(time1));
      Put_Line(To_Str(Read_time2) & ": " & To_Str(time2));
      Put_Line(To_Str(Time(Read_time3)) & ": " & To_Str(time3));

      pragma Assert(Read_time1 = time1);
      pragma Assert(Read_time2 = time2);
      pragma Assert(Read_time3 = Pizza_Time(time3));
   end;

   Time_Message_Encode((0, m0006), (0, m0006), 0,
                       time0,
                       time1,
                       Pizza_Time(time2),
                       Result => Message);

   declare
      Read_time0 : Time;
      Read_time1 : Time;
      Read_time2 : Pizza_Time;
      Decode_Status : Message_Status_Type;
   begin
      Time_Message_Decode(Message, Read_time0, Read_time1, Read_time2, Decode_Status);
      pragma Assert(Decode_Status = Success);
      pragma Assert(Read_time0 = time0);
      pragma Assert(Read_time1 = time1);
      pragma Assert(Read_time2 = Pizza_Time(time2));
   end;

   -- Time Span
   Time_Span_Message_Encode((0, m0006), (0, m0006), 0,
                            span0,
                            span1,
                            Turtle_Time(span2),
                            Result => Message);

   declare
      Read_span0 : Time_Span;
      Read_span1 : Time_Span;
      Read_span2 : Turtle_Time;
      Decode_Status : Message_Status_Type;
   begin
      Time_Span_Message_Decode(Message, Read_span0, Read_span1, Read_span2, Decode_Status);
      pragma Assert(Decode_Status = Success);
      Put_Line(To_Str(span0) & ": " & To_Str(Read_span0));
      pragma Assert(Read_span0 = span0);
      Put_Line(To_Str(span1) & ": " & To_Str(Read_span1));
      pragma Assert(Read_span1 = span1);
      Put_Line(To_Str(span2) & ": " & To_Str(Time_Span(Read_span2)));
      pragma Assert(Time_Span(Read_span2) = span2);
   end;

   Time_Span_Message_Encode((0, m0006), (0, m0006), 0,
                            span1,
                            span2,
                            Turtle_Time(span3),
                            Result => Message);

   declare
      Read_span4 : Time_Span;
      Read_span5 : Time_Span;
      Read_span3 : Turtle_Time;
      Decode_Status : Message_Status_Type;
   begin
      Time_Span_Message_Decode(Message, Read_span4, Read_span5, Read_span3, Decode_Status);
      pragma Assert(Decode_Status = Success);
      Put_Line(To_Str(span4) & ": " & To_Str(Read_span4));
      pragma Assert(Read_span4 = span4);
      Put_Line(To_Str(span5) & ": " & To_Str(Read_span5));
      pragma Assert(Read_span5 = span5);
      Put_Line(To_Str(span3) & ": " & To_Str(Time_Span(Read_span3)));
      pragma Assert(Time_Span(Read_span3) = span3);
   end;

end Integration_Test_m0006;
