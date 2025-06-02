with Ada.text_io;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;

procedure Hello_Big_Integers is

-- DESCRIPTION :
--   Adjusted from learn Ada web site, at learn.adacore.edu.

  x : constant string :=
      "115792089237316195423570985008687907853269984665640564039457584007913129639936";
  y : Big_Integer;

begin
  Ada.Text_IO.Put_Line(Big_Integer'Image(2 ** 256));
  Ada.Text_IO.Put_Line(To_String(2**256));
  Ada.Text_IO.Put_Line(To_String(2**256, base=>16));
  Ada.Text_IO.Put_Line(To_String(2**256, base=>2));
  y := From_String(x);
  Ada.Text_IO.Put_Line(Big_Integer'Image(y));
end Hello_Big_Integers;
