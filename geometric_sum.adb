with Ada.text_io;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;
use  Ada.Numerics.Big_Numbers.Big_Reals;

procedure Geometric_Sum is

-- DESCRIPTION :
--   Computes a geometric sum with big reals.

  package Integer_IO is new Ada.Text_IO.Integer_IO(integer);

  package Long_Float_IO is new Ada.Text_IO.Float_IO(Long_Float);

  package conversions is
    new Ada.Numerics.Big_Numbers.Big_Reals.Float_Conversions(long_float);

  procedure Test ( dim : integer; ratio : Long_Float ) is

    gsum : Big_Real := To_Big_Real(1);
    bratio : constant Big_Real := conversions.To_Big_Real(ratio);
    accu : Big_Real := bratio;

  begin
    for i in 1..dim loop
      gsum := gsum + accu;
      accu := bratio*accu;
    end loop;
    Ada.Text_IO.put("the sum :");
    Ada.Text_IO.Put_Line(To_String(gsum,2,32,1));
    Ada.Text_IO.Put("  numerator :");
    Ada.Text_IO.Put_Line(Big_Integer'Image(Numerator(gsum)));
    Ada.Text_IO.Put("denominator :");
    Ada.Text_IO.Put_Line(Big_Integer'Image(Denominator(gsum)));
  end Test;

  procedure Main is

  -- DESCRIPTION :
  --   Computes a geometric sum with big reals ...

  begin
    Test(61,0.99999);
  end Main;

begin
  Main;
end Geometric_Sum;
