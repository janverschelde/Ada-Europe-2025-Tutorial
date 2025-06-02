with Ada.text_io;
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;
use  Ada.Numerics.Big_Numbers.Big_Reals;

procedure Big_SQRT is

-- DESCRIPTION :
--   Illustrates the application of Newton's method to
--   to compute a square root with big real numbers.

  package Integer_IO is new Ada.Text_IO.Integer_IO(integer);

  package Long_Float_IO is new Ada.Text_IO.Float_IO(Long_Float);

  package conversions is
    new Ada.Numerics.Big_Numbers.Big_Reals.Float_Conversions(long_float);

  function Square_Root ( x : Big_Real; eps : Long_Float;
                         verbose : in boolean := true )
                       return Big_Real is

  -- DESCRIPTION :
  --   Computes a big real approximation of the square root of x,
  --   accurate up to the given precision eps.

    two : constant Big_Real := To_Big_Real(2);
    flx : constant Long_Float := conversions.From_Big_Real(x);
    xbr : constant Big_Real := conversions.To_Big_Real(SQRT(flx));
    tol : constant Big_Real := conversions.To_Big_Real(eps);
    dp : constant integer := integer(-long_float(long_float'exponent(eps))/3.32);
    y,z : Big_Real;

  begin
    z := xbr;
    if verbose then
      Ada.Text_IO.put("initial z :");
      Ada.Text_IO.Put_Line(To_String(z,2,dp,0));
    end if;
    y := z*z - x;
    if verbose then
      Ada.Text_IO.put("        y :");
      Ada.Text_IO.Put_Line(To_String(y,2,dp,0));
    end if;
    for i in 1..100 loop
      z := z - y/(two*z);
      y := z*z - x;
      if verbose then
        Ada.Text_IO.Put("step "); Integer_IO.Put(i,1);
        Ada.Text_IO.Put(", z : ");
        Ada.Text_IO.Put_Line(To_String(z,2,dp,0));
        Ada.Text_IO.Put("        y :");
        Ada.Text_IO.Put_Line(To_String(y,2,dp,0));
      end if;
      exit when (abs(y) < tol);
    end loop;
    if verbose then
      Ada.Text_IO.Put("  numerator of z :");
      Ada.Text_IO.Put_Line(Big_Integer'Image(Numerator(z)));
      Ada.Text_IO.Put("denominator of z :");
      Ada.Text_IO.Put_Line(Big_Integer'Image(Denominator(z)));
    end if;
    return z;
  end Square_Root;

  procedure Main is

  -- DESCRIPTION :
  --   Computes the square root of 5 with tolerance 1.0E-255.

    eps : constant long_float := 1.0E-255;
    dp : constant integer := integer(-long_float(long_float'exponent(eps))/3.32);
    sqrt5 : constant Big_Real := square_root(5.0, 1.0E-255);
    value : Big_Real := sqrt5*sqrt5;

  begin
    Ada.Text_IO.Put_Line(To_String(value,2,dp,0));
  end Main;

begin
  Main;
end Big_SQRT;
