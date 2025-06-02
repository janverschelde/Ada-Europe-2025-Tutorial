with Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;
use  Ada.Numerics.Big_Numbers.Big_Reals;

procedure Hello_Big_Reals is

  package Integer_IO is new Ada.Text_IO.Integer_IO(integer);

  function Square_Root ( x : in positive;
                         verbose : in boolean := true )
                       return Big_Real is

  -- DESCRIPTION :
  --   Computes a big real approximation of the square root of x,
  --   accurate up to 32 decimal places.

    two : constant Big_Real := To_Big_Real(2);
    xbr : constant Big_Real := To_Big_Real(To_Big_Integer(x));
    y,z : Big_Real;

  begin
    z := xbr;
    if verbose then
      Ada.Text_IO.put("initial z :");
      Ada.Text_IO.Put_Line(Big_Real'Image(z));
    end if;
    y := z*z - xbr;
    if verbose then
      Ada.Text_IO.put("        y :");
      Ada.Text_IO.Put_Line(Big_Real'Image(y));
    end if;
    for i in 1..100 loop
      z := z - y/(two*z);
      y := z*z - xbr;
      if verbose then
        Ada.Text_IO.Put("step "); Integer_IO.Put(i,1);
        Ada.Text_IO.Put(", z : ");
        Ada.Text_IO.Put_Line(To_String(z,2,32,0));
        Ada.Text_IO.Put("        y :");
        Ada.Text_IO.Put_Line(To_String(y,2,64,0));
      end if;
      exit when (y < 1.0e-32);
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

    nbr : constant positive := 127;
    root : Big_Real;

  begin
    root := Square_Root(nbr);
    Ada.Text_IO.Put("The square root of ");
    Integer_IO.Put(nbr,1);
    Ada.Text_IO.Put_Line(" is");
    Ada.Text_IO.Put_Line(To_String(root,2,32,0));
  end Main;
  
begin
  Main;
end Hello_Big_Reals;
