with Ada.text_io;
with Ada.Numerics.Long_Elementary_Functions;
use  Ada.Numerics.Long_Elementary_Functions;
with Ada.Numerics.Big_Numbers.Big_Integers;
use  Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;
use  Ada.Numerics.Big_Numbers.Big_Reals;

procedure Fibonacci is

-- Computes the sequence of Fibonacci numbers
-- to illustrate the Big Integers.

  package Integer_IO is new Ada.Text_IO.Integer_IO(integer);

  procedure Show_Fibonacci_Numbers ( n : in integer ) is

    previous : Big_Integer := To_Big_Integer(0);
    current : Big_Integer := To_Big_Integer(1);
    next : Big_Integer;

  begin
    Ada.Text_IO.Put_Line(Big_Integer'Image(previous));
    Ada.Text_IO.Put_Line(Big_Integer'Image(current));
    for i in 1..n loop
      next := previous + current;
      previous := current;
      current := next;
      Ada.Text_IO.Put_Line(Big_Integer'Image(current));
    end loop;
    Ada.Text_IO.Put("Number of decimal places : ");
    Integer_IO.Put(To_String(current)'length,1);
    Ada.Text_IO.New_Line;
  end Show_Fibonacci_Numbers;

  package Long_Float_IO is new Ada.Text_IO.Float_IO(Long_Float);

  package conversions is
    new Ada.Numerics.Big_Numbers.Big_Reals.Float_Conversions(long_float);

  function Square_Root ( x : Big_Real; eps : in Long_Float;
                         verbose : in boolean := true )
                       return Big_Real is

  -- DESCRIPTION :
  --   Computes a big real approximation of the square root of x,
  --   accurate up to the given precision eps.

    two : constant Big_Real := To_Big_Real(2);
    flx : constant Long_Float := conversions.From_Big_Real(x);
    xbr : constant Big_Real := conversions.To_Big_Real(SQRT(flx));
    tol : constant Big_Real := conversions.To_Big_Real(eps);
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
      exit when (y < tol);
    end loop;
    if verbose then
      Ada.Text_IO.Put("  numerator of z :");
      Ada.Text_IO.Put_Line(Big_Integer'Image(Numerator(z)));
      Ada.Text_IO.Put("denominator of z :");
      Ada.Text_IO.Put_Line(Big_Integer'Image(Denominator(z)));
    end if;
    return z;
  end Square_Root;

  procedure Long_Float_Evaluate_Formula ( n : in integer ) is

  -- DESCRIPTION :
  --   Evaluates the formula for the n-th Fibonacci number
  --   using long floats.

    sqrt5 : constant Long_Float := SQRT(5.0);
    fifthsqrt5 : constant Long_Float := sqrt5/5.0;
    oneplus : constant Long_Float := (1.0 + sqrt5)/2.0;
    oneminus : constant Long_Float := (1.0 - sqrt5)/2.0;
    value : Long_Float;

  begin
    Long_Float_IO.put(sqrt5);
    Long_Float_IO.put(sqrt5**2);
    Ada.Text_IO.New_Line;
    value := fifthsqrt5*oneplus**n - fifthsqrt5*oneminus**n;
    Long_Float_IO.put(value);
    Ada.Text_IO.New_Line;
  end Long_Float_Evaluate_Formula;

  procedure Big_Real_Evaluate_Formula ( n : in integer ) is

  -- DESCRIPTION :
  --   Evaluates the formula for the n-th Fibonacci number
  --   using big reals.

   -- sqrt5 : constant Big_Real := square_root(5.0, 1.0E-16);
    sqrt5 : constant Big_Real := conversions.To_Big_Real(SQRT(5.0));
    one : constant Big_Real := To_Big_Real(1);
    two : constant Big_Real := To_Big_Real(2);
    five : constant Big_Real := To_Big_Real(5);
    fifthsqrt5 : constant Big_Real := sqrt5/five;
    oneplus : constant Big_Real := (one + sqrt5)/two;
    oneminus : constant Big_Real := (one - sqrt5)/two;
    value : Big_Real;

  begin
    value := fifthsqrt5*oneplus**n - fifthsqrt5*oneminus**n;
    Ada.Text_IO.Put_Line(Big_Real'Image(value));
  end Big_Real_Evaluate_Formula;

  procedure Main is
  begin
    Show_Fibonacci_Numbers(1000);
    Long_Float_Evaluate_Formula(10);
    Big_Real_Evaluate_Formula(55);
  end Main;

begin
  Main;
end Fibonacci;
