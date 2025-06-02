with Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
 use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;
 use Ada.Numerics.Big_Numbers.Big_Reals;

procedure One_Tenth is

-- DESCRIPTION :
--   0.1 is not 1/10 as stored in a hardware double ...

 -- type double_float is digits 15;
  type integer64 is new long_long_integer;

 -- package double_float_io is new Ada.Text_IO.Float_io(double_float);
  package long_float_io is new Ada.Text_IO.Float_io(long_float);
  package integer64_io is new Ada.Text_IO.integer_io(integer64);

  package conversions is
   -- new Ada.Numerics.Big_Numbers.Big_Reals.Float_Conversions(double_float);
    new Ada.Numerics.Big_Numbers.Big_Reals.Float_Conversions(long_float);

  procedure Show_Double_Float is

  -- DESCRIPTION :
  --   Writes the fraction of 0.1, stored as a hardware double,
  --   in binary and in hexadecimal format, showing that the
  --   1/10 has an infinite binary expansion.

   -- x : constant double_float := 0.1;
   -- f : constant double_float := double_float'fraction(x);
   -- e : constant integer64 := integer64(double_float'exponent(x));
   -- c : constant double_float := double_float'compose(f, e);
   -- s : constant double_float := double_float'compose(f, 52);
   -- m : constant integer64 := integer64(double_float'truncation(s));
    x : constant long_float := 0.1;
    f : constant long_float := long_float'fraction(x);
    e : constant integer64 := integer64(long_float'exponent(x));
    c : constant long_float := long_float'compose(f, e);
    s : constant long_float := long_float'compose(f, 52);
    m : constant integer64 := integer64(long_float'truncation(s));

  begin
    Ada.Text_IO.Put(" x : ");
   -- double_float_io.Put(x);
    long_float_io.Put(x);
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put("52-bit fraction of x : ");
    integer64_io.Put(m,1);
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put("the exponent of x : ");
    integer64_io.Put(e,1);
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put("composed number of x : ");
   -- double_float_io.put(c);
    long_float_io.put(c);
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put("bin : ");
    integer64_io.put(m,1,base=>2);
    Ada.Text_io.New_Line;
    Ada.Text_IO.Put("hex : ");
    integer64_io.put(m,1,base=>16);
    Ada.Text_IO.New_Line;
  end Show_Double_Float;

  procedure Show_Big_Real is

  -- DESCRIPTION :
  --   Shows that the big real version of 0.1 is 1/10
  --   and shows the difference with the hardware double 0.1.

   -- x : constant double_float := 0.1;
    x : constant long_float := 0.1;
    y : constant Big_Real := To_Big_Real(1)/To_Big_Real(10);
    z : constant Big_Real := conversions.To_Big_Real(x) - y;

  begin
    Ada.Text_IO.Put("y : ");
    Ada.Text_IO.Put_Line(To_String(y,2,32,0));
    Ada.Text_IO.Put("  numerator of y :");
    Ada.Text_IO.Put_Line(Big_Integer'Image(Numerator(y)));
    Ada.Text_IO.Put("denominator of y :");
    Ada.Text_IO.Put_Line(Big_Integer'Image(Denominator(y)));
    Ada.Text_IO.Put("x - y : ");
    Ada.Text_IO.Put_Line(To_String(z,2,32,0));
  end Show_Big_Real;

begin
  Show_Double_Float;
  Show_Big_Real;
end One_Tenth;
