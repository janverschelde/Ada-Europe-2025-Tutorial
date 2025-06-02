with Ada.Text_IO;
with Ada.Numerics.Big_Numbers.Big_Integers;
 use Ada.Numerics.Big_Numbers.Big_Integers;
with Ada.Numerics.Big_Numbers.Big_Reals;
 use Ada.Numerics.Big_Numbers.Big_Reals;

procedure Pi_Rational_Approximations is

-- DESCRIPTION :
--   Truncating the fractions of Pi,
--   we can use conversions to the Big_Real
--   to compute rational approximations of pi.

  type integer64 is new long_long_integer;
  type unsigned_integer64 is mod 2**integer64'size;

  package long_float_io is new Ada.Text_IO.Float_io(long_float);
  package integer64_io is new Ada.Text_IO.integer_io(integer64);

  package conversions is
    new Ada.Numerics.Big_Numbers.Big_Reals.Float_Conversions(long_float);

  function last_mask ( nbrbits : integer ) return unsigned_integer64 is

  -- DESCRIPTION :
  --   Returns 1 + 2 + .. + 2^n, where n = nbrbits - 1,
  --   to serve as a mask to extract the last nbrbits
  --   of an unsigned 64-bit number with the "and" operator.

    res : unsigned_integer64 := 1;
    pwr : unsigned_integer64 := 2;

  begin
    for i in 1..nbrbits-1 loop
      res := res + pwr;
      pwr := 2*pwr;
    end loop;
    return res;
  end last_mask;

  function last_bits ( nbr : integer64;
                       nbrbits : integer ) return integer64 is

  -- DESCRIPTION :
  --   Returns the last bits of the number nbr,
  --   as many as the value of nbrbits.

    mask : constant unsigned_integer64 := last_mask(nbrbits);
    unbr : constant unsigned_integer64 := unsigned_integer64(nbr);
    rnbr : constant unsigned_integer64 := mask and unbr;

  begin
    return integer64(rnbr);
  end last_bits;

  function first_bits ( nbr : integer64;
                        nbrbits : integer ) return integer64 is

  -- DESCRIPTION :
  --   Returns the first nbrbits of the number nbr,
  --   when viewed as a 52-bit number.

  begin
     return nbr - last_bits(nbr, 52 - nbrbits);
  end first_bits;

  procedure Show_Rational_Approximations is

  -- DESCRIPTION :
  --   Takes the first bits of the fraction of Pi
  --   to compute rational approximations for Pi.

    x : constant long_float := Ada.Numerics.Pi;
    f : constant long_float := long_float'fraction(x);
    e : constant integer64 := integer64(long_float'exponent(x));
    s : constant long_float := long_float'compose(f, 52);
    m : constant integer64 := integer64(long_float'truncation(s));
    mchop : integer64;
    xchop : long_float;
    y : Big_Real;

  begin
    Ada.Text_IO.Put(" x : ");
    long_float_io.Put(x);
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put("52-bit fraction of x : ");
    integer64_io.Put(m,1);
    Ada.Text_IO.New_Line;
    Ada.Text_IO.Put("the exponent of x : ");
    integer64_io.Put(e,1);
    Ada.Text_IO.New_Line;
    for i in 1..51 loop
      mchop := first_bits(m,i);
      integer64_io.put(mchop,1,base=>2);
      xchop := long_float'compose(long_float(mchop), e);
      long_float_io.put(xchop);
      y := conversions.To_Big_Real(xchop);
      Ada.Text_IO.Put(Big_Integer'Image(Numerator(y)));
      Ada.Text_IO.Put(" /");
      Ada.Text_IO.Put_Line(Big_Integer'Image(Denominator(y)));
    end loop;
  end Show_Rational_Approximations;

begin
  Show_Rational_Approximations;
end Pi_Rational_Approximations;
