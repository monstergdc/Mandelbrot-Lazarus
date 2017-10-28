unit easycomplex;

//Complex numbers basic manipulation, v0.99
//(c) 2017 Noniewicz.com
//created: date unknown, circa 1992-1994
//(last date of orig file was 1993-12-02)
//revisited: 20171028 but not really tested

interface

type
  complex = record
    re, im: extended
  end;
  Pcomplex = ^complex;

var x: Pcomplex;

  function cx_makecx(re, im: extended): complex;
  function cx_add(a, b: complex): complex;
  function cx_sub(a, b: complex): complex;
  function cx_mult(a, b: complex): complex;
  function cx_divide(a, b: complex): complex;
  function cx_modul(a: complex): complex;
  function cx_argument(a: complex): complex;
  function cx_toAlgebraic(modul, arg: extended): complex;
  function cx_sqrRoot(modul, arg: extended): complex;


implementation


function cx_makecx(re, im: extended): complex;
begin
  result.re := re;
  result.im := im;
end;

function cx_add(a, b: complex): complex;
begin
  result.re := a.re + b.re;
  result.im := a.im + b.im;
end;

function cx_sub(a, b: complex): complex;
begin
  result.re := a.re - b.re;
  result.im := a.im - b.im;
end;

function cx_mult(a, b: complex): complex;
begin
  result.re := a.re*b.re-a.im*b.im;
  result.im := a.re*b.im+a.im*b.re;
end;

function cx_divide(a, b: complex): complex;
var zp: extended;
begin
  if (b.re <> 0) or (b.im <> 0) then
  begin
    zp := sqr(b.re) + sqr(b.im);
    result.re := (a.re*b.re + a.im*b.im) / zp;
    result.im := (a.im*b.re - a.re*b.im) / zp;
  end
  else  //err
  begin
    result.re := 0;
    result.im := 0;
  end
end;

function cx_modul(a: complex): complex;
begin
  result.im := 0;
  result.re := sqrt(sqr(a.re)+sqr(a.im));
end;

function cx_argument(a: complex): complex;
var x: complex;
begin
  x.im := 0;
  if (a.re = 0) and (a.im = 0) then
  begin
    x.re := 0;
    result := x;
    exit;
  end
  else
  begin
    if a.re = 0 then
    begin
      if a.im > 0 then
        x.re := pi/2
      else                 { a.im < 0 }
        x.re := -pi/2;
    end
    else
      if a.im = 0 then
      begin
         if a.re > 0 then
           x.re := 0
         else               { a.re < 0 }
           x.re := pi;
      end
      else
      begin
        if a.re > 0 then
          x.re := ArcTan(a.im/a.re)
        else
          if a.im > 0 then
            x.re := pi-ArcTan(a.im/-a.re)   {gnoj, Panie!} {WTF?}
          else
            x.re := -pi-ArcTan(a.im/-a.re);
      end;
  end;

  result := x;
end;

function cx_toAlgebraic(modul, arg: extended): complex;
begin
  result.re := modul * cos(arg);
  result.im := modul * sin(arg);
end;

function cx_sqrRoot(modul, arg: extended): complex;
begin
  result.re := sqrt(modul) * cos(arg/2);
  result.im := sqrt(modul) * sin(arg/2);
end;

end.

