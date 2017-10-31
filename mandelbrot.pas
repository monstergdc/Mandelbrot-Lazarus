unit mandelbrot;

//Mandelbrot fractal example (z'=z^2+c), v1.0
//(c)2017 Noniewicz.com, Jakub Noniewicz aka MoNsTeR/GDC
//created: 20171028
//updated: 20171031

{todo:
- ?
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Graphics, easycomplex;


  procedure generate_mandelbrot(x0, x1, y0, y1: single; maxiter: integer; w, h: integer; negavite: boolean; Canvas: TCanvas);

implementation


procedure generate_mandelbrot(x0, x1, y0, y1: single; maxiter: integer; w, h: integer; negavite: boolean; Canvas: TCanvas);
var i, x, y, pix: integer;
    ii, xs0, ys0, pv: single;
    z, c: complex;
    matrix: array of byte;
    noesc: boolean;
begin
  if (h <= 0) or (w <= 0) or (maxiter <= 0) then exit;
  if not assigned(Canvas) then exit;

  Canvas.Brush.Color := clBlack;
  Canvas.Brush.Style := bsSolid;
  Canvas.FillRect(0, 0, w, h);

  setlength(matrix, w*h);
  xs0 := abs(x1-x0)/w;
  ys0 := abs(y1-y0)/h;

  for y := 0 to h-1 do
  begin
    for x := 0 to w-1 do
    begin
      z.re := 0;
      z.im := 0;
      c.re := x0+xs0*x;
      c.im := y0+ys0*y;
      i := 0;
      ii := 0;
      noesc := true;
      while noesc and (i < maxiter) do
      begin
        try
          z := cx_add(cx_mult(z, z), c);
          if cx_modul(z).re >= 2 then
            raise(Exception.Create(''));
        except
          ii := i;
          noesc := false;
        end;
        inc(i);
      end;
      if noesc then ii := maxiter;
      pv := 1.0 - single(ii) / single(maxiter);
      if negavite then
        pix := 255-round(pv*255)
      else
        pix := round(pv*255);
      matrix[x+y*w] := pix;
    end;
  end;

  for y := 0 to h-1 do
    for x := 0 to w-1 do
    begin
      pix := matrix[x+y*w];
      pix := (pix and 255) +
             (pix and 255 shl 8) +
             (pix and 255 shl 16);
      Canvas.Pixels[x, y] := pix;
    end;
end;

end.
