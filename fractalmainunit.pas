unit FractalMainUnit;

//Mandelbrot fractal example (z'=z^2+c), v1.0
//(c)2017 Noniewicz.com, Jakub Noniewicz aka MoNsTeR/GDC
//created: 20171028 1700-1820
//updated: 20171028 1930-2120
//updated: 20171028 2155-2220

{todo:
- color mapping?
- benchmark?
- jpg/png save?
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, Spin,
  easycomplex;

type

  { TForm1 }

  TForm1 = class(TForm)
    AcRun: TAction;
    AcSave: TAction;
    ActionList1: TActionList;
    BitBtnRun: TBitBtn;
    BitBtnSave: TBitBtn;
    cbNegative: TCheckBox;
    FloatSpinEditX0: TFloatSpinEdit;
    FloatSpinEditX1: TFloatSpinEdit;
    FloatSpinEditY0: TFloatSpinEdit;
    FloatSpinEditY1: TFloatSpinEdit;
    Image1: TImage;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    SpinEditMaxIter: TSpinEdit;
    SpinEditW: TSpinEdit;
    SpinEditH: TSpinEdit;
    procedure AcRunExecute(Sender: TObject);
    procedure AcSaveExecute(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
  public
    procedure run_frac(x0, x1, y0, y1: single; maxiter: integer; w, h: integer);
  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

{ TForm1 }


procedure TForm1.AcRunExecute(Sender: TObject);
begin
  screen.Cursor := crHourGlass;
  AcRun.Enabled := false;
  AcSave.Enabled := false;
  run_frac(FloatSpinEditX0.Value, FloatSpinEditX1.Value, FloatSpinEditY0.Value, FloatSpinEditY1.Value, SpinEditMaxIter.Value, SpinEditW.Value, SpinEditH.Value);
  AcSave.Enabled := true;
  AcRun.Enabled := true;
  screen.Cursor := crDefault;
end;

procedure TForm1.AcSaveExecute(Sender: TObject);
begin
  screen.Cursor := crHourGlass;
  AcRun.Enabled := false;
  AcSave.Enabled := false;
  if SaveDialog1.Execute then
  try
    Image1.Picture.Bitmap.SaveToFile(SaveDialog1.FileName);
  except
    on E: Exception do
      showmessage('Error saving image: '+E.Message);
  end;
  AcSave.Enabled := true;
  AcRun.Enabled := true;
  screen.Cursor := crDefault;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  SaveDialog1.InitialDir := ExtractFilePath(Application.ExeName);
end;

procedure TForm1.run_frac(x0, x1, y0, y1: single; maxiter: integer; w, h: integer);
var i, x, y, pix: integer;
    ii, xs0, ys0, pv: single;
    z, c: complex;
    matrix: array of byte;
    noesc: boolean;
begin
  if (h <= 0) or (w <= 0) or (maxiter <= 0) then exit;

  self.Image1.Width := w;
  self.Image1.Height := h;
  self.Image1.Picture.Bitmap.Width := w;
  self.Image1.Picture.Bitmap.Height := h;

  self.Image1.Picture.Bitmap.Canvas.Brush.Color := clBlack;
  self.Image1.Picture.Bitmap.Canvas.Brush.Style := bsSolid;
  self.Image1.Picture.Bitmap.Canvas.FillRect(0, 0, w, h);

  setlength(matrix, w*h);
  xs0 := abs(x1-x0)/w;
  ys0 := abs(y1-y0)/h;

  for y := 0 to h-1 do
  begin
    for x := 0 to w-1 do
    begin
      pix := 0;
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
      if cbNegative.Checked then
        pix := 255-round(pv*255)
      else
        pix := round(pv*255);
      matrix[x+y*w] := pix;
    end;
  end;

  self.Image1.Enabled := false;
  for y := 0 to h-1 do
    for x := 0 to w-1 do
    begin
      pix := matrix[x+y*w];
      pix := (pix and 255) +
             (pix and 255 shl 8) +
             (pix and 255 shl 16);
      self.Image1.Picture.Bitmap.Canvas.Pixels[x, y] := pix;
    end;
  self.Image1.Enabled := true;
end;

end.

