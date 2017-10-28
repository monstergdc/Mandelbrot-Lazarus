unit FractalMainUnit;

//Fractal test (z'=z^2+c),v1.0
//(c)2017 Noniewicz.com, Jakub Noniewicz aka MoNsTeR/GDC
//created: 20171028 1700-1820
//updated: 20171028 1930-2040

{todo:
- why seems negative?
- center?
- expose params / allow bigger image
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
    BitBtn1: TBitBtn;
    BitBtn2: TBitBtn;
    Image1: TImage;
    Label1: TLabel;
    Panel1: TPanel;
    SaveDialog1: TSaveDialog;
    ScrollBox1: TScrollBox;
    SpinEditMaxIter: TSpinEdit;
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
  run_frac(-2.5, 1.0, -1.0, 1.0, SpinEditMaxIter.Value, 700, 400);
end;

procedure TForm1.AcSaveExecute(Sender: TObject);
begin
  if SaveDialog1.Execute then
  try
    Image1.Picture.Bitmap.SaveToFile(SaveDialog1.FileName);
  except
    on E: Exception do
      showmessage('Error saving image: '+E.Message);
  end;
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

  self.Image1.Picture.Bitmap.Width := w;
  self.Image1.Picture.Bitmap.Height := h;

  setlength(matrix, w*h);
  xs0 := (x1-x0)/w;
  ys0 := (y1-y0)/h;

  for y := 0 to h-1 do
  begin
    for x := 0 to w-1 do
    begin
      pix := 0;
      z.re := 0;
      z.im := 0;
      c.re := xs0*(x-w*0.7);
      c.im := ys0*(y-h*0.5);
      i := 0;
      ii := 0;
      noesc := true;
      while noesc and (i < maxiter) do
      begin
        try
          z := cx_add(cx_mult(z, z), c);
          if cx_modul(z).re > 1e999 then
            raise(Exception.Create(''));
        except
          ii := i;
          noesc := false;
        end;
        inc(i);
      end;
      if noesc then ii := 0;
      pv := 1.0 - single(ii) / single(maxiter);
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

