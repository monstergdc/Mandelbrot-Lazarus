unit FractalMainUnit;

//Mandelbrot fractal example (z'=z^2+c), v1.0
//(c)2017 Noniewicz.com, Jakub Noniewicz aka MoNsTeR/GDC
//created: 20171028 1700-1820
//updated: 20171028 1930-2120
//updated: 20171028 2155-2220
//updated: 20171030 1730-1745

{todo:
- color mapping?
- benchmark?
- no-GUI version
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  Buttons, ActnList, StdCtrls, Spin,
  mandelbrot;

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
var ext: string;
begin
  screen.Cursor := crHourGlass;
  AcRun.Enabled := false;
  AcSave.Enabled := false;
  if SaveDialog1.Execute then
  try
    ext := uppercase(ExtractFileExt(SaveDialog1.FileName));
    if ext = '.PNG' then
      Image1.Picture.PNG.SaveToFile(SaveDialog1.FileName);
    if ext = '.BMP' then
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
  SaveDialog1.FileName := 'fractal.png';
  SaveDialog1.FilterIndex := 2;
end;

procedure TForm1.run_frac(x0, x1, y0, y1: single; maxiter: integer; w, h: integer);
begin
  if (h <= 0) or (w <= 0) or (maxiter <= 0) then exit;

  Image1.Width := w;
  Image1.Height := h;
  Image1.Picture.Bitmap.Width := w;
  Image1.Picture.Bitmap.Height := h;
  self.Image1.Enabled := false;
  generate_mandelbrot(x0, x1, y0, y1, maxiter, w, h, cbNegative.Checked, Image1.Picture.Bitmap.Canvas);
  Image1.Enabled := true;
end;

end.

