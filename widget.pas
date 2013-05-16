unit Widget;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Menus;

type

  { TfrmWidget }

  TfrmWidget = class(TForm)
    miConfig: TMenuItem;
    pmWidget: TPopupMenu;
    procedure FormCreate(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FormMouseDown(Sender: TObject; Button: TMouseButton;
      Shift: TShiftState; X, Y: Integer);
    procedure FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
    procedure FormPaint(Sender: TObject);
    procedure FormResize(Sender: TObject);
  private
    procedure Draw;
  public
    mX, mY: Integer;

    FBuffer: TBitmap;
    FMask: TBitmap;
  end;

var
  frmWidget: TfrmWidget;

implementation

uses
  Main;

{$R *.lfm}

{ TfrmWidget }

procedure TfrmWidget.FormCreate(Sender: TObject);
begin
  FMask := TBitmap.Create;
  FMask.PixelFormat := pf1bit;

  FBuffer := TBitmap.Create;

  pmWidget.Images := frmMain.Image;
  miConfig.Action := frmMain.actConfig;
end;

procedure TfrmWidget.FormDestroy(Sender: TObject);
begin
  FMask.Free;
  FBuffer.Free;
end;

procedure TfrmWidget.FormMouseDown(Sender: TObject; Button: TMouseButton; Shift: TShiftState; X, Y: Integer);
begin
  mX := X;
  mY := Y;
end;

procedure TfrmWidget.FormMouseMove(Sender: TObject; Shift: TShiftState; X, Y: Integer);
begin
  if ssLeft in Shift then begin
    Left := Left + X - mX;
    Top  := Top  + Y - mY;
  end;
end;

procedure TfrmWidget.FormPaint(Sender: TObject);
begin
  Canvas.Draw(0, 0, FBuffer);
end;

procedure TfrmWidget.FormResize(Sender: TObject);
begin
  FMask.Width    := ClientWidth;
  FMask.Height   := ClientHeight;

  FBuffer.Width  := ClientWidth;
  FBuffer.Height := ClientHeight;

  Draw;
end;

procedure TfrmWidget.Draw;
var
  W, H: Integer;

  procedure DrawBlock(X, Y, W, H: Integer; AColor: TColor);
  begin
    FBuffer.Canvas.Brush.Color := RGBToColor(136, 136, 136);
    FBuffer.Canvas.FillRect(X + 2, Y + 2, X + W - 2, Y + H - 2);

    FMask.Canvas.FillRect(X, Y, X + W, Y + H);
  end;
begin
  FMask.Canvas.Brush.Color := clBlack;
  FMask.Canvas.FillRect(0, 0, FMask.Width, FMask.Height);
  FMask.Canvas.Brush.Color := clWhite;

  FBuffer.Canvas.Brush.Color := clBlack;
  FBuffer.Canvas.FillRect(0, 0, FBuffer.Width, FBuffer.Height);

  FBuffer.Canvas.Font.Color := RGBToColor(238, 238, 238);
  FBuffer.Canvas.Font.Size := 8;

  W := FBuffer.Canvas.TextWidth('narodmon.ru') + 4 + 12;
  H := FBuffer.Canvas.TextHeight('narodmon.ru') + 4 + 2;

  FMask.Canvas.FillRect(130 - W, 0, 130, H);

  DrawBlock(130 - W, 0, W, H, RGBToColor(136, 136, 136));

  FBuffer.Canvas.Brush.Style := bsClear;
  FBuffer.Canvas.TextOut(130 - W + 2 + 6, 3, 'narodmon.ru');

  frmWidget.SetShape(FMask);

  Repaint;
end;

end.

