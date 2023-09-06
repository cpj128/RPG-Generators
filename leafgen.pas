unit LeafGen;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  SpinEx, BGRABitmap, BGRABitmapTypes;

type

  { TForm1 }

  TForm1 = class(TForm)
    bRedraw: TButton;
    bRandom: TButton;
    cbRColor: TColorButton;
    cbTColor: TColorButton;
    cbEdgeDistort: TComboBox;
    cbBColor: TColorButton;
    fsLayerScale: TFloatSpinEditEx;
    fsLeafWidth: TFloatSpinEditEx;
    Label1: TLabel;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    pbDisplay: TPaintBox;
    pnLeft: TPanel;
    seBottomAngle: TSpinEditEx;
    seRibDist: TSpinEditEx;
    seTopAngle: TSpinEditEx;
    seLeafSize: TSpinEditEx;
    seLayers: TSpinEditEx;
    seLeavesPerLayer: TSpinEditEx;
    seMidAngle: TSpinEditEx;
    procedure bRandomClick(Sender: TObject);
    procedure bRedrawClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbDisplayPaint(Sender: TObject);
  private
    FPlant: TBGRABitmap;
  public

  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses
  Math,
  MapGens,
  RPGGens;

{ TForm1 }

procedure TForm1.pbDisplayPaint(Sender: TObject);
begin
  FPlant.Draw(pbDisplay.Canvas, (pbDisplay.Width - FPlant.Width) div 2, (pbDisplay.Height - FPlant.Height) div 2, False);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FPlant := MakeLeafy(144, 5, 2, 0.8, 0.4, 63, 11, -22, ldNone, BGRA(95, 134, 69), BGRA(125, 184, 92), BGRA(66, 116, 47), 25);
end;

procedure TForm1.bRedrawClick(Sender: TObject);
begin
  if Assigned(FPlant) then
    FPlant.Free;
  FPlant := MakeLeafy(seLeafSize.Value, seLeavesPerLayer.Value, seLayers.Value, fsLayerScale.Value,
                      fsLeafWidth.Value, seBottomAngle.Value, seTopAngle.Value, seMidAngle.Value,
                      TLeafEdgeDistortion(cbEdgeDistort.ItemIndex), cbBColor.ButtonColor, cbTColor.ButtonColor, cbRColor.ButtonColor, 25);
  Invalidate;
end;

procedure TForm1.bRandomClick(Sender: TObject);
var
  rand: TMRandom;
  hsla: THSLAPixel;
begin
  rand := TMRandom.Create(GetTickCount);

  seLeavesPerLayer.Value := 4 + rand.Random(4);
  seLayers.Value := 2 + Random(2);

  seBottomAngle.Value := EnsureRange(Round(rand.RandomGauss * 45 + 45), 0, 90);
  seTopAngle.Value := EnsureRange(Round(rand.RandomGauss * 40 + 50), 10, 90);
  seMidAngle.Value := EnsureRange(Round((seTopAngle.Value - seBottomAngle.Value) / 2 + rand.RandomGauss * 10),
                                  -Min(seTopAngle.Value, seBottomAngle.Value),
                                  Min(seTopAngle.Value, seBottomAngle.Value));
  fsLeafWidth.Value := EnsureRange(rand.RandomGauss * 0.4 + 0.4, 0.1, 0.8);
  if ((seTopAngle.Value <= 10) or (seBottomAngle.Value <= 10)) and (Abs(seMidAngle.Value) > 30) and (fsLeafWidth.Value < 0.2) then
    fsLeafWidth.Value := fsLeafWidth.Value + 0.2;
  cbEdgeDistort.ItemIndex := rand.Random(cbEdgeDistort.Items.Count);

  hsla.hue := Round((96 + rand.RandomGauss * 10) / 360 * 65535);
  hsla.saturation := Round(EnsureRange(0.5 + rand.RandomGauss * 0.1, 0, 1) * 65535);
  hsla.lightness := Round(EnsureRange(0.5 + rand.RandomGauss * 0.1, 0, 1) * 65535/2);
  hsla.alpha := 65535;
  cbBColor.ButtonColor := hsla.ToColor;

  hsla.hue := Round((96 + rand.RandomGauss * 10) / 360 * 65535);
  hsla.saturation := Round(EnsureRange(0.5 + rand.RandomGauss * 0.1, 0, 1) * 65535);
  hsla.lightness := Round(EnsureRange(0.7 + rand.RandomGauss * 0.1, 0, 1) * 65535/2);
  hsla.alpha := 65535;
  cbTColor.ButtonColor := hsla.ToColor;

  hsla.hue := Round((96 + rand.RandomGauss * 10) / 360 * 65535);
  hsla.saturation := Round(EnsureRange(0.6 + rand.RandomGauss * 0.1, 0, 1) * 65535);
  hsla.lightness := Round(EnsureRange(0.45 + rand.RandomGauss * 0.1, 0, 1) * 65535/2);
  hsla.alpha := 65535;
  cbRColor.ButtonColor := hsla.ToColor;

  seRibDist.Value := EnsureRange(Round(25 + rand.RandomGauss * 10), 15, 35);

  bRedrawClick(self);
  rand.Free;
end;

end.

