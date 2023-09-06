unit TreeForm;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ExtCtrls, StdCtrls,
  ComCtrls, SpinEx, BGRABitmap, {BGRACanvas2D,} BGRABitmapTypes, Types, GeometryHelpers,
  BGRAKnob, BCListBox,
  MapGens;

type

  { TForm1 }

  TShapeTestScanner = class(TBGRACustomScanner)
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;


  TForm1 = class(TForm)
    bkLightHAngle: TBGRAKnob;
    bkLightDir: TBGRAKnob;
    bRedraw: TButton;
    cbTreeShape: TComboBox;
    cbLeaveShape: TComboBox;
    cbDistortionType: TComboBox;
    Label10: TLabel;
    Label11: TLabel;
    Label12: TLabel;
    Label13: TLabel;
    Label14: TLabel;
    Label15: TLabel;
    Label16: TLabel;
    Label17: TLabel;
    Label18: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    seLeaveDensity: TFloatSpinEditEx;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    pbDisplay: TPaintBox;
    ScrollBox1: TScrollBox;
    seEdgeDistortion: TFloatSpinEditEx;
    seDistFreq1: TSpinEditEx;
    seDistFreq2: TSpinEditEx;
    seLightGain: TFloatSpinEditEx;
    seLeaveHueDeviation: TFloatSpinEditEx;
    seLeaveSize: TSpinEditEx;
    seWidth: TSpinEditEx;
    seHeight: TSpinEditEx;
    seConeHeight: TSpinEditEx;
    tbLeaveHue: TTrackBar;
    tbLeaveLightnessLight: TTrackBar;
    tbLeaveLightnessDark: TTrackBar;
    procedure bkLightDirValueChanged(Sender: TObject; Value: single);
    procedure bRedrawClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure pbDisplayPaint(Sender: TObject);
  private
    FTree: TBGRABitmap;
    FTestScanner: TShapeTestScanner;
    FShadowScanner: TTreeShadowScanner;
  public       


  end;

var
  Form1: TForm1;

implementation

{$R *.lfm}

uses Math,
  GeometryGens, BGRAGradients;

{ TShapeTestScanner }

function TShapeTestScanner.ScanAt(X, Y: Single): TBGRAPixel;
begin
  Result := clWhite;
  //if Hypot(x - 100, y - 100) + Sin(20 * X)  * 10 + 10 * Sin(20 * Y) < 100 then
  //if Hypot(x - 150, y - 150) + Sqr(Sin(ArcTan2(y - 150, x - 150) * 5 )) * 20 * Sin(ArcTan2(y - 150, x - 150) * 8 ) < 100 then
  //if Hypot(x - 150, y - 150) - sqrt(1 - sqr(frac((ArcTan2(y - 150, X - 150) + PI) * 7 / (2*PI)) * 2 - 1)) * 40 < 100 then
  if Hypot(x - 150, y - 150) - CircleWave((ArcTan2(y - 150, X - 150) + PI) * 7 / (2*PI)) * 40 * CircleWave((ArcTan2(y - 150, X - 150) + PI) * 4 / (2*PI)) < 100 then
    Result := clBlack;
end;

{ TForm1 }

procedure TForm1.pbDisplayPaint(Sender: TObject);
var tmpBmp: TBGRABitmap;
begin
  //FTree.Fill(FTestScanner);
  //FTree.Draw(pbDisplay.Canvas, (pbDisplay.Width - FTree.Width) div 2, (pbDisplay.Height - FTree.Height) div 2, False);
  FShadowScanner.SetTreeData(pbDisplay.Width div 2, pbDisplay.Height div 2, seWidth.Value, seHeight.Value,
                             TDistortionType(cbDistortionType.ItemIndex),
                             seDistFreq1.Value, seDistFreq2.Value, seEdgeDistortion.Value,
                             bkLightDir.Value, bkLightHAngle.Value, TTreeShape(cbTreeShape.ItemIndex),
                             seConeHeight.Value);
  tmpBmp := TBGRABitmap.Create(pbDisplay.Width, pbDisplay.Height, clWhite);
  tmpBmp.Fill(FShadowScanner, dmDrawWithTransparency);
  tmpBmp.Draw(pbDisplay.Canvas, 0, 0);
  FTree.Draw(pbDisplay.Canvas, (pbDisplay.Width - FTree.Width) div 2, (pbDisplay.Height - FTree.Height) div 2, False);
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  FTree := MakeTree(144, 144, dtNone, 1, 1, 0.1, 16, 0.35, 315, 315, 0.3, tsSpherical, 144*4, lsLeave, 110, 5, 0.4, 0);
  FTestScanner := TShapeTestScanner.Create;
  FShadowScanner := TTreeShadowScanner.Create;
end;

procedure TForm1.bRedrawClick(Sender: TObject);
begin
  FTree.Free;
  FTree := MakeTree(seWidth.Value, seHeight.Value,
                    TDistortionType(cbDistortionType.ItemIndex),
                    seDistFreq1.Value,
                    seDistFreq2.Value,
                    seEdgeDistortion.Value,
                    seLeaveSize.Value, seLeaveDensity.Value,
                    bkLightDir.Value, bkLightHAngle.Value, seLightGain.Value,
                    TTreeShape(cbTreeShape.ItemIndex), seConeHeight.Value,
                    TLeaveShape(cbLeaveShape.ItemIndex),
                    tbLeaveHue.Position, seLeaveHueDeviation.Value,
                    tbLeaveLightnessLight.Position / 100, tbLeaveLightnessDark.Position / 100);
  FShadowScanner.SetTreeData(pbDisplay.Width div 2, pbDisplay.Height div 2, seWidth.Value, seHeight.Value,
                             TDistortionType(cbDistortionType.ItemIndex),
                             seDistFreq1.Value, seDistFreq2.Value, seEdgeDistortion.Value,
                             bkLightDir.Value, bkLightHAngle.Value, TTreeShape(cbTreeShape.ItemIndex),
                             seConeHeight.Value);
  pbDisplay.Invalidate;
end;

procedure TForm1.bkLightDirValueChanged(Sender: TObject; Value: single);
begin
  bRedrawClick(Sender);
end;



end.

