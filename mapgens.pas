unit MapGens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils,
  BGRABitmapTypes, BGRABitmap,
  RPGGens;
       
  function EdgeDistortionNone(t, amp: Single; normal: TPointF): TPointF; register;
  function EdgeDistortionSine(t, amp: Single; normal: TPointF): TPointF; register;
  function EdgeDistortionSaw(t, amp: Single; normal: TPointF): TPointF; register;
  function EdgeDistortionDblSaw(t, amp: Single; normal: TPointF): TPointF; register;
  function EdgeDistortionSemicircleOut(t, amp: Single; normal: TPointF): TPointF; register;
  function EdgeDistortionSemicircleIn(t, amp: Single; normal: TPointF): TPointF; register;
  function EdgeDistortionSemicircleWave(t, amp: Single; normal: TPointF): TPointF; register;

type

  TTreeShape = (tsSpherical, tsConical);
  TLeaveShape = (lsLeave, lsCircle, lsEllipse, lsWideEllipse, lsFir, lsHeart);
  TDistortionType = (dtNone, dtBlurEdge, dtWarpEdgeSine, dtWarpEdgeCirc);

  TLeafEdgeDistortionFunc = function(t, amp: Single; normal: TPointF): TPointF;

  TLeafEdgeDistortion = (ldNone, ldSine, ldSaw, ldDblSaw, ldCircleIn, ldCircleOut, ldCircleInOut);

const
  LeafEdgeDistortFuncs : array[TLeafEdgeDistortion] of TLeafEdgeDistortionFunc = (@EdgeDistortionNone,
                                                                                  @EdgeDistortionSine,
                                                                                  @EdgeDistortionSaw,
                                                                                  @EdgeDistortionDblSaw,
                                                                                  @EdgeDistortionSemicircleIn,
                                                                                  @EdgeDistortionSemicircleOut,
                                                                                  @EdgeDistortionSemicircleWave);

type


  TTreeShadowScanner = class(TBGRACustomScanner)
  private
    FWidth, FHeight,
    FDistortionFreq1, FDistortionFreq2,
    FConeHeight: Integer;
    FDistortionAmp: Single;
    FDistType: TDistortionType;
    FTreeShape: TTreeShape;
    FRootPnt, FTrunkPnt: TPointF;
    FTrunkRad: Single;
    FScaleFactor: Single;
    FTransMat: TAffineMatrix;
    FClipRect: TRect;
  public
    constructor Create;
    procedure SetTreeData(PosX, PosY, Width, Height: Integer;
                          DistortionType: TDistortionType;
                          DistortionFreq1, DistortionFreq2: Integer;
                          EdgeDistortion: Single;
                          LightAngle, LightHAngle: Single;
                          TreeShape: TTreeShape;
                          ConeHeight: Integer);
    function ScanAt(X,Y: Single): TBGRAPixel; override;
  end;

  TLeafTextureScanner = class(TBGRACustomScanner)
  private
    FSize: Integer;
    FBClr, FTClr, FRClr: TBGRAPixel;
    FRibDist: Single;
    FRand: TMRandom;
  public
    constructor Create(size: Integer; BClr, TClr, RClr: TBGRAPixel; RibDist: Single);
    destructor Destroy; override;
    function ScanAt(X, Y: Single): TBGRAPixel; override;
  end;

  function MakeTree(pwidth, pheight: Integer;
                      DistortionType: TDistortionType;
                      DistortionFreq1, DistortionFreq2: Integer;
                      EdgeDistortion: Single;
                      Leavesize: Integer; LeaveDensity: Single;
                      LightAngle, LightHAngle, LightGain: Single;
                      TreeShape: TTreeShape; ConeHeight: Integer;
                      LeaveShape: TLeaveShape;
                      LeaveHue, LeaveHueDeviation: Single;
                      LeaveLightnessLight, LeaveLightnessDark: Single): TBGRABitmap;

  function MakeLeafy(pRadius, LeavesPerLayer, Layers: Integer;
                     LayerSizeFactor: Single;
                     LeafWidthRel, LeafBottomAngle, LeafTopAngle, LeafMidAngle: Single;
                     EdgeDistortionType: TLeafEdgeDistortion;
                     BClr, TClr, RClr: TBGRAPixel;
                     RibDist: Single): TBGRABitmap;


implementation

uses
  Math, Graphics,
  BGRAGradientScanner, BGRAPath, BGRATransform,
  GeometryGens, GeometryHelpers;


function SortByDist(Item1, Item2: Pointer): Integer;
var
  p1, p2: TPoint;
begin
  p1 := TPointWrapper(Item1).point;
  p2 := TPointWrapper(item2).point;
  Result := -CompareValue(Hypot(p1.x, p1.y), Hypot(p2.x, p2.y));
end;


constructor TTreeShadowScanner.Create;
begin
  inherited;
  SetTreeData(0, 0, 1, 1, dtNone, 1, 1, 0, 0, 300, tsSpherical, 0);
end;

procedure TTreeShadowScanner.SetTreeData(PosX, PosY, Width, Height: Integer;
                                         DistortionType: TDistortionType;
                                         DistortionFreq1, DistortionFreq2: Integer;
                                         EdgeDistortion: Single;
                                         LightAngle, LightHAngle: Single;
                                         TreeShape: TTreeShape;
                                         ConeHeight: Integer);
var
  TrunkLen: Single;
  ClipPnts: array[0..3] of TPointF;
  rotate: TAffineMatrix;
  i: Integer;
begin
  if (360 - LightHAngle) mod 180 = 90 then
    FScaleFactor := 1
  else
    FScaleFactor := Hypot(Width, Width * Tan(DegToRad(360 - LightHAngle))) / Width;
  FRootPnt.SetLocation(PosX, PosY);
  TrunkLen := Max(Width, Height) * Tan(DegToRad(360 - LightHAngle));
  {FTrunkPnt := TPointF.Create(TrunkLen * -Sin(DegToRad(LightAngle)), TrunkLen * Cos(DegToRad(LightAngle)));
  ClipPnts[0] := FTrunkPnt * 2;
  FTrunkPnt.Offset(FRootPnt);
  ClipPnts[0].Offset(FRootPnt);}

  rotate := AffineMatrixRotationDeg(LightAngle + 90);
  FTrunkPnt := (rotate * TPointF.Create(TrunkLen, 0)) + FRootPnt;
  ClipPnts[0] := (rotate * TPointF.Create((TrunkLen + Width / 2) * FScaleFactor, Width)) + FRootPnt;
  ClipPnts[1] := (rotate * TPointF.Create((TrunkLen + Width / 2) * FScaleFactor, -Width)) + FRootPnt;
  ClipPnts[2] := (rotate * TPointF.Create(Min(TrunkLen, -Width / 2), Width)) + FRootPnt;
  ClipPnts[3] := (rotate * TPointF.Create(Min(TrunkLen, -Width / 2), -Width)) + FRootPnt;

  FClipRect := TRect.Create(Point(Round(FRootPnt.X), Round(FRootPnt.Y)), 0, 0);
  for i := 0 to 3 do
  begin
    FClipRect.Left   := Min(FClipRect.Left, Round(ClipPnts[i].x));
    FClipRect.Top    := Min(FClipRect.Top, Round(ClipPnts[i].Y));
    FClipRect.Right  := Max(FClipRect.Right, Round(ClipPnts[i].X));
    FClipRect.Bottom := Max(FClipRect.Bottom, Round(ClipPnts[i].y));
  end;
  FClipRect.Inflate(10, 10);

  //FClipRect := Rect(0, 0, 1000, 1000);
  //FClipRect.Create(Point(Round(FRootPnt.X), Round(FRootPnt.Y)), Point(Round(ClipPnt.X), Round(ClipPnt.Y)), True);

  FTransMat := AffineMatrixRotationDeg(LightAngle);
  FTransMat := AffineMatrixTranslation(FTrunkPnt.x, FTrunkPnt.Y) * FTransMat;
  if IsAffineMatrixInversible(FTransMat) then
    FTransMat := AffineMatrixInverse(FTransMat)
  else
    FTransMat := AffineMatrixIdentity;


  FTrunkRad := Map(Max(Width, Height), 122, 210, 5, 10);

  FWidth := Width;
  FHeight := Height;
  FDistortionFreq1 := DistortionFreq1;
  FDistortionFreq2 := DistortionFreq2;
  FConeHeight := ConeHeight;
  FDistortionAmp := EdgeDistortion;
  //FLightAngle := LightAngle;
  //FLightHAngle := LightHAngle;
  FDistType := DistortionType;
  FTreeShape := TreeShape;
end;

function TTreeShadowScanner.ScanAt(X,Y: Single): TBGRAPixel;
var
  dist1, dist2: Single;
  tmpPnt: TPointF;
  RadX, RadY: Single;
  WarpOff: Single;
  pntVal: Single;
begin
  // BGRA(0, 0, 0, 0);
  if not FClipRect.Contains(Point(Round(X), Round(Y))) then
    Exit(BGRA(255, 255, 255, 0));

  dist1 := SegmentDist(TPointF.Create(x, y), FRootPnt, FTrunkPnt) - FTrunkRad;

  WarpOff := 0;
  RadX := FWidth / 2;
  RadY := FHeight / 2;
  tmpPnt := TPointF.Create(x, y);
  //dist2 := Hypot((RadX - X) / RadX, (RadY - Y) / RadY);
  //dist2 := Hypot((FTrunkPnt.X - x), (FTrunkPnt.Y - Y)) - Max(RadX, RadY);
  dist2 := EllipseDist(FTransMat * tmpPnt, RadX, RadY * FScaleFactor);
  if FDistType = dtBlurEdge then
    dist2 := dist2 + Sin(20 * x) * Sin(20 * Y) * FDistortionAmp * Max(RadX, RadY)
  else if FDistType = dtWarpEdgeSine then
    dist2 := dist2 + Sqr(Sin(ArcTan2(FTrunkPnt.Y - Y, FTrunkPnt.x - X) * FDistortionFreq1 + WarpOff)) * Sin(ArcTan2(FTrunkPnt.y - Y, FTrunkPnt.x - X) * FDistortionFreq2 - WarpOff) * FDistortionAmp * Max(RadX, RadY)
  else if FDistType = dtWarpEdgeCirc then
    dist2 := dist2 - (CircleWave((ArcTan2(FTrunkPnt.Y - Y, FTrunkPnt.x - X) + PI) * FDistortionFreq1 / (2*PI)) * CircleWave((ArcTan2(FTrunkPnt.Y - Y, FTrunkPnt.x - X) + PI) * FDistortionFreq2 / (2*PI)) - 1) * FDistortionAmp * Max(RadX, RadY);


  pntVal := SmoothMin(dist1, dist2, 5);
  if pntVal < 0 then
    Result := BGRA(160, 160, 160, 128)
  else if pntVal < 10 then
    Result := BGRA(160, 160, 160, EnsureRange(Round(Map(pntVal, 0, 10, 128, 0)), 0, 255))
  else
    Result := BGRA(255, 255, 255, 0);
end;

function MakeTree(pWidth, pHeight: Integer;
                  DistortionType: TDistortionType;
                  DistortionFreq1, DistortionFreq2: Integer;
                  EdgeDistortion: Single;
                  Leavesize: Integer; LeaveDensity: Single;
                  LightAngle, LightHAngle, LightGain: Single;
                  TreeShape: TTreeShape; ConeHeight: Integer;
                  LeaveShape: TLeaveShape;
                  LeaveHue, LeaveHueDeviation: Single;
                  LeaveLightnessLight, LeaveLightnessDark: Single): TBGRABitmap;
var
  pntsRaw: TPointArray;
  pntRelList: TList;
  i, j: Integer;
  LightDir: TPointF;
  d, pntHeight: Double;
  CurPnt: TPoint;
  RadX, RadY: Integer;
  RadFactor: Single;
  CenterPnts: TPointArray;
  InCircle: Boolean;
  pntDist: Double;
  Div1, Div2: Double;
  LightScale: Double;
  hsla: THSLAPixel;
  rand, RandAngle: TMRandom;
  leave: TBGRAPath;
  rotation: TAffineMatrix;
  LeaveAngle: Single;
  LightHeight: Single;
  tmpHue: Single;
  ParticlesOnCircumference, ParticlesOnRadius: Integer;
  StrokeRunCount: Integer;
  ParticleCount: Integer;
  WarpOff: Single;
//const
  //LEAVESIZE = 16;
  //LIGHTHEIGHT = -0.5;
begin
  Result := TBGRABitmap.Create(pwidth, pHeight);
  rand := TMRandom.Create;
  // Punkte erzeugen
  pntsRaw := GetPoissonDiskPoints(pWidth - LEAVESIZE, pHeight - LEAVESIZE, LEAVESIZE * LeaveDensity);

  pntRelList := TList.Create;

  RadX := (pWidth div 2) - LEAVESIZE;
  RadY := (pHeight div 2) - LEAVESIZE;

  if (RadX <= 0) or (RadY <= 0) then
    Exit;

  RadFactor := 0.7;
  SetLength(CenterPnts, 3);
  for i := 0 to Length(CenterPnts) - 1 do
    CenterPnts[i] := Point(Round(Random * pWidth * (1 - RadFactor) + RadX),
                           Round(Random * pHeight * (1 - RadFactor) + RadY));

  //WarpOff := Random * 2 * PI;
  WarpOff := 0;
  // Nur Punkte im Kreis auswählen
  for i := 0 to Length(pntsRaw) - 1 do
  begin
    InCircle := False;
    pntDist := Hypot((RadX - pntsRaw[i].X) / RadX, (RadY - pntsRaw[i].Y) / RadY);
    // Mehrere Kreise
    //for j := 0 to Length(CenterPnts) - 1 do
    //  InCircle := InCircle or (Hypot((CenterPnts[j].X - pntsRaw[i].X) / (RadX * RadFactor), (CenterPnts[j].Y - pntsRaw[i].Y) / (RadY * RadFactor)) < 1);
    // Kreis
    if DistortionType = dtNone then
      InCircle := pntDist < 1
    // Umriss verzerren
    else if DistortionType = dtBlurEdge then
      InCircle := (pntDist - 1) + Sin(20 * pntsRaw[i].X)  * EdgeDistortion * Sin(20 * pntsRaw[i].Y) < 0
    else if DistortionType = dtWarpEdgeSine then
    InCircle := (pntDist - 1) + Sqr(Sin(ArcTan2(RadY - pntsRaw[i].Y, RadX - pntsRaw[i].X) * DistortionFreq1 + WarpOff)) * Sin(ArcTan2(RadY - pntsRaw[i].Y, RadX - pntsRaw[i].X) * DistortionFreq2 - WarpOff) * EdgeDistortion < 0
    else if DistortionType = dtWarpEdgeCirc then
      InCircle := (pntDist - 1) - (CircleWave((ArcTan2(RadY - pntsRaw[i].Y, RadX - pntsRaw[i].X) + PI) * DistortionFreq1 / (2*PI)) * CircleWave((ArcTan2(RadY - pntsRaw[i].Y, RadX - pntsRaw[i].X) + PI) * DistortionFreq2 / (2*PI)) - 1) * EdgeDistortion < 0;

    //if pntDist < 0.01 then InCircle := False;
    if InCircle then
    begin
      pntRelList.Add(TPointWrapper.Create(Point(pntsRaw[i].x - RadX + (LEAVESIZE div 2), pntsRaw[i].y - RadY + (LEAVESIZE div 2))));
    end;
  end;

  // Punkte nach Entfernung vom Mittelpunkt sortieren
  pntRelList.Sort(@SortByDist);

  // Lichtvektor
  //LightDir := TPointF.Create(1, 1);
  LightDir.x := -Sin(DegToRad(LightAngle));
  LightDir.Y := Cos(DegToRad(LightAngle));
  LightHeight := -Cos(DegToRad(LightHAngle));
  LightDir.Scale(-Sin(DegToRad(LightHAngle)));

  leave := TBGRAPath.Create;
  if LeaveShape = lsLeave then
  begin
    // Leave-shaped
    leave.moveTo(-LEAVESIZE / 2, 0);
    leave.quadraticCurveTo(0, -LEAVESIZE / 2, LEAVESIZE / 2, 0);
    leave.quadraticCurveTo(0, LEAVESIZE / 2, -LEAVESIZE / 2, 0);
  end
  else if LeaveShape = lsCircle then
  begin
    // Circle
    leave.moveTo(0, -LEAVESIZE / 2);
    leave.arcTo(1, 1, 0, false, false, 0, LEAVESIZE / 2);
    leave.arcTo(1, 1, 0, false, false, 0, -LEAVESIZE / 2);
  end
  else if LeaveShape = lsEllipse then
  begin
    // Elliptic tip out
    leave.moveTo(0, -LEAVESIZE / 4);
    leave.arcTo(1, 0.5, 0, false, false, 0, LEAVESIZE / 4);
    leave.arcTo(1, 0.5, 0, false, false, 0, -LEAVESIZE / 4);
  end
  else if LeaveShape = lsWideEllipse then
  begin
    // Elliptic bulge out
    leave.moveTo(-LEAVESIZE / 4, 0);
    leave.arcTo(0.5, 1, 0, false, false, LEAVESIZE / 4, 0);
    leave.arcTo(0.5, 1, 0, false, false, -LEAVESIZE / 4, 0);
  end
  else if LeaveShape = lsFir then
  begin
    // Fir branch
    leave.Free;
    leave := GetParticle_Fir(LEAVESIZE);
  end
  else
  begin
    // Heart
    leave.Free;
    leave := GetParticle_Heart(LEAVESIZE);
  end;

  // Zeichnen
  RandAngle := TMRandom.Create(123456789);
  // Zuerst die Umrisse
  for i := 0 to pntRelList.Count - 1 do
  begin
    // Blätter rotieren, sodass die Spitze nach außen zeigt
    LeaveAngle := -ArcTan2(TPointWrapper(pntRelList[i]).point.y, TPointWrapper(pntRelList[i]).point.x);
    // Ein bisschen Pseudo-Zufall zur Blattrotation dazu
    LeaveAngle := LeaveAngle + RandAngle.RandomGauss * 0.2;
    rotation := AffineMatrixRotationRad(LeaveAngle);
    rotation := AffineMatrixTranslation(TPointWrapper(pntRelList[i]).point.x + RadX,
                                        TPointWrapper(pntRelList[i]).point.y + RadY) * rotation;
    leave.stroke(Result,
                 rotation,
                 clBlack, 1.5);
  end;

  // Falls wir noch eine steckengebliebene Gauss-Zufallszahl haben, entfernen wir die hier
  // Für die Füllungen wollen wir schließlich genau die gleiche Folge haben
  if Odd(pntRelList.Count) then
    RandAngle.RandomGauss;

  // Dann die Füllung
  hsla.Alpha := 65535;
  //hsla.saturation := 65535;

  RandAngle.Seed := 123456789;

  ParticlesOnRadius := Ceil(Max(pWidth, pHeight) / (2 * LeaveSize * LeaveDensity));
  ParticlesOnCircumference := Ceil(2 * PI * ParticlesOnRadius);
  StrokeRunCount := -1;
  ParticleCount := pntRelList.Count;
  for i := 0 to pntRelList.Count - 1 do
  begin
    CurPnt := TPointWrapper(pntRelList[i]).point;
    if TreeShape = tsSpherical then
    begin
      d := Sqr(CurPnt.X / RadX) + Sqr(CurPnt.Y / RadY);
      pntHeight := 0;
      if d < 1 then
        pntHeight := Sqrt(1 - d) * Max(RadX, RadY);
    end
    else
    begin
      // Für kegelförmige Bäume:
      //pntHeight := -1 / Map(Hypot(CurPnt.X, CurPnt.Y), 0, RadX, ConeHeight, 0.1);
      pntHeight := Hypot(CurPnt.X / RadX, CurPnt.Y / RadY);
      pntHeight := 1 - Sqrt(Sqr(pntHeight-1) + 0.01);
      pntHeight := -1 / Map(pntHeight, 0, 1, ConeHeight, 0.1);
    end;

    Div1 := Sqrt(Sqr(CurPnt.X) + Sqr(CurPnt.Y) + Sqr(pntHeight));

    Div2 := Sqrt(Sqr(LightDir.x) + Sqr(LightDir.Y) + Sqr(LIGHTHEIGHT));

    LightScale := EnsureRange(((CurPnt.X * LightDir.X) + (CurPnt.Y * LightDir.Y) + (pntHeight * LIGHTHEIGHT)) / (Div1 * Div2), -1, 1);
    LightScale := Map(LightScale, -1, 1, 0, 1);
    LightScale := Gain(LightScale, LightGain);
    LightScale := Map(LightScale, 0, 1, -1, 1);
    LightScale := ArcCos(LightScale);
    hsla.lightness := Round(Map(LightScale, 0, PI, LeaveLightnessDark * 65535, LeaveLightnessLight * 65535));
    hsla.saturation := EnsureRange(Round(65535 * (0.9 + rand.RandomGauss * 0.1)), 0, 65535);

    //hsla.hue := Round(65535 * (120 / 360) + ;
    tmpHue := LeaveHue + rand.RandomGauss * LeaveHueDeviation;
    while tmpHue < 0 do tmpHue := tmpHue + 360;
    while tmpHue > 360 do tmpHue := tmpHue - 360;
    hsla.Hue := Round(65535 * tmpHue / 360);

    LeaveAngle := -ArcTan2(CurPnt.y, CurPnt.x);
    LeaveAngle := LeaveAngle + RandAngle.RandomGauss * 0.2;
    rotation := AffineMatrixRotationRad(LeaveAngle);
    rotation := AffineMatrixTranslation(CurPnt.x + RadX, CurPnt.y + RadY) * rotation;

    if (i > ParticlesOnCircumference * 1) and
       (i < pntRelList.Count - Round(PI * Sqr(ParticlesOnRadius / 5))) and
       (StrokeRunCount < 0) and
       (Random(100) <= 1) then
    begin
      StrokeRunCount := 10;
    end;
    if StrokeRunCount >= 0 then
    begin
      leave.stroke(Result,
                   rotation,
                   clBlack, 1.5);
      Dec(StrokeRunCount);
    end;

    leave.fill(Result,
               rotation,
               HSLAToBGRA(hsla));


  end;
  leave.Free;
  pntRelList.Free;
  rand.Free;
  RandAngle.Free;
end;

function LeafWindowFunc(CurPos, StableStart, StableEnd: Single): Single;
begin
  StableStart := Min(StableStart, 0.5);
  StableEnd := Max(0.5, StableEnd);
  if CurPos < StableStart then
    Result := Ease(CurPos, 0, 1, StableStart, etSmoothStep)
  else if CurPos > StableEnd then
    Result := Ease(CurPos - StableEnd, 1, -1, 1 - StableEnd, etSmoothStep)
  else
    Result := 1;
end;

//function LeafEdgeDistort(t: Single; Normal: TPointF): TPointF;

function EdgeDistortionNone(t, amp: Single; normal: TPointF): TPointF;
begin
  Result.SetLocation(0, 0);
end;

function EdgeDistortionSine(t, amp: Single; normal: TPointF): TPointF;
begin
  Result := normal * amp * Sin(t * 10 * PI);
end;

function EdgeDistortionSaw(t, amp: Single; normal: TPointF): TPointF;
begin
  Result := -normal * amp * Frac(t * 15);
end;

function EdgeDistortionDblSaw(t, amp: Single; normal: TPointF): TPointF;
begin
  Result := -normal * (amp * Frac(t * 10) + amp * 0.5 * Frac(t * 20));
end;

function EdgeDistortionSemicircleOut(t, amp: Single; normal: TPointF): TPointF;
begin
  Result := -normal * amp * sqrt(1 - sqr(frac(t * 10) * 2 - 1));
end;

function EdgeDistortionSemicircleIn(t, amp: Single; normal: TPointF): TPointF;
begin
  Result := normal * amp * sqrt(1 - sqr(frac(t * 10) * 2 - 1));
end;

function EdgeDistortionSemicircleWave(t, amp: Single; normal: TPointF): TPointF;
begin
  Result := normal * amp * sqrt(1 - sqr(frac(t * 10) * 2 - 1)) * sign(sin( t * 10*Pi));
end;

constructor TLeafTextureScanner.Create(size: Integer; BClr, TClr, RClr: TBGRAPixel; RibDist: Single);
begin
  inherited Create;
  FSize := size;
  FBClr := BClr;//BGRA(95, 134, 69);
  FTClr := TClr;//BGRA(125, 184, 92);
  FRClr := RClr;//BGRA(66, 116, 47);
  FRibDist := RibDist;//25;
  FRand := TMRandom.Create;
end;

destructor TLeafTextureScanner.Destroy;
begin
  FRand.Free;
  inherited;
end;

function TLeafTextureScanner.ScanAt(X, Y: Single): TBGRAPixel;
var
  pnt: TPointF;
  t: Single;
  d: Single;
//const
//  RIBOFFSET = 20;
begin
  pnt := TPointF.Create(Abs(x - (FSize div 2)), Frac(Y/FRibDist) * FRibDist);
  t := ClampedEase(FSize / 3, FSize, Y, etSmoothStep) + FRand.RandomGauss * 0.05;

  Result := MixPixel(FTClr, FBClr, t);

  d := SegmentDist(pnt, TPointF.Create(0, 0), TPointF.Create(0, FRibDist)) - 0;

  if FRibDist > 0 then
    d := SmoothMin(d, SegmentDist(pnt, TPointF.Create(0, FRibDist-2), TPointF.Create(FSize / 2, 0)), 4);

  d := ClampedEase(0, 5, d, etOutQuart);
  Result := MixPixel(FRClr, Result, d);

  //TBGRAGradientScanner.Create(BGRA(95, 134, 69), BGRA(125, 184, 92), gtRadial, TPointF.Create(pRadius / 2, pRadius), TPointF.Create(3 * pRadius / 4, pRadius / 2));
end;

function MakeLeafy(pRadius, LeavesPerLayer, Layers: Integer;
                   LayerSizeFactor: Single;
                   LeafWidthRel, LeafBottomAngle, LeafTopAngle, LeafMidAngle: Single;
                   EdgeDistortionType: TLeafEdgeDistortion;
                   BClr, TClr, RClr: TBGRAPixel;
                   RibDist: Single): TBGRABitmap;
var
  SingleLeaf: TBGRABitmap;
  i, j: Integer;
  transform: TAffineMatrix;
  OuterShape, ribs: TBGRAPath;
  OuterCursor: TBGRAPathCursor;
  ptsLeft, ptsRight: array of TPointF;
  vNormal: TPointF;
  RndAngle: TMRandom;
  LeafAngle, CurSizeFactor, CurAngleOffset: Single;
  //grad: TBGRAGradientScanner;
  grad: TLeafTextureScanner;
  BAngleRad, TAngleRad: Single;
  MidPnt, MidAngOffset: TPointF;
const
  LEAFBOTTOMSTR = 0.225;
  LEAFTOPSTR = 0.255;
  LEAFMIDSTR = 0.27;
//  LEAVECOUNT = 5;
begin
  Result := TBGRABitmap.Create(pRadius * 2, pRadius * 2, BGRA(0, 0, 0, 0));

  SingleLeaf := TBGRABitmap.Create(pRadius, pRadius);

  //M 0 0 C 0.2 -0.1 0.3 -0.25 0.2 -0.5 C 0.1 -0.75 0.05 -0.75 0 -1 C -0.05 -0.75 -0.1 -0.75 -0.2 -0.5  C -0.3 -0.25 -0.2 -0.1 0 0

  BAngleRad := DegToRad(LeafBottomAngle);
  TAngleRad := DegToRad(LeafTopAngle);
  MidPnt := TPointF.Create(pRadius * LeafWidthRel * 0.5, -0.5 * pRadius);
  MidAngOffset := TPointF.Create(Sin(DegToRad(LeafMidAngle)) * LEAFMIDSTR * pRadius, -Cos(DegToRad(LeafMidAngle)) * LEAFMIDSTR * pRadius);
  OuterShape := TBGRAPath.Create;
  OuterShape.moveTo(0, 0);
  OuterShape.bezierCurveTo(pRadius * LEAFBOTTOMSTR * Sin(BAngleRad), -Cos(BAngleRad) * LEAFBOTTOMSTR * pRadius, MidPnt.x - MidAngOffset.X, MidPnt.Y - MidAngOffset.Y, MidPnt.X, MidPnt.Y);//pRadius * 0.4, -0.5 * pRadius);
  OuterShape.bezierCurveTo(MidPnt.x + MidAngOffset.X, MidPnt.Y + MidAngOffset.Y, pRadius * Sin(TAngleRad) * LEAFTOPSTR, (-1 + Cos(TAngleRad)* LEAFTOPSTR) * pRadius, pRadius * 0.0, -1 * pRadius);
  {OuterShape.bezierCurveTo(pRadius * -0.05, -0.75 * pRadius, pRadius * -0.1, -0.75 * pRadius, pRadius * -0.2, -0.5 * pRadius);
  OuterShape.bezierCurveTo(pRadius * -0.3, -0.25 * pRadius, pRadius * -0.2, -0.1 * pRadius, 0, 0);
  OuterShape.closePath;}

  OuterCursor := TBGRAPathCursor.Create(OuterShape);

  SetLength(ptsLeft, Ceil(OuterCursor.PathLength));
  SetLength(ptsRight, Length(ptsLeft));

  OuterCursor.Position := 0;

  for i := 0 to Floor(OuterCursor.PathLength) do
  begin
    vNormal := TPointF.Create(OuterCursor.CurrentTangent.y, -OuterCursor.CurrentTangent.x);
    vNormal.Scale(1 / vNormal.Length);

    ptsLeft[i] := OuterCursor.CurrentCoordinate;
    ptsLeft[i] := ptsLeft[i] + LeafWindowFunc(i / OuterCursor.PathLength, 0.1, 0.7) * LeafEdgeDistortFuncs[EdgeDistortionType](i / OuterCursor.PathLength, 3, vNormal);
    //ptsLeft[i] := ptsLeft[i] + LeafWindowFunc(i / OuterCursor.PathLength, 0.1, 0.7) * EdgeDistortSemicircleIn(i / OuterCursor.PathLength, vNormal);
    ptsRight[i] := ptsLeft[i];
    ptsRight[i].x := - ptsRight[i].x;
    ptsLeft[i] := ptsLeft[i] + TPointF.Create(pRadius / 2, pRadius);
    ptsRight[i] := ptsRight[i] + TPointF.Create(pRadius / 2, pRadius);
    OuterCursor.MoveForward(1);
  end;

  OuterCursor.Position := OuterCursor.PathLength;
  ptsLeft[Length(ptsLeft) - 1] := OuterCursor.CurrentCoordinate + TPointF.Create(pRadius / 2, pRadius);
  ptsRight[Length(ptsRight) - 1] := OuterCursor.CurrentCoordinate + TPointF.Create(pRadius / 2, pRadius);

  
  OuterShape.Free;
  OuterCursor.Free;

  //ribs := TBGRAPath.Create;

  {for i := 0 to Length(ptsLeft) - 2 do
    if i mod 2 = 1 then
      ptsLeft[i].x := ptsLeft[i].x + 3;}

  //grad := TBGRAGradientScanner.Create(BGRA(95, 134, 69), BGRA(125, 184, 92), gtRadial, TPointF.Create(pRadius / 2, pRadius), TPointF.Create(3 * pRadius / 4, pRadius / 2));
  grad := TLeafTextureScanner.Create(pRadius, BClr, TClr, RClr, RibDist);
  //SingleLeaf.DrawPolygonAntialias(ptsLeft, BGRA(0, 128, 0), 2, BGRA(0, 192, 0));
  //SingleLeaf.DrawPolygonAntialias(ptsRight, BGRA(0, 128, 0), 2, BGRA(0, 192, 0));
  //SingleLeaf.DrawPolygonAntialias(ptsLeft, grad, 2);
  //SingleLeaf.DrawPolygonAntialias(ptsRight, grad, 2);
  SingleLeaf.FillPoly(ptsLeft, grad, dmDrawWithTransparency);
  SingleLeaf.FillPoly(ptsRight, grad, dmDrawWithTransparency);
  SingleLeaf.DrawPolyLineAntialias(ptsLeft, BGRA(0, 128, 0), 1);
  SingleLeaf.DrawPolyLineAntialias(ptsRight, BGRA(0, 128, 0), 1);
  // Durch Version mit TBGRAGradientScanner ersetzen

  //SingleLeaf.DrawLineAntialias(pRadius / 2, pRadius, pRadius / 2, 0, BGRA(0, 128, 0), 3, True);
 
  {SingleLeaf.FillEllipseAntialias(MidPnt.X + pRadius / 2, MidPnt.Y + pRadius, 3, 3, clRed);
  SingleLeaf.FillEllipseAntialias(MidPnt.X + MidAngOffset.X + pRadius / 2, MidPnt.Y + MidAngOffset.Y + pRadius, 2, 2, clRed);
  SingleLeaf.FillEllipseAntialias(MidPnt.X - MidAngOffset.X + pRadius / 2, MidPnt.Y - MidAngOffset.Y + pRadius, 2, 2, clRed);}

  //SingleLeaf.Fill(grad);

  RndAngle := TMRandom.Create;
  CurSizeFactor := 1;

  for j := 0 to Layers - 1 do
  begin
    CurAngleOffset := 0;
    if Odd(j) then
      CurAngleOffset := 360 / (LeavesPerLayer * 2);
    for i := 0 to LeavesPerLayer - 1 do
    begin
      LeafAngle := EnsureRange(rndAngle.RandomGauss * 10, -10, 10) + i * 360 / LeavesPerLayer + CurAngleOffset;

      //transform := AffineMatrixIdentity;
      transform := AffineMatrixTranslation(-pRadius / 2, -pRadius);
      //transform := AffineMatrixRotationDeg(360 / LeavesPerLayer * i) * transform;
      transform := AffineMatrixRotationDeg(LeafAngle) * transform;
      transform := AffineMatrixScale(CurSizeFactor, CurSizeFactor) * transform;
      transform := AffineMatrixTranslation(pRadius, pRadius) * transform;

      Result.PutImageAffine(transform, SingleLeaf);
    end;
    CurSizeFactor := CurSizeFactor * LayerSizeFactor;
  end;

  grad.Free;
  RndAngle.Free;
  //ribs.Free;
  SingleLeaf.Free;
end;

end.

