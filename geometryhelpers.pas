unit GeometryHelpers;


{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BGRABitmapTypes, fgl;

type
                
  TPointArray = array of TPoint;

  TPointWrapper = class
    point: TPoint;
    Angle: Double;
    constructor Create(pPoint: TPoint);
  end;


  TEdge = class
    P1, P2: TPoint;
    constructor Create(n1, n2: TPoint);
  end;

  TTriangle = class
    P1, P2, P3: TPoint;
    constructor Create(n1, n2, n3: TPoint);
    function CircumCenter: TPoint;
    function Centroid: TPoint;
    function Area: Double;
    function ContainsPoint(pnt: TPoint): Boolean;
  end;

  TTriangleArray = array of TTriangle;

  TPolygon = class
  private
    FPoints: TList;
    FCenter: TPoint;
  public
    constructor Create(pCenter: TPoint);
    destructor Destroy; override;
    procedure Add(val: TPoint);
    procedure Sort;
    procedure Scale(factor: Double);
    function GetPoint(idx: Integer): TPoint;
    function ToPointFArray: ArrayOfTPointF;
    function Count: Integer;
  end;

  TPolygonArray = array of TPolygon;

  TPointDict = specialize TFPGMap<string, TPolygon>;

  TPointList = specialize TFPGObjectList<TPointWrapper>;

// produces a wave of semicircles with a wavelength and amplitude of 1.
function CircleWave(t: Double): Double;

function Map(Val, InValStart, InValEnd, OutValStart, OutValEnd: Double): Double;

function Bias(Time, pBias: Double): Double;

function Gain(Time, pGain: Double): Double;

// Easings
type TEasingType = (etLinear,
                    etOutQuad,
                    etInQuad,
                    etInOutQuad,
                    etInCubic,
                    etOutCubic,
                    etInOutCubic,
                    etInQuart,
                    etOutQuart,
                    etInOutQuart,
                    etInQuint,
                    etOutQuint,
                    etInOutQuint,
                    etInSine,
                    etOutSine,
                    etInOutSine,
                    etInExpo,
                    etOutExpo,
                    etInOutExpo,
                    etInCirc,
                    etOutCirc,
                    etInOutCirc,
                    etInElastic,
                    etOutElastic,
                    etInOutElastic,
                    etInBack,
                    etOutBack,
                    etInOutBack,
                    etInBounce,
                    etOutBounce,
                    etInOutBounce,
                    etSmoothStep,
                    etSmootherStep);

function Ease(Time, StartVal, ChangeAmt, Duration: Double; eType: TEasingType): Double;

function ClampedEase(MinPos, MaxPos, CurPos: Double; eType: TEasingType): Double;

function SmoothMin(v1, v2, k: Double): Double;

function MixByte(b1, b2: Byte; s: Single): Byte;

function MixPixel(p1, p2: TBGRAPixel; s: Single): TBGRAPixel;

// A few selected SDFs - ported mostly from https://iquilezles.org/articles/distfunctions2d/

function SegmentDist(p, pnt1, pnt2: TPointF): Single;

function EllipseDist(p: TPointF; XRad, YRad: Single): Single;

implementation

uses
  Math;


function CircleWave(t: Double): Double;
begin
  Result := Sqrt(1 - Sqr(Frac(t) * 2 - 1));
end;

function Map(Val, InValStart, InValEnd, OutValStart, OutValEnd: Double): Double;
begin
  Result := OutValStart + (OutValEnd - OutValStart) * ((Val - InValStart) / (InValEnd - InValStart));
end;

function Bias(Time, pBias: Double): Double;
begin
  Result := Time / ((((1.0 / EnsureRange(pBias, 1e-7, 1)) - 2.0) * (1.0 - Time)) + 1.0);
end;

function Gain(Time, pGain: Double): Double;
begin
  //Result := Time / ((((1.0 / EnsureRange(pBias, 1e-7, 1)) - 2.0) * (1.0 - Time)) + 1.0);
  if time < 0.5 then
    Result := Bias(time * 2, pGain) / 2
  else
    Result := Bias(time * 2 - 1, 1 - pGain) / 2 + 0.5;
end;

function MixByte(b1, b2: Byte; s: Single): Byte;
begin
  Result := EnsureRange(Round(b1 + s * (b2 - b1)), 0, 255);
end;

function MixPixel(p1, p2: TBGRAPixel; s: Single): TBGRAPixel;
begin
  Result := BGRA(MixByte(p1.red, p2.Red, s),
                 MixByte(p1.green, p2.green, s),
                 MixByte(p1.blue, p2.blue, s),
                 MixByte(p1.alpha, p2.alpha, s));
end;


{ TPointWrapper }

constructor TPointWrapper.Create(pPoint: TPoint);
begin
  point := pPoint;
end;


{ TEdge }

constructor TEdge.Create(n1, n2: TPoint);
begin
  P1 := n1;
  P2 := n2;
end;

{ TTriangle }

constructor TTriangle.Create(n1, n2, n3: TPoint);
begin
  P1 := n1;
  P2 := n2;
  P3 := n3;
end;

function TTriangle.Area: Double;
begin
  Result := 0.5 * Abs(P1.X * (P2.Y - P3.Y) + P2.X * (P3.Y - P1.Y) + P3.X * (P1.Y - P2.Y));
end;

function TTriangle.CircumCenter: TPoint;
var
  sq1, sq2, sq3, d: Extended;
begin
  d := 2 * (P1.X * (P2.Y - P3.Y) + P2.X * (P3.Y - P1.Y) + P3.X * (P1.Y - P2.Y));
  if SameValue(d, 0) then
    raise Exception.Create('invalid triangle');
  sq1 := Sqr(P1.X) + Sqr(P1.Y);
  sq2 := Sqr(P2.X) + Sqr(P2.Y);
  sq3 := Sqr(P3.X) + Sqr(P3.Y);

  Result.X := Round((sq1 * (P2.Y - P3.Y) + sq2 * (P3.Y - P1.Y) + sq3 * (P1.Y - P2.Y)) / d);
  Result.Y := Round((sq1 * (P3.X - P2.X) + sq2 * (P1.X - P3.X) + sq3 * (P2.X - P1.X)) / d);
end;

function TTriangle.Centroid: TPoint;
begin
  Result := P1 + P2 + P3;
  Result.X := Round(Result.X / 3);
  Result.Y := Round(Result.Y / 3);
end;

function TTriangle.ContainsPoint(pnt: TPoint): Boolean;
var A0, A1, A2, A3: Double;
begin
  A0 := Abs(P1.X * (P2.Y - P3.Y) + P2.X * (P3.Y - P1.Y) + P3.X * (P1.Y - P2.Y));
  A1 := Abs(pnt.X * (P2.Y - P3.Y) + P2.X * (P3.Y - pnt.Y) + P3.X * (pnt.Y - P2.Y));
  A2 := Abs(P1.X * (pnt.Y - P3.Y) + pnt.X * (P3.Y - P1.Y) + P3.X * (P1.Y - pnt.Y));
  A3 := Abs(P1.X * (P2.Y - pnt.Y) + P2.X * (pnt.Y - P1.Y) + pnt.X * (P1.Y - P2.Y));
  Result := SameValue(A0, A1 + A2 + A3);
end;

{ TPolygon }

constructor TPolygon.Create(pCenter: TPoint);
begin
  FCenter := pCenter;
  FPoints := TList.Create;
end;

destructor TPolygon.Destroy;
var i: Integer;
begin
  for i := 0 to FPoints.Count - 1 do
    TPointWrapper(FPoints[i]).Free;
  FPoints.Free;
  inherited;
end;

procedure TPolygon.Add(val: TPoint);
var wrap: TPointWrapper;
begin
  wrap := TPointWrapper.Create(val);
  wrap.Angle := ArcTan2(FCenter.Y - val.Y, FCenter.X - val.X);
  FPoints.Add(wrap);
end;

function SortByAngle(Item1, Item2: Pointer): Integer;
begin
  Result := CompareValue(TPointWrapper(Item1).Angle, TPointWrapper(Item2).Angle);
end;

procedure TPolygon.Sort;
begin
  FPoints.Sort(@SortByAngle);
end;

procedure TPolygon.Scale(factor: Double);
var
  i: Integer;
  tmpPnt: TPointF;
begin
  for i := 0 to FPoints.Count - 1 do
  begin
    tmpPnt := TPointF.Create(TPointWrapper(FPoints[i]).point);
    tmpPnt.Offset(LongInt(-FCenter.X), -FCenter.Y);
    tmpPnt.Scale(factor);
    tmpPnt.Offset(FCenter);
    TPointWrapper(FPoints[i]).point := tmpPnt.Round;
  end;
end;

function TPolygon.GetPoint(idx: Integer): TPoint;
begin
  if (idx < 0) or (idx >= FPoints.Count) then
    Exit(Point(0, 0));
  Result := TPointWrapper(FPoints[idx]).point;
end;

function TPolygon.Count: Integer;
begin
  Result := FPoints.Count;
end;

function TPolygon.ToPointFArray: ArrayOfTPointF;
var i: Integer;
begin
  SetLength(Result, FPoints.Count);
  for i := 0 to FPoints.Count - 1 do
    Result[i] := TPointF.Create(TPointWrapper(FPoints[i]).point);
end;

{ Easing-Functions }

function EaseLinear(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * (Time / Duration) + StartVal;
end;

function EaseInQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqr(Time / Duration) + StartVal;
end;

function EaseOutQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * (Time / Duration) * ((Time / Duration) - 2) + StartVal;
end;

function EaseInOutQuad(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(t) + StartVal
  else
    Result := -ChangeAmt * 0.5 * ((t - 1) * (t - 3) - 1) + StartVal;
end;

function EaseInCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * t * t * t + StartVal;
end;

function EaseOutCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := ChangeAmt * (t * t * t + 1) + StartVal;
end;

function EaseInOutCubic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * t * t * t + StartVal
  else
    Result := ChangeAmt * 0.5 * ((t - 2) * (t - 2) * (t - 2) + 2) + StartVal;
end;

function EaseInQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqr(Sqr(Time / Duration)) + StartVal;
end;

function EaseOutQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := -ChangeAmt * (Sqr(Sqr(t)) - 1) + StartVal;
end;

function EaseInOutQuart(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(Sqr(t)) + StartVal
  else
    Result := -ChangeAmt * 0.5 * (Sqr(Sqr(t - 2)) - 2) + StartVal;
end;

function EaseInQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * Sqr(Sqr(t)) * t + StartVal;
end;

function EaseOutQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration - 1;
  Result := ChangeAmt * (Sqr(sqr(t)) * t + 1) + StartVal;
end;

function EaseInOutQuint(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := ChangeAmt * 0.5 * Sqr(Sqr(t)) * t + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sqr(Sqr(t - 2)) * (t - 2) + 2) + StartVal;
end;

function EaseInSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * Cos(Time / Duration * (PI / 2)) + ChangeAmt + StartVal;
end;

function EaseOutSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sin(Time / Duration * (PI / 2)) + StartVal;
end;

function EaseInOutSine(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * 0.5 * (Cos(PI * Time / Duration) - 1) + StartVal;
end;

function EaseInExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if SameValue(Time, 0.0) then
    Result := StartVal
  else
    Result := ChangeAmt * Power(2, 10 * (Time / Duration - 1)) + StartVal;
end;

function EaseOutExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if SameValue(Time, Duration) then
    Result := StartVal + ChangeAmt
  else
    Result := ChangeAmt * (-Power(2, -10 * Time / Duration) + 1) + StartVal;
end;

function EaseInOutExpo(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if SameValue(Time, 0) then
    Result := StartVal
  else if SameValue(Time, Duration) then
    Result := StartVal + ChangeAmt
  else if (t / 2) < 1 then
    Result := ChangeAmt * 0.5 * Power(2, 10 * (t - 1)) + StartVal
  else
    Result := ChangeAmt * 0.5 * (-Power(2, -10 * (t - 1)) + 2) + StartVal;
end;

function EaseInCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := -ChangeAmt * (Sqrt(1 - Sqr(Time / Duration)) - 1) + StartVal;
end;

function EaseOutCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt * Sqrt(1 - Sqr(Time / Duration - 1)) + StartVal;
end;

function EaseInOutCirc(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration * 2;
  if t < 1 then
    Result := -ChangeAmt * 0.5 * (Sqrt(1 - Sqr(t)) - 1) + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sqrt(1 - Sqr(t - 2)) + 1) + StartVal;
end;

function EaseInElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * Sin(13 * PI / 2 * t) * Power(2, 10 * (t - 1)) + StartVal;
end;

function EaseOutElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * (Sin(-13 * PI / 2 * (t + 1)) * Power(2, -10 * t) + 1) + StartVal;
end;

function EaseInOutElastic(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  if t < 0.5 then
    Result := ChangeAmt * 0.5 * Sin(13 * PI * t) * Power(2, 10 * ((2 * t) - 1)) + StartVal
  else
    Result := ChangeAmt * 0.5 * (Sin(-13 * PI * t) * Power(2, -10 * (2 * t - 1)) + 2) + StartVal;
end;

function EaseInBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  Result := ChangeAmt * (Sqr(t) * t - t * Sin(t * PI)) + StartVal;
end;

function EaseOutBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := 1 - Time / Duration;
  Result := ChangeAmt * (1 - (Sqr(t) * t - t * Sin(t * PI))) + StartVal;
end;

function EaseInOutBack(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  if (Time / Duration) < 0.5 then
  begin
    t := 2 * Time / Duration;
    Result := ChangeAmt * 0.5 * (Sqr(t) * t - t * Sin(t * PI)) + StartVal;
  end
  else
  begin
    t := 2 - 2 * Time / Duration;
    Result := ChangeAmt * (0.5 * (1 - (Sqr(t) * t - t * Sin(t * PI))) + 0.5) + StartVal;
  end;
end;

function EaseOutBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := Time / Duration;
  if t < (1 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t)) + StartVal
  else if t < (2 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t - (1.5 / 2.75)) + 0.75) + StartVal
  else if t < (2.5 / 2.75) then
    Result := ChangeAmt * (7.5625 * Sqr(t - (2.25 / 2.75)) + 0.9375) + StartVal
  else
    Result := ChangeAmt * (7.5625 * Sqr(t - (2.625 / 2.75)) + 0.984375) + StartVal;
end;

function EaseInBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  Result := ChangeAmt - EaseOutBounce(Duration - Time, 0, ChangeAmt, Duration) + StartVal;
end;

function EaseInOutBounce(Time, StartVal, ChangeAmt, Duration: Double): Double;
begin
  if Time < (Duration / 2) then
    Result := EaseInBounce(Time * 2, 0, ChangeAmt, Duration) * 0.5 + StartVal
  else
    Result := EaseOutBounce(Time * 2 - Duration, 0, ChangeAmt, Duration) * 0.5 + ChangeAmt * 0.5 + StartVal;
end;

function EaseSmoothStep(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := EnsureRange(Time / Duration, 0, 1);
  Result := ChangeAmt * (t * t * (3 - 2 * t)) + StartVal;
end;

function EaseSmootherStep(Time, StartVal, ChangeAmt, Duration: Double): Double;
var t: Double;
begin
  t := EnsureRange(Time / Duration, 0, 1);
  Result := ChangeAmt * (t * t * t * (t * (t * 6 - 15) + 10)) + StartVal;
end;

function Ease(Time, StartVal, ChangeAmt, Duration: Double; eType: TEasingType): Double;
begin
  case eType of
    etLinear      : Result := EaseLinear(Time, StartVal, ChangeAmt, Duration);
    etOutQuad     : Result := EaseOutQuad(Time, StartVal, ChangeAmt, Duration);
    etInQuad      : Result := EaseInQuad(Time, StartVal, ChangeAmt, Duration);
    etInOutQuad   : Result := EaseInOutQuad(Time, StartVal, ChangeAmt, Duration);
    etInCubic     : Result := EaseInCubic(Time, StartVal, ChangeAmt, Duration);
    etOutCubic    : Result := EaseOutCubic(Time, StartVal, ChangeAmt, Duration);
    etInOutCubic  : Result := EaseInOutCubic(Time, StartVal, ChangeAmt, Duration);
    etInQuart     : Result := EaseInQuart(Time, StartVal, ChangeAmt, Duration);
    etOutQuart    : Result := EaseOutQuart(Time, StartVal, ChangeAmt, Duration);
    etInOutQuart  : Result := EaseInOutQuart(Time, StartVal, ChangeAmt, Duration);
    etInQuint     : Result := EaseInQuint(Time, StartVal, ChangeAmt, Duration);
    etOutQuint    : Result := EaseOutQuint(Time, StartVal, ChangeAmt, Duration);
    etInOutQuint  : Result := EaseInOutQuint(Time, StartVal, ChangeAmt, Duration);
    etInSine      : Result := EaseInSine(Time, StartVal, ChangeAmt, Duration);
    etOutSine     : Result := EaseOutSine(Time, StartVal, ChangeAmt, Duration);
    etInOutSine   : Result := EaseInOutSine(Time, StartVal, ChangeAmt, Duration);
    etInExpo      : Result := EaseInExpo(Time, StartVal, ChangeAmt, Duration);
    etOutExpo     : Result := EaseOutExpo(Time, StartVal, ChangeAmt, Duration);
    etInOutExpo   : Result := EaseInOutExpo(Time, StartVal, ChangeAmt, Duration);
    etInCirc      : Result := EaseInCirc(Time, StartVal, ChangeAmt, Duration);
    etOutCirc     : Result := EaseOutCirc(Time, StartVal, ChangeAmt, Duration);
    etInOutCirc   : Result := EaseInOutCirc(Time, StartVal, ChangeAmt, Duration);
    etInElastic   : Result := EaseInElastic(Time, StartVal, ChangeAmt, Duration);
    etOutElastic  : Result := EaseOutElastic(Time, StartVal, ChangeAmt, Duration);
    etInOutElastic: Result := EaseInOutElastic(Time, StartVal, ChangeAmt, Duration);
    etInBack      : Result := EaseInBack(Time, StartVal, ChangeAmt, Duration);
    etOutBack     : Result := EaseOutBack(Time, StartVal, ChangeAmt, Duration);
    etInOutBack   : Result := EaseInOutBack(Time, StartVal, ChangeAmt, Duration);
    etInBounce    : Result := EaseInBounce(Time, StartVal, ChangeAmt, Duration);
    etOutBounce   : Result := EaseOutBounce(Time, StartVal, ChangeAmt, Duration);
    etInOutBounce : Result := EaseInOutBounce(Time, StartVal, ChangeAmt, Duration);
    etSmoothStep  : Result := EaseSmoothStep(Time, StartVal, ChangeAmt, Duration);
    etSmootherStep: Result := EaseSmootherStep(Time, StartVal, ChangeAmt, Duration);
  else
    Result := 0;
  end;
end;

function ClampedEase(MinPos, MaxPos, CurPos: Double; eType: TEasingType): Double;
begin
  if CurPos <= MinPos then
    Result := 0
  else if CurPos >= MaxPos then
    Result := 1
  else
    Result := Ease(CurPos - MinPos, 0, 1, MaxPos - MinPos, eType);
end;

function SmoothMin(v1, v2, k: Double): Double;
begin
  Result := Min(v1, v2) - Sqr(Max(k - Abs(v1 - v2), 0) / k) * k / 4;
end;

function SegmentDist(p, pnt1, pnt2: TPointF): Single;
var
  vPP1, vP2P1: TPointF;
  h: Single;
begin
  vPP1 := p - pnt1;
  vP2P1 := pnt2 - pnt1;
  h := EnsureRange((vPP1 * vP2P1) / (vP2P1 * vP2P1), 0, 1);
  Result := (vPP1 - vP2P1*h).Length;
end;

function EllipseDist(p: TPointF; XRad, YRad: Single): Single;
var
  pnt, r: TPointF;
  rax, ray, l, m, m2, n, n2, c, c3, q, d, g, co: Single;
  h, s, t, u, rm, rx, ry: Single;
begin
  if SameValue(XRad, YRad) then
    Exit(p.Length - XRad);
  if Abs(p.X) > Abs(p.y) then
  begin
    pnt := TPointF.Create(Abs(p.Y), Abs(p.X));
    rax := YRad;
    ray := XRad;
  end
  else
  begin
    pnt := TPointF.Create(Abs(p.X), Abs(p.Y));
    rax := XRad;
    ray := YRad;
  end;

  l := Sqr(ray) - Sqr(rax);
  m := rax * pnt.X / l;
  m2 := Sqr(m);
  n := ray * pnt.Y / l;
  n2 := Sqr(n);
  c := (m2 + n2 - 1) / 3;
  c3 := c * Sqr(c);
  q := c3 + m2 * n2 * 2;
  d := c3 + m2 * n2;
  g := m + m * n2;

  if d < 0 then
  begin
    h := ArcCos(q / c3) / 3;
    s := Cos(h);
    t := Sin(h) * Sqrt(3);
    rx := Sqrt(-c * (s + t + 2) + m2);
    ry := Sqrt(-c * (s - t + 2) + m2);
    co := (ry + Sign(l) * rx + Abs(g) / (rx * ry) - m) / 2;
  end
  else
  begin
    h := 2 * m * n * Sqrt(d);
    s := Sign(q + h) * Power(Abs(q + h), 1/3);
    u := Sign(q - h) * Power(Abs(q - h), 1/3);
    rx := -s - u - c * 4 + 2 * m2;
    ry := (s - u) * Sqrt(3);
    rm := Hypot(rx, ry);
    co := (ry / Sqrt(rm - rx) + 2 * g / rm - m) / 2;
  end;

  r := TPointF.Create(rax * co, ray * Sqrt(1 - Sqr(co)));
  Result := (r - pnt).Length * Sign(pnt.Y - r.Y);
end;

end.


