unit GeometryGens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, BGRAPath, GeometryHelpers;
        
  function GetPoissonDiskPoints(pWidth, pHeight: Integer; pDistance: Double; pAttempts: Integer = 30): TPointArray;

  function Triangulate(points: TPointArray): TTriangleArray;

  function Voronoi(points: TPointArray): TPolygonArray; overload;
  function Voronoi(points: TPointArray; Triangles: TTriangleArray): TPolygonArray; overload;

  // Similar to Voronoi, but uses Centroid instead of circumsenter:
  function BarycentricMesh(points: TPointArray): TPolygonArray; overload;
  function BarycentricMesh(points: TPointArray; Triangles: TTriangleArray): TPolygonArray; overload;

  function ConvexHull(pnts: TPointArray): TPolygon;

  function GetDistancePointToLine(L1, L2, pnt: TPoint): Double;
  function GetPointSideOfLine(L1, L2, pnt: TPoint): Integer;

  function GetParticle_Leaf(size: Single): TBGRAPath;
  function GetParticle_Clover(size: Single; leaves: Integer): TBGRAPath;
  function GetParticle_Ivy(size: Single): TBGRAPath;
  function GetParticle_Heart(size: Single): TBGRAPath;
  function GetParticle_Fir(size: Single): TBGRAPath;


implementation

uses
  Math;

function GetDistancePointToLine(L1, L2, pnt: TPoint): Double;
begin
  Result := Abs((Pnt.Y - L1.Y) * (L2.X - L1.X) - (L2.Y - L1.Y) * (Pnt.X - L1.X)) / Hypot(L2.X - L1.X, L2.Y - L1.Y);
end;

function GetPointSideOfLine(L1, L2, pnt: TPoint): Integer;
begin
  // -1: Left of the line; 0: On the line; +1: Right of the line
  Result := Sign((Pnt.Y - L1.Y) * (L2.X - L1.X) - (L2.Y - L1.Y) * (Pnt.X - L1.X));
end;

function Voronoi(points: TPointArray): TPolygonArray;
begin
  Result := Voronoi(points, Triangulate(points));
end;

function Voronoi(points: TPointArray; Triangles: TTriangleArray): TPolygonArray;
var
  i: Integer;
  PointDict: TPointDict;
  tmpPoly: TPolygon;
  CurTri: TTriangle;
  CurCenter: TPoint;

  function PntToStr(val: TPoint): string;
  begin
    Result := IntToStr(val.X) + ';' + IntToStr(val.Y);
  end;

begin
  // We have a polygon for every base point
  PointDict := TPointDict.Create;
  SetLength(Result, Length(points));
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := TPolygon.Create(points[i]);
    PointDict.Add(PntToStr(points[i]), Result[i]);
  end;

  // Iterate all triangles, add circumcenter to polygon of all corner points
  for i := 0 to Length(Triangles) - 1 do
  begin
    CurTri := Triangles[i];
    CurCenter := CurTri.CircumCenter;
    if PointDict.TryGetData(PntToStr(CurTri.P1), tmpPoly) then
      tmpPoly.Add(CurCenter);
    if PointDict.TryGetData(PntToStr(CurTri.P2), tmpPoly) then
      tmpPoly.Add(CurCenter);
    if PointDict.TryGetData(PntToStr(CurTri.P3), tmpPoly) then
      tmpPoly.Add(CurCenter);
  end;
  // Sort polygons...
  for i := 0 to Length(Result) - 1 do
    Result[i].Sort;

  PointDict.Free;
end;

function BarycentricMesh(points: TPointArray): TPolygonArray;
begin
  Result := Voronoi(points, Triangulate(points));
end;

function BarycentricMesh(points: TPointArray; Triangles: TTriangleArray): TPolygonArray;
var
  i: Integer;
  PointDict: TPointDict;
  tmpPoly: TPolygon;
  CurTri: TTriangle;
  CurCenter: TPoint;

  function PntToStr(val: TPoint): string;
  begin
    Result := IntToStr(val.X) + ';' + IntToStr(val.Y);
  end;

begin
  // We have a polygon for every base point
  PointDict := TPointDict.Create;
  SetLength(Result, Length(points));
  for i := 0 to Length(Result) - 1 do
  begin
    Result[i] := TPolygon.Create(points[i]);
    PointDict.Add(PntToStr(points[i]), Result[i]);
  end;

  // Iterate all triangles, add circumcenter to polygon of all corner points
  for i := 0 to Length(Triangles) - 1 do
  begin
    CurTri := Triangles[i];
    CurCenter := CurTri.Centroid;
    if PointDict.TryGetData(PntToStr(CurTri.P1), tmpPoly) then
      tmpPoly.Add(CurCenter);
    if PointDict.TryGetData(PntToStr(CurTri.P2), tmpPoly) then
      tmpPoly.Add(CurCenter);
    if PointDict.TryGetData(PntToStr(CurTri.P3), tmpPoly) then
      tmpPoly.Add(CurCenter);
  end;
  // Sort polygons...
  for i := 0 to Length(Result) - 1 do
    Result[i].Sort;

  PointDict.Free;
end;

function ConvexHull(pnts: TPointArray): TPolygon;
var
  i: Integer;
  MinX, MaxX: Integer;
  MinIdx, MaxIdx: Integer;

  procedure FindHull(L1, L2: TPoint; SubPnts: TPointArray; poly: TPolygon);
  var
    myPnts: TPointArray;
    pointCount, i: Integer;
    MaxDist: Double;
    MaxDistIdx: Integer;
  begin
    SetLength(myPnts, Length(SubPnts));
    pointCount := 0;
    for i := 0 to Length(SubPnts) - 1 do
    begin
      if GetPointSideOfLine(L1, L2, SubPnts[i]) > 0 then
      begin
        myPnts[pointCount] := SubPnts[i];
        Inc(pointCount);
      end;
    end;

    if pointCount > 0 then
    begin
      SetLength(myPnts, pointCount);
      MaxDist := 0;
      MaxDistIdx := -1;
      for i := 0 to pointCount - 1 do
      begin
        if GetDistancePointToLine(L1, L2, myPnts[i]) > MaxDist then
        begin
          MaxDist := GetDistancePointToLine(L1, L2, myPnts[i]);
          MaxDistIdx := i;
        end;
      end;
      if MaxDistIdx >= 0 then
      begin
        poly.Add(myPnts[MaxDistIdx]);
        FindHull(L1, myPnts[MaxDistIdx], myPnts, poly);
        FindHull(myPnts[MaxDistIdx], L2, myPnts, poly);
      end;
    end;
  end;

begin
  // We need at least 3 points for a hull
  if Length(pnts) < 3 then
    Exit;
  // find left- and rightmost point
  MinX := MAXINT;
  MaxX := -MAXINT;
  MinIdx := -1;
  MaxIdx := -1;
  for i := 0 to Length(Pnts) - 1 do
  begin
    if pnts[i].x < MinX then
    begin
      MinX := pnts[i].x;
      MinIdx := i;
    end;
    if pnts[i].x > MaxX then
    begin
      MaxX := pnts[i].x;
      MaxIdx := i;
    end;
  end;

  // Check if we have found two valid, distinct points
  if (MaxIdx < 0) or (MinIdx < 0) or (MinIdx = MaxIdx) then
    Exit;

  Result := TPolygon.Create(Point((pnts[MinIdx].X + pnts[MaxIdx].X) div 2,
                                  (pnts[MinIdx].Y + pnts[MaxIdx].Y) div 2));
  Result.Add(pnts[MinIdx]);
  Result.Add(pnts[MaxIdx]);

  FindHull(pnts[MinIdx], pnts[MaxIdx], pnts, Result);
  FindHull(pnts[MaxIdx], pnts[MinIdx], pnts, Result);

  Result.Sort;
end;

function SortByXPos(Item1, Item2: Pointer): Integer;
begin
  Result := CompareValue(TPointWrapper(Item1).point.X, TPointWrapper(Item2).point.X);
end;

function Triangulate(points: TPointArray): TTriangleArray;
var
  i, j, k: Integer;
  boundingRect: TRect;
  SuperTriangle: TTriangle;
  maxDist: Integer;
  PointList: TList; // Contains TPointWrapper
  Open, Closed: TList; // contains TTriangle
  Edges: TList; // Contains TEdge
  CurTriangle: TTriangle;
  CurPoint, CurCenter: TPoint;
  CurRad: Double;
  Edge1, Edge2: TEdge;
  DoDelete: Boolean;
begin
  // Using Bowyer-Watson algorithm
  if Length(points) < 3 then
  begin
    // No triangles in this case
    SetLength(Result, 0);
    Exit;
  end;

  // Create list of points, sort by x-position
  PointList := Tlist.Create;
  for i := 0 to Length(points) - 1 do
    PointList.Add(TPointWrapper.Create(points[i]));

  PointList.Sort(@SortByXPos);

  // Calculate bounding triangle
  boundingRect := Rect(MAXINT, MAXINT, -MAXINT, -MAXINT);
  for i := 0 to Length(points) - 1 do
  begin
    boundingRect.Left := Min(boundingRect.Left, points[i].X);
    boundingRect.Top := Min(boundingRect.Top, points[i].Y);
    boundingRect.Right := Max(boundingRect.Right, points[i].X);
    boundingRect.Bottom := Max(boundingRect.Bottom, points[i].Y);
  end;
  maxDist := Max(boundingRect.Width, boundingRect.Height);

  SuperTriangle := TTriangle.Create(boundingRect.TopLeft + Point(-1, -1),
                                    Point(boundingRect.Left + boundingRect.Width * 2 + 1, boundingRect.Top),
                                    Point(boundingRect.Left, boundingRect.Top + boundingRect.Height * 2 + 1));

  Open := TList.Create;
  Open.Add(TTriangle.Create(SuperTriangle.P1, SuperTriangle.P2, SuperTriangle.P3));
  Closed := TList.Create;

  Edges := TList.Create;

  // Start adding points
  for i := 0 to PointList.Count - 1 do
  begin
    CurPoint := TPointWrapper(PointList[i]).point;

    // Check every open triangle if point is in the circumcircle
    j := 0;
    //for j := 0 to Open.Count - 1 do
    while j < Open.Count do
    begin
      CurTriangle := TTriangle(Open[j]);
      CurCenter := CurTriangle.CircumCenter;
      CurRad := Hypot(CurCenter.X - CurTriangle.P1.X, CurCenter.Y - CurTriangle.P1.Y);

      // If the point is to the right of the circumcircle, we never have to check this triangle again.
      // Remove from open list and add to result
      if CurPoint.X > CurCenter.X + CurRad then
      begin
        Closed.Add(CurTriangle);
        Open.Delete(j);
        Continue;
        // No Inc(j) here
      end;

      if Hypot(CurCenter.X - CurPoint.X, CurCenter.Y - CurPoint.Y) < CurRad then
      begin
        // Remove current triangle, add edges to list
        Edges.Add(TEdge.Create(CurTriangle.P1, CurTriangle.P3));
        Edges.Add(TEdge.Create(CurTriangle.P3, CurTriangle.P2));
        Edges.Add(TEdge.Create(CurTriangle.P2, CurTriangle.P1));
        Open.Delete(j);
        CurTriangle.Free;
      end
      else
        Inc(j);
    end;

    // Check edge list for duplicates, if found, remove all occurences of that edge
    j := 0;
    while j < Edges.Count - 1 do
    begin
      Edge1 := TEdge(Edges[j]);
      DoDelete := False;
      k := j + 1;
      while k < Edges.Count do
      begin
        Edge2 := TEdge(Edges[k]);
        if ((Edge1.P1 = Edge2.P1) and (Edge1.P2 = Edge2.P2)) or
           ((Edge1.P1 = Edge2.P2) and (Edge1.P2 = Edge2.P1)) then
        begin
          Edges.Delete(k);
          DoDelete := True;
        end
        else
          Inc(k);
      end;
      if DoDelete then
        Edges.Delete(j)
      else
        Inc(j);
    end;

    // Add new triangles
    for j := Edges.Count - 1 downto 0 do
    begin
      Edge1 := TEdge(Edges[j]);
      // Note that the points are not necessarily in CCW order here. Maybe do a bit of vector math and swap, if required.
      CurTriangle := TTriangle.Create(CurPoint, Edge1.P1, Edge1.P2);
      Open.Add(CurTriangle);
      Edges.Delete(j);
      Edge1.Free;
    end;
  end;

  // Add all remaining open triangles to closed list
  for i := Open.Count - 1 downto 0 do
  begin
    Closed.Add(Open[i]);
    Open.Delete(i);
  end;

  // Remove all triangles that share a vertex with the super triangle
  for i := Closed.Count - 1 downto 0 do
  begin
    CurTriangle := TTriangle(Closed[i]);
    if (CurTriangle.P1 = SuperTriangle.P1) or (CurTriangle.P1 = SuperTriangle.P2) or (CurTriangle.P1 = SuperTriangle.P3) or
       (CurTriangle.P2 = SuperTriangle.P1) or (CurTriangle.P2 = SuperTriangle.P2) or (CurTriangle.P2 = SuperTriangle.P3) or
       (CurTriangle.P3 = SuperTriangle.P1) or (CurTriangle.P3 = SuperTriangle.P2) or (CurTriangle.P3 = SuperTriangle.P3) then
      Closed.Delete(i);
  end;
  // Everything left over is our Result
  SetLength(Result, Closed.Count);
  for i := 0 to Closed.Count - 1 do
    Result[i] := TTriangle(Closed[i]);

  // Cleanup
  // These should be empty already
  Edges.Free;
  Open.Free;
  // We still need the contents of this one
  Closed.Free;
  // This one needs full clearing
  for i := 0 to PointList.Count - 1 do
    TPointWrapper(PointList[i]).Free;
  PointList.Free;

end;

function GetPoissonDiskPoints(pWidth, pHeight: Integer; pDistance: Double; pAttempts: Integer = 30): TPointArray;
var
  CellSize: Integer;
  GridCellsX, GridCellsY: Integer;
  CurCellX, CurCellY: Integer;
  PntGrid: array of array of Integer;
  PntCount: Integer;
  ActivePnts: TList; // Contains indices for the result array
  i, j: Integer;
  CurPnt, NewPnt, TestPnt: TPoint;
  CurPntIdx, CurListIdx: Integer;
  NewDist, NewAngle: Double;
  Found: Boolean;
  CurAttempt: Integer;
begin
  CellSize := Floor(pDistance / Sqrt(2));
  if CellSize = 0 then
    Exit;
  GridCellsX := Ceil(pWidth / CellSize);
  GridCellsY := Ceil(pHeight / CellSize);
  if (GridCellsX <= 0) or (GridCellsY <= 0) then
    Exit;
  SetLength(PntGrid, GridCellsX);
  for i := 0 to GridCellsX - 1 do
  begin
    SetLength(PntGrid[i], GridCellsY);
    for j := 0 to GridCellsY - 1 do
      PntGrid[i][j] := -1;
  end;
  // Upper bound for the size of out result set: one point per cell
  SetLength(Result, GridCellsX * GridCellsY);
  PntCount := 0;
  ActivePnts := TList.Create;

  // Add first point
  NewPnt := Point(Floor(Random * pWidth), Floor(Random * pHeight));
  Result[PntCount] := NewPnt;
  PntGrid[Floor(NewPnt.X / CellSize)][Floor(NewPnt.Y / CellSize)] := PntCount;
  ActivePnts.Add(Pointer(PntCount));
  Inc(PntCount);

  while ActivePnts.Count > 0 do
  begin
    CurListIdx := Random(ActivePnts.Count);
    CurPntIdx := Integer(ActivePnts[CurListIdx]);
    CurPnt := Result[CurPntIdx];
    // Try to find new point
    CurAttempt := 0;
    repeat
      Found := True;
      NewDist := (1 + Random) * pDistance;
      NewAngle := 360 * Random;
      NewPnt := Point(CurPnt.X + Round(Sin(DegToRad(NewAngle)) * NewDist),
                      CurPnt.Y - Round(Cos(DegToRad(NewAngle)) * NewDist));

      // Check if coordinates are insde the grid
      //if not PtInRect(Rect(0, 0, pWidth, pHeight), NewPnt) then
      if not Rect(0, 0, pWidth, pHeight).Contains(NewPnt) then
      begin
        Inc(CurAttempt);
        Found := False;
        Continue;
      end;

      CurCellX := Floor(NewPnt.X / CellSize);
      CurCellY := Floor(NewPnt.Y / CellSize);

      // Check if the cell is already occupied
      if PntGrid[CurCellX][CurCellY] >= 0 then
      begin
        Inc(CurAttempt);
        Found := False;
        Continue;
      end;

      // Check neighbour cells
      for i := Max(0, CurCellX - 2) to Min(CurCellX + 2, GridCellsX - 1) do
      begin
        for j := Max(0, CurCellY - 2) to Min(CurCellY + 2, GridCellsY - 1) do
        begin
          if (i <> CurCellX) or (j <> CurCellY) then
            if PntGrid[i][j] >= 0 then
            begin
              TestPnt := Result[Integer(PntGrid[i][j])];
              if Hypot(NewPnt.X - TestPnt.X, NewPnt.Y - TestPnt.Y) < pDistance then
              begin
                Inc(CurAttempt);
                Found := False;
                Break;
              end;
            end;
        end;
        if not Found then
          Break;
      end;

    until Found or (CurAttempt >= pAttempts);

    if Found then
    begin
      Result[PntCount] := NewPnt;
      PntGrid[Floor(NewPnt.X / CellSize)][Floor(NewPnt.Y / CellSize)] := PntCount;
      ActivePnts.Add(Pointer(PntCount));
      Inc(PntCount);
    end
    else
    begin
      //ActivePnts.Remove((CurPntIdx));
      ActivePnts.Delete(CurListIdx);
    end;
  end;

  SetLength(Result, PntCount);
  ActivePnts.Free;
end;

function GetParticle_Leaf(size: Single): TBGRAPath;
begin
  Result := TBGRAPath.Create;
  Result.moveTo(-size / 1, 0);
  Result.quadraticCurveTo(-size / 4, -size, size, 0);
  Result.quadraticCurveTo(-size / 4, size, -size, 0);
  Result.closePath;
end;

function GetParticle_Clover(size: Single; leaves: Integer): TBGRAPath;
var
  i: Integer;
  LeaveAngle, CurAngle: Single;
begin
  Result := TBGRAPath.Create;
  Result.moveTo(0, 0);
  LeaveAngle := 360 / leaves;
  for i := 0 to leaves - 1 do
  begin
    CurAngle := i * LeaveAngle;
    Result.bezierCurveTo(size * Sin(DegToRad(CurAngle - LeaveAngle * 0.5)) * 2, size * Cos(DegToRad(CurAngle - LeaveAngle * 0.5)) * 2,
                         size * Sin(DegToRad(CurAngle + LeaveAngle * 0.5)) * 2, size * Cos(DegToRad(CurAngle + LeaveAngle * 0.5)) * 2,
                         0, 0);
  end;
  Result.ClosePath;
end;

function GetParticle_Ivy(size: Single): TBGRAPath;
begin
  Result := TBGRAPath.Create;
  Result.moveTo(0, size * 0.37);
  Result.bezierCurveTo(size * 0.16, size * 0.64, size * 0.45, size * 0.59, size * 0.59, size * 0.52);
  Result.bezierCurveTo(size * 0.58, size * 0.46, size * 0.57, size * 0.39, size * 0.52, size * 0.32);
  Result.bezierCurveTo(size * 0.63, size * 0.17, size * 0.77, 0,           size * 0.8,  size * -0.17);
  Result.bezierCurveTo(size * 0.65, size * -0.22,size * 0.52, size * -0.22,size * 0.34, size * -0.23);
  Result.bezierCurveTo(size * 0.27, size * -0.45,size * 0.08, size * -0.9, 0,           size * -0.92);

  Result.bezierCurveTo(size * -0.08, size * -0.9, size * -0.27, size * -0.45,size * -0.34, size * -0.23);
  Result.bezierCurveTo(size * -0.52, size * -0.22,size * -0.65, size * -0.22,size * -0.8,  size * -0.17);
  Result.bezierCurveTo(size * -0.77, 0,           size * -0.63, size * 0.17, size * -0.52, size * 0.32);
  Result.bezierCurveTo(size * -0.57, size * 0.39, size * -0.58, size * 0.46, size * -0.59, size * 0.52);
  Result.bezierCurveTo(size * -0.45, size * 0.59, size * -0.16, size * 0.64, 0,            size * 0.37);
  Result.closePath;
end;

function GetParticle_Heart(size: Single): TBGRAPath;
begin
  Result := TBGRAPath.Create;
  Result.moveto(-size / 6, 0);
  Result.bezierCurveTo(-size / 2, 0,        -size / 2, -size / 3, -size / 6, -size / 3);
  Result.bezierCurveTo( size / 6, -size / 3, size / 6, 0,          size / 2, 0);
  Result.bezierCurveTo( size / 6, 0,         size / 6, size / 3,  -size / 6, size / 3);
  Result.bezierCurveTo(-size / 2, size / 3, -size / 2, 0,         -size / 6, 0);
  Result.ClosePath;
end;

function GetParticle_Fir(size: Single): TBGRAPath;
begin
  Result := TBGRAPath.Create;
  Result.moveTo(-size * 0.5,   size * 0.15);
  Result.lineTo(-size * 0.2,   size * 0.15);
  Result.lineTo( size * 0.1,   size * 0.45);
  Result.arcTo(1, 1, 0, false, true, size * 0.3, size * 0.25);
  Result.lineTo( size * 0.2,   size * 0.15);
  Result.lineTo( size * 0.35,  size * 0.15);
  Result.arcTo(1, 1, 0, false, true, size * 0.35, -size * 0.15);
  Result.lineTo( size * 0.2,  -size * 0.15);
  Result.lineTo( size * 0.3,  -size * 0.25);
  Result.arcTo(1, 1, 0, false, true, size * 0.1, -size * 0.45);
  Result.lineTo(-size * 0.2,  -size * 0.15);
  Result.lineTo(-size * 0.5,  -size * 0.15);
  Result.closePath;
  {M -10 3 L -4 3 L 2 9
   A 1 1 0 0 0 6 5
   L 4 3 L 7 3
   A 1 1 0 0 0 7 -3
   L 4 -3 L 6 -5
   A 1 1 0 0 0 2 -9
   L -4 -3 L -10 -3 Z}
end;

end.

