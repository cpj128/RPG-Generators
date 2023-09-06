unit RPGGens;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, DOM;

type

TCustomRNG = class
private
  NextGauss: Double;
  HasNextGauss: Boolean;
protected
  procedure SetSeed(pSeed: Cardinal); virtual; abstract;
  function NextValue: Cardinal; virtual; abstract;
public
  //constructor Create;
  function Random(val: Cardinal): Cardinal; overload;
  function Random: Double; overload;
  function RandomGauss: Double;
  property Seed: Cardinal write SetSeed;
end;

// Marsaglias KISS-Zufallsgenerator
TMRandom = class(TCustomRNG)
private
  x, y, z, c: Longword;
protected
  procedure SetSeed(pSeed: Cardinal); override;
  function NextValue: Cardinal; override;
public
  constructor Create(pSeed: Cardinal = 123456789);
end;

TNodeType = (ntError, ntBinary, ntValue);

TParserNode = class
public
  NodeType: TNodeType;
  Operation: Char;
  LeftChild, RightChild: TParserNode;
  Value: Integer;
  constructor Create(op: Char; Left: TParserNode);
  destructor Destroy; override;
  function Eval: Integer;
end;


TDiceParser = class
private
  FExpression: string;
  FRootNode: TParserNode;
  FParseError: Boolean;
  FCurChar: Char;
  FValue: Integer;
  procedure GetNextChar;
  function ParseL3: TParserNode; // +-
  function ParseL2: TParserNode; // */
  function ParseL1: TParserNode; // d
  function ParseL0: TParserNode; // literals
public
  constructor Create;
  destructor Destroy; override;
  function TryParse(Expression: string; var value: Integer): Boolean;
  function Parse(Expression: string): Integer;
end;

TWeightedString = class
  Text: string;
  Weight: Double;
  function ToFullStr: string;
end;

TTextGenerator = class
private
  FRNG: TCustomRNG;
protected
  function Rnd: Double; overload;
  function Rnd(val: Cardinal): Cardinal; overload;
  function RndGauss: Double;
public
  function GetText: string; virtual; abstract;
  procedure LoadFromXMLNode(node: TDOMNode); virtual; abstract;
  function SaveToXMLNode(doc: TXMLDocument): TDOMNode; virtual; abstract;
  property RNG: TCustomRNG read FRNG write FRNG;
end;

TIntTextGenerator = class(TTextGenerator)
private
  FMinVal, FMaxVal: Integer;

public
  constructor Create;
  function GetText: string; override;
  procedure LoadFromXMLNode(node: TDOMNode); override;
  function SaveToXMLNode(doc: TXMLDocument): TDOMNode; override;
  property MinVal: Integer read FMinVal write FMinVal;
  property MaxVal: Integer read FMaxVal write FMaxVal;
end;

TFloatTextGenerator = class(TTextGenerator)
private
  FMinVal,
  FMaxVal: Double;
  FDigits: Integer;
public
  constructor Create;
  function GetText: string; override;  
  procedure LoadFromXMLNode(node: TDOMNode); override;
  function SaveToXMLNode(doc: TXMLDocument): TDOMNode; override;
  property MinVal: Double read FMinVal write FMinVal;
  property MaxVal: Double read FMaxVal write FMaxVal;
  property Digits: Integer read FDigits write FDigits;
end;

TNormalFloatTextGenerator = class(TFloatTextGenerator)
private
  FMean, FStdDev: Double;
public
  constructor Create;
  function GetText: string; override;
  procedure LoadFromXMLNode(node: TDOMNode); override;
  function SaveToXMLNode(doc: TXMLDocument): TDOMNode; override;
  property Mean: Double read FMean write FMean;
  property StdDev: Double read FStdDev write FStdDev;
end;

TTextFromListGenerator = class(TTextGenerator)
private
  FWeightedList: TList;
  function SelectWeightedEntry: string;

public
  constructor Create;
  destructor Destroy; override;
  procedure Clear;
  procedure AddEntry(text: string; weight: Double = 1);
  procedure LoadFromFile(FileName: string);
  function GetText: string; override;  
  procedure LoadFromXMLNode(node: TDOMNode); override;
  function SaveToXMLNode(doc: TXMLDocument): TDOMNode; override;
  function GetDistinctText(Amount: Integer): TStringList;
end;

TMarkovTextGenerator = class(TTextGenerator)
private
  FStartTokens,
  FFollowTokens: TStringList;
  FMaxTokens: Integer;
  FSeparator: Char;
public
  constructor Create;
  destructor Destroy; override;
  procedure Clear;
  procedure LoadFromFile(Filename: string);
  function GetText: string; override;  
  procedure LoadFromXMLNode(node: TDOMNode); override;
  function SaveToXMLNode(doc: TXMLDocument): TDOMNode; override;
  // Stop generation at the next possible end token after this many tokens have been added
  property MaxTokens: Integer read FMaxTokens write FMaxTokens;
  property Separator: Char read FSeparator write FSeparator;
end;


implementation

uses
  Math;

{ TCustomRNG }

function TCustomRNG.Random(val: Cardinal): Cardinal;
begin
  Result := NextValue mod val;
end;

function TCustomRNG.Random: Double;
begin
  Result := NextValue / $FFFFFFFF;
end;

function TCustomRNG.RandomGauss: Double;
var v1, v2, s, mult: Double;
begin
  if HasNextGauss then
  begin
    HasNextGauss := False;
    Exit(NextGauss);
  end;

  // Polar-Methode
  repeat
    v1 := 2 * self.Random - 1;
    v2 := 2 * self.Random - 1;
    s := Sqr(v1) + Sqr(v2);
  until InRange(s, 0, 1);

  mult := Sqrt(-2 * Lnxp1(s - 1) / s);
  Result := v1 * mult;
  NextGauss := v2 * mult;
  HasNextGauss := True;
end;

{ TMRandom }

constructor TMRandom.Create(pSeed: Cardinal = 123456789);
begin
  Seed := pSeed;
end;

procedure TMRandom.SetSeed(pSeed: Cardinal);
begin
  x := pSeed;
  y := 362436000;
  z := 521288629;
  c := 7654321;
end;

function TMRandom.NextValue: Cardinal;
var
  t: Int64;
begin
  // Linear Congruential Generator
  x := 69069 * x + 12345;

  // XOR-Shift
  y := y xor (y shl 13);
  y := y xor (y shr 17);
  y := y xor (y shl 5);

  // Multiply-with-Carry
  t := 698769069 * Int64(z) + Int64(c);
  c := t shr 32;
  z := t and $FFFFFFFF;

  Result := x + y + z;
end;

{ TParserNode }

constructor TParserNode.Create(op: Char; Left: TParserNode);
begin
  NodeType := ntBinary;
  Operation := op;
  LeftChild := Left;
  RightChild := nil;
end;

destructor TParserNode.Destroy;
begin
  if Assigned(LeftChild) then
    LeftChild.Free;
  if Assigned(RightChild) then
    RightChild.Free;
  inherited;
end;

function TParserNode.Eval: Integer;
var
  i, dAmt, dSize: Integer;
begin
  Result := 0;
  case NodeType of
    ntValue: Result := Value;
    ntError: Result := 0;
    ntBinary:
      begin
        if Assigned(LeftChild) and Assigned(RightChild) then
        begin
          case Operation of
            '+':
              Result := LeftChild.Eval + RightChild.Eval;
            '-':
              Result := LeftChild.Eval - RightChild.Eval;
            '*':
              Result := LeftChild.Eval * RightChild.Eval;
            '/':
              Result := LeftChild.Eval div RightChild.Eval;
            'd', 'D':
              begin
                Result := 0;
                dAmt := LeftChild.Eval;
                dSize := RightChild.Eval;
                for i := 0 to dAmt - 1 do
                  Result := Result + Random(dSize) + 1;
              end;
          end;
        end;
      end;
  end;
end;

{ TDiceParser }

constructor TDiceParser.Create;
begin
  FCurChar := #0;
  FExpression := '';
  FValue := 0;
  FRootNode := nil;
end;

destructor TDiceParser.Destroy;
begin
  if Assigned(FRootNode) then
    FRootNode.Free;
  inherited;
end;

function TDiceParser.TryParse(Expression: string; var value: Integer): Boolean;
var val: Integer;
begin
  FExpression := Expression;
  GetNextChar;
  FRootNode := ParseL3;
  Result := not ((FRootNode.NodeType = ntError) or not (FCurChar = #0));
  if not Result then
    Exit;
  FParseError := False;
  val := FRootNode.Eval;
  Result := not FParseError;
  if not Result then
    Exit;
  FValue := val;
  value := val;
end;

function TDiceParser.Parse(Expression: string): Integer;
begin
  TryParse(Expression, FValue);
  Result := FValue;
end;

function TDiceParser.ParseL3: TParserNode;
var
  n1, n2: TParserNode;
  cont: Boolean;
begin
  n1 := ParseL2;
  if n1.NodeType <> ntError then
  begin
    cont := True;
    while cont and not (FCurChar = #0) do
    begin
      while (FCurChar = ' ') do
        GetNextChar;
      if FCurChar in ['+', '-'] then
      begin
        n1 := TParserNode.Create(FCurChar, n1);
        GetNextChar;
        n2 := ParseL2;
        if n2.NodeType = ntError then
        begin
          n1.Free;
          n1 := n2;
          cont := False;
        end
        else
          n1.RightChild := n2;
      end
      else
        cont := False;
    end;
  end;
  Result := n1;
end;

function TDiceParser.ParseL2: TParserNode;
var
  n1, n2: TParserNode;
  cont: Boolean;
begin
  n1 := ParseL1;
  if n1.NodeType <> ntError then
  begin
    cont := True;
    while cont and not (FCurChar = #0) do
    begin
      while (FCurChar = ' ') do
        GetNextChar;
      if FCurChar in ['*', '/'] then
      begin
        n1 := TParserNode.Create(FCurChar, n1);
        GetNextChar;
        n2 := ParseL1;
        if n2.NodeType = ntError then
        begin
          n1.Free;
          n1 := n2;
          cont := False;
        end
        else
          n1.RightChild := n2;
      end
      else
        cont := False;
    end;
  end;
  Result := n1;
end;

function TDiceParser.ParseL1: TParserNode;
var
  n1, n2: TParserNode;
  cont: Boolean;
begin
  n1 := ParseL0;
  if n1.NodeType = ntError then
  begin
    // We want to allow constructions like "d10" without the first number
    n1.NodeType := ntValue;
    n1.Value := 1;
  end;

  begin
    cont := True;
    while cont and not (FCurChar = #0) do
    begin
      while (FCurChar = ' ') do
        GetNextChar;
      if FCurChar in ['d', 'D'] then
      begin
        n1 := TParserNode.Create(FCurChar, n1);
        GetNextChar;
        n2 := ParseL0;
        if n2.NodeType = ntError then
        begin
          n1.Free;
          n1 := n2;
          cont := False;
        end
        else
          n1.RightChild := n2;
      end
      else
        cont := False;
    end;
  end;
  Result := n1;
end;

function TDiceParser.ParseL0: TParserNode;
var
  n1: TParserNode;
  val: Integer;
  neg: Boolean;
begin
  n1 := nil;
  while (FCurChar = ' ') do
    GetNextChar;
  if FCurChar = '(' then
  begin
    GetNextChar;
    n1 := ParseL3;
    if n1.NodeType <> ntError then
    begin
      while (FCurChar = ' ') do
        GetNextChar;
      if FCurChar = ')' then
        GetNextChar
      else
      begin
        n1.Free;
        n1 := TParserNode.Create(#0, nil);
        n1.NodeType := ntError;
      end;
    end;
  end
  else if FCurChar in ['0'..'9', '+', '-'] then
  begin
    neg := FCurChar = '-';
    while FCurChar in ['+', '-'] do
      GetNextChar;

    val := 0;
    if FCurChar in ['0'..'9'] then
    begin
      while FCurChar in ['0'..'9'] do
      begin
        val := 10 * val + (Ord(FCurChar) - Ord('0'));
        GetNextChar;
      end;
      n1 := TParserNode.Create(#0, nil);
      n1.NodeType := ntValue;
      n1.Value := val * IfThen(neg, -1, 1);
    end;
  end;
  if not Assigned(n1) then
  begin
    n1 := TParserNode.Create(#0, nil);
    n1.NodeType := ntError;
  end;
  Result := n1;
end;

procedure TDiceParser.GetNextChar;
begin
  if FExpression = '' then
  begin
    FCurChar := #0;
    Exit;
  end;
  FCurChar := FExpression[1];
  Delete(FExpression, 1, 1);
end;

{ TWeightedString }

function TWeightedString.ToFullStr: string;
begin
  Result := Text + '^' + FloatToStrF(Weight, ffFixed, 8, 8);
end;

{ TTextGenerator }

function TTextGenerator.Rnd: Double;
begin
  if Assigned(FRNG) then
    Result := FRNG.Random
  else
    Result := Random;
end;

function TTextGenerator.Rnd(val: Cardinal): Cardinal;
begin
  if val = 0 then
    Exit(0);
  if Assigned(FRNG) then
    Result := FRNG.Random(val)
  else
    Result := Random(val);
end;

function TTextGenerator.RndGauss: Double;
begin
  if Assigned(FRNG) then
    Result := FRNG.RandomGauss
  else
    Result := (Random + Random + Random) * 2 / 3 - 1; // Poor approximation, but we will probably not use this anyway...
end;


{ TIntTextGenerator }

constructor TIntTextGenerator.Create;
begin
  FMinVal := 0;
  FMaxVal := 1;
end;

function TIntTextGenerator.GetText: string;
begin
  Result := IntToStr(FMinVal + Rnd(FMaxVal - FMinVal + 1));
end;

procedure TIntTextGenerator.LoadFromXMLNode(node: TDOMNode);
var i: Integer;
begin
  if not SameText(node.nodeName, 'IntTextGen') then
    Exit;
  for i := 0 to node.Attributes.Length - 1 do
  begin
    if SameText(node.Attributes[i].NodeName, 'MinVal') then
      FMinVal := StrToIntDef(node.Attributes[i].NodeValue, 0)
    else if SameText(node.Attributes[i].NodeName, 'MaxVal') then
      FMaxVal := StrToIntDef(node.Attributes[i].NodeValue, 1);
  end;
end;

function TIntTextGenerator.SaveToXMLNode(doc: TXMLDocument): TDOMNode;
begin
  Result := doc.CreateElement('IntTextGen');
  TDOMElement(Result).SetAttribute('MinVal', IntToStr(FMinVal));
  TDOMElement(Result).SetAttribute('MaxVal', IntToStr(FMaxVal));
end;

{ TFloatTextGenerator }

constructor TFloatTextGenerator.Create;
begin
  FMinVal := 0;
  FMaxVal := 1;
  FDigits := 2;
end;

function TFloatTextGenerator.GetText: string;
begin
  Result := FloatToStrF(FMinVal + Rnd * (FMaxVal - FMinVal), ffFixed, FDigits, FDigits);
end;

procedure TFloatTextGenerator.LoadFromXMLNode(node: TDOMNode);  
var i: Integer;
begin
  if not SameText(node.nodeName, 'FloatTextGen') then
    Exit;
  for i := 0 to node.Attributes.Length - 1 do
  begin
    if SameText(node.Attributes[i].NodeName, 'MinVal') then
      FMinVal := StrToFloatDef(node.Attributes[i].NodeValue, 0)
    else if SameText(node.Attributes[i].NodeName, 'MaxVal') then
      FMaxVal := StrToFloatDef(node.Attributes[i].NodeValue, 1)
    else if SameText(node.Attributes[i].NodeName, 'Digits') then
      FDigits := StrToIntDef(node.Attributes[i].NodeValue, 2);
  end;
end;

function TFloatTextGenerator.SaveToXMLNode(doc: TXMLDocument): TDOMNode;
begin
  Result := doc.CreateElement('FloatTextGen');
  TDOMElement(Result).SetAttribute('MinVal', FloatToStrF(FMinVal, ffFixed, 8, 8));
  TDOMElement(Result).SetAttribute('MaxVal', FloatToStrF(FMaxVal, ffFixed, 8, 8));
  TDOMElement(Result).SetAttribute('Digits', IntToStr(FDigits));
end;

{ TNormalFloatTextGenerator }
//  FMean, FStdDev: Double;

constructor TNormalFloatTextGenerator.Create;
begin
  FMinVal := -MAXDOUBLE;
  FMaxVal := MAXDOUBLE;
  FDigits := 2;
  FMean := 0;
  FStdDev := 1;
end;

function TNormalFloatTextGenerator.GetText: string;
begin
  Result := FloatToStrF(EnsureRange(FStdDev * RndGauss + FMean, FMinVal, FMaxVal), ffFixed, FDigits, FDigits);
end;
 
procedure TNormalFloatTextGenerator.LoadFromXMLNode(node: TDOMNode);
var i: Integer;
begin
  if not SameText(node.nodeName, 'NormalFloatTextGen') then
    Exit;
  for i := 0 to node.Attributes.Length - 1 do
  begin
    if SameText(node.Attributes[i].NodeName, 'MinVal') then
      FMinVal := StrToFloatDef(node.Attributes[i].NodeValue, 0)
    else if SameText(node.Attributes[i].NodeName, 'MaxVal') then
      FMaxVal := StrToFloatDef(node.Attributes[i].NodeValue, 1)
    else if SameText(node.Attributes[i].NodeName, 'Digits') then
      FDigits := StrToIntDef(node.Attributes[i].NodeValue, 2)
    else if SameText(node.Attributes[i].NodeName, 'Mean') then
      FMean := StrToFloatDef(node.Attributes[i].NodeValue, 0)
    else if SameText(node.Attributes[i].NodeName, 'StdDev') then
      FStdDev := StrToFloatDef(node.Attributes[i].NodeValue, 1);
  end;
end;

function TNormalFloatTextGenerator.SaveToXMLNode(doc: TXMLDocument): TDOMNode;
begin
  Result := doc.CreateElement('NormalFloatTextGen');
  TDOMElement(Result).SetAttribute('MinVal', FloatToStrF(FMinVal, ffFixed, 8, 8));
  TDOMElement(Result).SetAttribute('MaxVal', FloatToStrF(FMaxVal, ffFixed, 8, 8));
  TDOMElement(Result).SetAttribute('Digits', IntToStr(FDigits));
  TDOMElement(Result).SetAttribute('Mean', FloatToStrF(FMean, ffFixed, 8, 8));
  TDOMElement(Result).SetAttribute('StdDev', FloatToStrF(FStdDev, ffFixed, 8, 8));
end;

{ TTextFromListGenerator }

constructor TTextFromListGenerator.Create;
begin
  FWeightedList := TList.Create;
end;

destructor TTextFromListGenerator.Destroy;
begin
  Clear;
  FWeightedList.Free;
  inherited;
end;

procedure TTextFromListGenerator.Clear;
var
  tmpstr: TWeightedString;
begin
  while FWeightedList.Count > 0 do
  begin
    tmpStr := TWeightedString(FWeightedList[0]);
    FWeightedList.Delete(0);
    tmpStr.Free;
  end;
end;

function TTextFromListGenerator.SelectWeightedEntry: string;
var
  WeightsTotal, CurWeight, Selected: Double;
  i: Integer;
begin
  WeightsTotal := 0;
  for i := 0 to FWeightedList.Count - 1 do
    WeightsTotal := WeightsTotal + TWeightedString(FWeightedList[i]).Weight;
  if WeightsTotal = 0 then
    Exit('');
  CurWeight := 0;
  Selected := Rnd * WeightsTotal;
  for i := 0 to FWeightedList.Count - 1 do
  begin
    if InRange(Selected, CurWeight, CurWeight + TWeightedString(FWeightedList[i]).Weight) then
      Exit(TWeightedString(FWeightedList[i]).Text);
    CurWeight := CurWeight + TWeightedString(FWeightedList[i]).Weight;
  end;
end;

procedure TTextFromListGenerator.AddEntry(text: string; weight: Double = 1);
var
  tmpstr: TWeightedString;
begin
  tmpStr := TWeightedString.Create;
  tmpStr.Text := text;
  tmpStr.Weight := weight;
  FWeightedList.Add(tmpStr);
end;

procedure TTextFromListGenerator.LoadFromFile(FileName: string);
var
  FileList, SplitList: TStringList;
  i: Integer;
  CurWeight: Double;
begin
  if not FileExists(FileName) then
    Exit;
  FileList := TStringList.Create;
  SplitList := TStringList.Create;
  SplitList.Delimiter := '^';
  SplitList.StrictDelimiter := True;

  try
    FileList.LoadFromFile(FileName);
    for i := 0 to FileList.Count - 1 do
    begin
      SplitList.DelimitedText := FileList[i];
      if SplitList.Count = 0 then
        Continue;
      CurWeight := 1;
      if SplitList.Count > 1 then
        CurWeight := StrToFloatDef(SplitList[1], 1);
      AddEntry(SplitList[0], CurWeight);
    end;
  finally
    FileList.Free;
    SplitList.Free;
  end;
end;

function TTextFromListGenerator.GetText: string;
begin
  Result := SelectWeightedEntry;
end;

function TTextFromListGenerator.GetDistinctText(Amount: Integer): TStringList;
var
  i: Integer;
  tmpStr: string;
begin
  Result := TStringList.Create;
  if Amount >= FWeightedList.Count then
  begin
    for i := 0 to FWeightedList.Count - 1 do
      Result.Add(TWeightedString(FWeightedList[i]).Text);
    Exit;
  end;
  for i := 0 to Amount - 1 do
  begin
    repeat
      tmpStr := SelectWeightedEntry;
    until Result.IndexOf(tmpStr) < 0;
    Result.Add(tmpStr);
  end;
end;
       
procedure TTextFromListGenerator.LoadFromXMLNode(node: TDOMNode);
var
  SplitList: TStringList; 
  CurWeight: Double; 
  EntryNode: TDOMNode;
begin
  if not SameText(node.nodeName, 'TextFromListGen') then
    Exit;

  Clear;

  SplitList := TStringList.Create;
  SplitList.Delimiter := '^';
  SplitList.StrictDelimiter := True;

  try
    EntryNode := node.FirstChild;
    while Assigned(EntryNode) do
    begin
      SplitList.DelimitedText := EntryNode.FirstChild.TextContent;
      if SplitList.Count = 0 then
        Continue;
      CurWeight := 1;
      if SplitList.Count > 1 then
        CurWeight := StrToFloatDef(SplitList[1], 1);
      AddEntry(SplitList[0], CurWeight);
      EntryNode := EntryNode.NextSibling;
    end;
  finally
    SplitList.Free;
  end;

end;

function TTextFromListGenerator.SaveToXMLNode(doc: TXMLDocument): TDOMNode;
var
  i: Integer;
  EntryNode: TDOMNode;
begin
  Result := doc.CreateElement('TextFromListGen');
  for i := 0 to FWeightedList.Count - 1 do
  begin
    EntryNode := Result.AppendChild(doc.CreateElement('Entry'));
    EntryNode.AppendChild(doc.CreateTextNode(TWeightedString(FWeightedList[i]).ToFullStr));
  end;
end;

{ TMarvokTextGenerator }

constructor TMarkovTextGenerator.Create;
begin
  FStartTokens := TStringList.Create;
  FStartTokens.Sorted := True;        
  FStartTokens.Duplicates := dupIgnore;
  FFollowTokens := TStringList.Create;
  FFollowTokens.Sorted := True;
  FFollowTokens.Duplicates := dupIgnore;
  FMaxTokens := MAXINT;
  FSeparator := '-';
end;

destructor TMarkovTextGenerator.Destroy;
begin  
  Clear;
  FStartTokens.Free;
  FFollowTokens.Free;
  inherited;
end;

procedure TMarkovTextGenerator.Clear;   
var i: Integer;
begin
  for i := 0 to FStartTokens.Count - 1 do
    if Assigned(FStartTokens.Objects[i]) then
      TStringList(FStartTokens.Objects[i]).Free;
  FStartTokens.Clear;
  for i := 0 to FFollowTokens.Count - 1 do
    if Assigned(FFollowTokens.Objects[i]) then
      TStringList(FFollowTokens.Objects[i]).Free;
  FFollowTokens.Clear;
end;

procedure TMarkovTextGenerator.LoadFromFile(Filename: string);
var
  FileList, SplitList: TStringList;
  i, j, CurIdx: Integer;
  NextToken: string;
begin
  if not FileExists(Filename) then
    Exit;
  FileList := TStringList.Create;
  SplitList := TStringList.Create;
  SplitList.Delimiter := FSeparator;
  SplitList.StrictDelimiter := True;
  
  Clear;

  try
    FileList.LoadFromFile(Filename);
    for i := 0 to FileList.Count - 1 do
    begin
      SplitList.DelimitedText := FileList[i];
      if SplitList.Count >= 2 then
      begin
        CurIdx := FStartTokens.Add(SplitList[0]);
        if not Assigned(FStartTokens.Objects[CurIdx]) then
        begin
          FStartTokens.Objects[CurIdx] := TStringList.Create;
          TStringList(FStartTokens.Objects[CurIdx]).Sorted := True;
          TStringList(FStartTokens.Objects[CurIdx]).Duplicates := dupIgnore;
        end;
        TStringList(FStartTokens.Objects[CurIdx]).Add(SplitList[1]);

        for j := 1 to SplitList.Count - 1 do
        begin
          NextToken := '';
          if j < SplitList.Count - 1 then
            NextToken := SplitList[j + 1];
          CurIdx := FFollowTokens.Add(SplitList[j]);
          if not Assigned(FFollowTokens.Objects[CurIdx]) then
          begin
            FFollowTokens.Objects[CurIdx] := TStringList.Create;
            TStringList(FFollowTokens.Objects[CurIdx]).Sorted := True;
            TStringList(FFollowTokens.Objects[CurIdx]).Duplicates := dupIgnore;
            // TODO: Filter manually, accept only empty strings
          end;
          //if (NextToken = '') or (TStringList(FFollowTokens.Objects[CurIdx]).IndexOf(NextToken) < 0) then
            TStringList(FFollowTokens.Objects[CurIdx]).Add(NextToken);

        end;
      end;
    end;
  finally
    FileList.Free;
    SplitList.Free;
  end;
end;

procedure TMarkovTextGenerator.LoadFromXMLNode(node: TDOMNode);
{var
SplitList: TStringList;
CurWeight: Double;
EntryNode: TDOMNode;}
begin
  if not SameText(node.nodeName, 'MarkovTextGen') then
    Exit;

  Clear;


  {
  SplitList := TStringList.Create;
  SplitList.Delimiter := '^';
  SplitList.StrictDelimiter := True;

  try
  EntryNode := node.FirstChild;
  while Assigned(EntryNode) do
  begin
    SplitList.DelimitedText := EntryNode.FirstChild.TextContent;
    if SplitList.Count = 0 then
      Continue;
    CurWeight := 1;
    if SplitList.Count > 1 then
      CurWeight := StrToFloatDef(SplitList[1], 1);
    AddEntry(SplitList[0], CurWeight);
    EntryNode := EntryNode.NextSibling;
  end;
  finally
  SplitList.Free;
  end;    }

end;

function TMarkovTextGenerator.SaveToXMLNode(doc: TXMLDocument): TDOMNode;
var
  ListNode, EntryNode, SubNode: TDOMNode;
  SubList: TStringList;
  i, j: Integer;
begin
  Result := doc.CreateElement('TextFromListGen');
  TDOMElement(Result).SetAttribute('MaxTokens', IntToStr(FMaxTokens));
  TDOMElement(Result).SetAttribute('Separator', FSeparator);
  ListNode := Result.AppendChild(doc.CreateElement('StartTokens'));
  for i := 0 to FStartTokens.Count - 1 do
  begin
    EntryNode := ListNode.AppendChild(doc.CreateElement('Entry'));
    TDOMElement(EntryNode).SetAttribute('Name', FStartTokens[i]);

    SubList := TStringList(FStartTokens.Objects[i]);
    for j := 0 to SubList.Count - 1 do
    begin
      SubNode := EntryNode.AppendChild(doc.CreateElement('Follow'));
      SubNode.AppendChild(doc.CreateTextNode(SubList[j]));
    end;
  end;
  ListNode := Result.AppendChild(doc.CreateElement('FollowTokens'));
  for i := 0 to FFollowTokens.Count - 1 do
  begin             
    EntryNode := ListNode.AppendChild(doc.CreateElement('Entry'));
    //EntryNode.AppendChild(doc.CreateTextNode(FFollowTokens[i]));
    TDOMElement(EntryNode).SetAttribute('Name', FFollowTokens[i]);

    SubList := TStringList(FFollowTokens.Objects[i]);
    for j := 0 to SubList.Count - 1 do
    begin
      SubNode := EntryNode.AppendChild(doc.CreateElement('Follow'));
      SubNode.AppendChild(doc.CreateTextNode(SubList[j]));
    end;
  end;
end;

function TMarkovTextGenerator.GetText: string;
var
  CurToken, NextToken: string;
  i, TokenCnt: Integer;
  CanStop: Boolean;
begin
  Result := '';
  if FStartTokens.Count = 0 then
    Exit;
  i := Rnd(FStartTokens.Count);
  CurToken := FStartTokens[i];
  NextToken := TStringList(FStartTokens.Objects[i])[Rnd(TStringList(FStartTokens.Objects[i]).Count)];
  Result := CurToken;
  CurToken := NextToken;
  TokenCnt := 1;
  repeat

    i := FFollowTokens.IndexOf(CurToken);
    NextToken := TStringList(FFollowTokens.Objects[i])[Rnd(TStringList(FFollowTokens.Objects[i]).Count)];
    CanStop := TStringList(FFollowTokens.Objects[i]).IndexOf('') >= 0;
    Result := Result + CurToken;
    CurToken := NextToken;
    Inc(TokenCnt);
  until (NextToken = '') or ((TokenCnt >= FMaxTokens) and CanStop);
end;


end.

