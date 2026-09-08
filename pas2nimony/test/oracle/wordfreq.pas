program WordfreqOracle;
{ M6-2 "food": a real-world-shaped program - generate a corpus,
  analyze word frequencies, report. Exercises classes, records,
  sets, TStringList, file I/O, exceptions, width formatting. }
uses SysUtils, Classes, StrUtils;
type
  TWordKind = (wkShort, wkMedium, wkLong);

  TEntry = record
    Word1: string;
    Count: Integer;
    First: Char;
  end;

  TFreqTable = class
  private
    FEntries: array[1..64] of TEntry;
    FUsed: Integer;
    function FindEntry(const w: string): Integer;
  public
    constructor Create;
    procedure Add(const w: string);
    function Count(const w: string): Integer;
    function Total: Integer;
    function Distinct: Integer;
    procedure GetAt(idx: Integer; var e: TEntry);
  end;

  TAnalyzer = class
  private
    FTable: TFreqTable;
  public
    constructor Create;
    destructor Destroy; override;
    procedure Feed(const line: string);
    procedure Report(const title: string);
  end;

  EBadLine = class(Exception);

constructor TFreqTable.Create;
begin
  inherited Create;
  FUsed := 0;
end;

function TFreqTable.FindEntry(const w: string): Integer;
var
  k: Integer;
begin
  result := 0;
  for k := 1 to FUsed do
    if SameText(FEntries[k].Word1, w) then
    begin
      result := k;
      break;
    end;
end;

procedure TFreqTable.Add(const w: string);
var
  k: Integer;
begin
  if FUsed >= 64 then
    raise EBadLine.Create('table full');
  k := FindEntry(w);
  if k = 0 then
  begin
    Inc(FUsed);
    k := FUsed;
    FEntries[k].Word1 := w;
    FEntries[k].First := w[1];
  end;
  Inc(FEntries[k].Count);
end;

function TFreqTable.Count(const w: string): Integer;
var
  k: Integer;
begin
  k := FindEntry(w);
  if k = 0 then result := 0 else result := FEntries[k].Count;
end;

function TFreqTable.Total: Integer;
var
  k, s: Integer;
begin
  s := 0;
  for k := 1 to FUsed do
    s := s + FEntries[k].Count;
  result := s;
end;

function TFreqTable.Distinct: Integer;
begin
  result := FUsed;
end;

procedure TFreqTable.GetAt(idx: Integer; var e: TEntry);
begin
  e := FEntries[idx];
end;

constructor TAnalyzer.Create;
begin
  inherited Create;
  FTable := TFreqTable.Create;
end;

destructor TAnalyzer.Destroy;
begin
  FTable.Free;
  inherited Destroy;
end;

procedure TAnalyzer.Feed(const line: string);
var
  i, start1: Integer;
  w, clean: string;
  ok: Boolean;
begin
  i := 1;
  while i <= Length(line) do
  begin
    ok := false;
    clean := '';
    start1 := i;
    while (i <= Length(line)) and (UpCase(line[i]) in ['A'..'Z']) do
    begin
      clean := clean + UpCase(line[i]);
      Inc(i);
    end;
    if clean <> '' then
    begin
      if Length(clean) <= 3 then ok := true else ok := true;
      if not ok then
        raise EBadLine.Create('bad token: ' + clean);
      FTable.Add(clean);
    end
    else
      Inc(i);
  end;
end;

procedure TAnalyzer.Report(const title: string);
var
  k: Integer;
  e: TEntry;
  kind: TWordKind;
begin
  writeln('== ', title, ' ==');
  writeln('total=', FTable.Total, ' distinct=', FTable.Distinct);
  for k := 1 to FTable.Distinct do
  begin
    FTable.GetAt(k, e);
    case Length(e.Word1) of
      1..3: kind := wkShort;
      4..6: kind := wkMedium;
    else
      kind := wkLong;
    end;
    writeln(e.Word1:12, ' ', e.Count:4, '  ', e.First, ' ', Ord(kind));
  end;
end;

var
  lines, sorted: TStringList;
  an: TAnalyzer;
  k, total1: Integer;
  ratio: Double;
begin
  lines := TStringList.Create;
  lines.Add('the quick brown fox jumps over the lazy dog');
  lines.Add('the dog barks and the fox runs away');
  lines.Add('a fox is quick and a dog is loyal');
  lines.SaveToFile('corpus.txt');
  sorted := TStringList.Create;
  sorted.LoadFromFile('corpus.txt');
  writeln('lines=', sorted.Count);
  an := TAnalyzer.Create;
  try
    for k := 0 to sorted.Count - 1 do
      an.Feed(sorted[k]);
    an.Report('corpus');
    total1 := an.FTable.Total;
    ratio := total1 / 10;
    writeln('ratio=', FormatFloat('0.0', ratio));
    writeln('pos5=', PosEx('fox', UpperCase(sorted[0]), 5));
    writeln('rev=', ReverseString('abc'));
    writeln('dup=', DupeString('ab', 3));
    writeln('rep=', AnsiReplaceText('aFoxB', 'fox', 'cat'));
  finally
    an.Free;
    sorted.Free;
    lines.Free;
  end;
end.
