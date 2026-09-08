program StringListOracle;
{ M5 oracle: the TStringList surface both worlds share }
uses SysUtils, Classes;
var
  L: TStringList;
  i: Integer;
begin
  L := TStringList.Create;
  L.Add('beta');
  L.Add('alpha');
  L.Add('gamma');
  writeln('count=', L.Count, ' first=', L[0]);
  L[0] := 'BETA';
  writeln('set0=', L[0]);
  writeln('idxof=', L.IndexOf('gamma'), ' idxmiss=', L.IndexOf('zz'));
  L.Sort;
  writeln('sorted=', L[0], ' ', L[1], ' ', L[2]);
  L.Insert(1, 'inserted');
  writeln('ins=', L[1], ' ', L[2]);
  L.Delete(0);
  writeln('del=', L[0], ' count=', L.Count);
  writeln('textlen=', Length(L.Text));
  L.SaveToFile('orcl.txt');
  L.Clear;
  writeln('cleared=', L.Count);
  L.LoadFromFile('orcl.txt');
  writeln('reloaded=', L.Count, ' v0=', L[0]);
  L.Add('alpha');
  writeln('dupe-count=', L.Count);
end.
