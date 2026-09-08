program ClassMethTest;
{ M4 class methods and class vars: `class procedure`/`class function`
  lower to module-level procs named pasCm_<Class>_<Name> (static, no
  self); `class var` hoists to a module-level var named
  pasCv_<Class>_<Name> (zero-initialized like Delphi). Calls resolve
  from the class name, from an instance, and bare inside the class's
  own routines. v1 gaps: no `class virtual`, no Self (class reference)
  inside class methods, class constants/properties rejected. }

type
  TMath = class
  public
    class var FCount: Integer;
    class function Double(X: Integer): Integer;
    class procedure Bump;
  end;

class function TMath.Double(X: Integer): Integer;
begin
  Result := X * 2;
end;

class procedure TMath.Bump;
begin
  Inc(FCount);
end;

var
  M: TMath;
begin
  writeln('d=', TMath.Double(21));
  TMath.Bump;
  TMath.Bump;
  M := TMath.Create;
  M.Bump;
  writeln('count=', TMath.FCount);
  TMath.FCount := 10;
  writeln('final=', TMath.FCount);
end.