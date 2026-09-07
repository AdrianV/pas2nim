program CastTest;
{ M4 runtime-checked casts: `is` lowers to a nil-guarded `of` check
  (nimony's `of` answers true for nil, but Delphi's `nil is T` is
  false), `as` lowers to systempas.pasAs[T] - nil stays nil and a
  failed check yields nil (documented divergence: Delphi raises
  EInvalidCast, the raising variant would mark every transitive
  caller as a raising routine) }

type
  TAnimal = class
  public
    Name: String;
    procedure Speak; virtual;
  end;
  TDog = class(TAnimal)
  public
    procedure Fetch;
  end;
  TCat = class(TAnimal)
  end;

procedure TAnimal.Speak;
begin
  writeln('speak');
end;

procedure TDog.Fetch;
begin
  writeln('fetch');
end;

var
  A: TAnimal;
  D: TDog;
begin
  A := TDog.Create;
  if A is TDog then writeln('is-dog');
  if A is TCat then writeln('is-cat-BUG') else writeln('not-cat');
  D := A as TDog;
  D.Fetch;
  A := nil;
  if A is TDog then writeln('nil-is-BUG') else writeln('nil-not-dog');
  D := A as TDog;
  if D = nil then writeln('nil-as-nil');
end.