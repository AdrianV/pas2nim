program OpTest;
{ M4 operator overloading: `class operator Add(a, b: T): T` lowers to
  a nimony operator proc (`+`, `==`, ... - nifler-style quoted names
  in NIF, backticked in .nim); call sites stay plain infix/prefix
  expressions and nimony resolves the overloads. Records gained
  visibility sections; record-returning procs get a
  `result = default(T)` preamble because nimony's result-init proof
  rejects bodies that only assign result fields. v1 covers arithmetic,
  bitwise, comparison and unary operators; Inc/Dec, Explicit/Implicit
  and class-qualified explicit calls are rejected. }

type
  TPoint = record
  public
    X, Y: Integer;
    class operator Add(a, b: TPoint): TPoint;
    class operator Equal(a, b: TPoint): Boolean;
    class operator Multiply(a: TPoint; K: Integer): TPoint;
    class operator Negative(a: TPoint): TPoint;
  end;

class operator TPoint.Add(a, b: TPoint): TPoint;
begin
  Result.X := a.X + b.X;
  Result.Y := a.Y + b.Y;
end;

class operator TPoint.Equal(a, b: TPoint): Boolean;
begin
  Result := (a.X = b.X) and (a.Y = b.Y);
end;

class operator TPoint.Multiply(a: TPoint; K: Integer): TPoint;
begin
  Result.X := a.X * K;
  Result.Y := a.Y * K;
end;

class operator TPoint.Negative(a: TPoint): TPoint;
begin
  Result.X := -a.X;
  Result.Y := -a.Y;
end;

var
  A, B, C: TPoint;
begin
  A.X := 1; A.Y := 2;
  B.X := 3; B.Y := 4;
  C := A + B;
  writeln('c.x=', C.X, ' c.y=', C.Y);
  C := A * 5;
  writeln('mul.x=', C.X);
  C := -A;
  writeln('neg.x=', C.X);
  if A = B then writeln('eq') else writeln('ne');
  if A = A then writeln('self-eq');
end.