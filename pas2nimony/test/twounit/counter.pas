unit Counter;
interface
type
  TCounter = class
  private
    FCount: Integer;
  public
    constructor Create;
    procedure Inc1;
    function Value: Integer;
  end;
var
  GlobalHits: Integer;
implementation
constructor TCounter.Create;
begin
  FCount := 0;
end;
procedure TCounter.Inc1;
begin
  FCount := FCount + 1;
  GlobalHits := GlobalHits + 1;
end;
function TCounter.Value: Integer;
begin
  result := FCount;
end;
initialization
  GlobalHits := 100;
end.
