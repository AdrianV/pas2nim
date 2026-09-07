program UseCounter;
uses Counter;
var c: TCounter;
begin
  c := TCounter.Create;
  c.Inc1;
  c.Inc1;
  write('value=', c.Value, ' hits=', GlobalHits);
end.
