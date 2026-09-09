program HappyTickets;

uses
  SysUtils, DateUtils;

procedure run;
  var
    n1, n2, n3, n4, n5, n6, n7, n8: Integer;
    nx: Integer;
    m5, m6, m7: Integer;
    nx5, nx6: Integer;
    TicketsCount: int64;
    d1, d2: TDateTime;
  begin
    TicketsCount := 0;
    d1 := Now;
    for n1 := 0 to 9 do
      for n2 := 0 to 9 do
        for n3 := 0 to 9 do
          for n4 := 0 to 9 do begin
            nx:= n1 + n2 + n3 + n4;
            if nx > 9 then m5:= 9 else m5:= nx;
            for n5 := 0 to m5 do begin
              nx5:= nx - n5;
              if nx5 < 0 then break;
              if nx5 > 9 then m6 := 9 else m6:= nx5;
              for n6 := 0 to m6 do begin
                nx6:= nx5 - n6;
                if nx6 < 0 then break;
                if nx6 > 9 then m7:= 9 else m7:= nx6;
                for n7 := 0 to m7 do begin
                  //for n8 := 0 to 9 do
                  if nx6 - n7 in [0..9] then
                      TicketsCount := TicketsCount + 1; // Inc(TicketsCount) may be slower in FPC
                end;
              end;
            end;
          end;
    d2 := Now;
    writeln('Found ', TicketsCount, ' tickets. Elapsed time, msec: ', DateUtils.MilliSecondsBetween(d1, d2));
  end;

begin
    run;
end.
