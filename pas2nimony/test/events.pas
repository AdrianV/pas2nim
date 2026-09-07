program Events;
{ M4-3: bound method pointers (`procedure of object`): assigned,
  rebound, nil-ed and invoked through a property, a field and a
  plain variable - the bound instance rides along }
type
  TNotifyEvent = procedure(Sender: TObject) of object;

  TButton = class
  private
    FOnClick: TNotifyEvent;
    FLabel: String;
  public
    constructor Create(const ALabel: String);
    procedure Fire;
    property OnClick: TNotifyEvent read FOnClick write FOnClick;
  end;

  TForm1 = class
  public
    procedure ButtonClick(Sender: TObject);
  end;

  TForm2 = class
  public
    procedure OtherClick(Sender: TObject);
  end;

constructor TButton.Create(const ALabel: String);
begin
  FLabel := ALabel;
end;

procedure TButton.Fire;
begin
  if Assigned(FOnClick) then
    FOnClick(Self)
  else
    writeln(FLabel, ': no handler');
end;

procedure TForm1.ButtonClick(Sender: TObject);
begin
  writeln('form1 handles it');
end;

procedure TForm2.OtherClick(Sender: TObject);
begin
  writeln('form2 handles it');
end;

var
  Btn: TButton;
  F1: TForm1;
  F2: TForm2;
  ev: TNotifyEvent;
begin
  Btn := TButton.Create('BTN');
  F1 := TForm1.Create;
  F2 := TForm2.Create;

  Btn.Fire;
  Btn.OnClick := F1.ButtonClick;
  Btn.Fire;
  Btn.OnClick := F2.OtherClick;
  Btn.Fire;
  Btn.OnClick := nil;
  Btn.Fire;

  ev := F2.OtherClick;
  if Assigned(ev) then
    ev(nil)
  else
    writeln('ev is nil');
end.