unit DTPortA;

interface

uses
  SysUtils,
  DTDatum, DTPort;

type
  TStrTextOutA = class(TObject)
  private
    Buf: RawByteString;
    Ofs, Len: Integer;
    function GrowTo(NewLen: Integer): Boolean; virtual;
  public
    function WriteText(p: PAnsiChar; cc: Integer): Boolean; inline;
  public
    constructor Create(ALen: Integer);
    function WriteProc(p: Pointer; cb: Integer): Boolean;
    procedure Reset;
    function GetText: RawByteString;
  end;

  TCustomDynOutPortA = class(TCustomDynOutPort)
  public
    function WriteText(p: PAnsiChar; cc: Integer): Boolean; overload; inline;
    function WriteText(const s: RawByteString): Boolean; overload; inline;
  end;

implementation


{ TStrTextOutA }

function TStrTextOutA.GrowTo(NewLen: Integer): Boolean;
begin
  NewLen := (NewLen + 15) and not 15;
  SetLength(Buf, NewLen);
  Len := NewLen;
  Result := True;
end;

function TStrTextOutA.WriteProc(p: Pointer; cb: Integer): Boolean;
begin
  Result := WriteText(p, cb div SizeOf(WideChar));
end;

function TStrTextOutA.WriteText(p: PAnsiChar; cc: Integer): Boolean;
var
  NewOfs, i0: Integer;
begin
  NewOfs := Ofs + cc;
  if NewOfs > Len then
  begin
    Result := GrowTo(NewOfs);
    if not Result then
      Exit;
  end;
  i0 := Ofs + 1;
  Move(p^, Buf[i0], cc * SizeOf(WideChar));
  Ofs := NewOfs;
  Result := True;
end;

constructor TStrTextOutA.Create(ALen: Integer);
begin
  GrowTo(ALen);
end;

procedure TStrTextOutA.Reset;
begin
  Ofs := 0;
end;

function TStrTextOutA.GetText: RawByteString;
begin
  Result := Copy(Buf, 1, Ofs);
end;


{ TCustomDynOutPortA }

function TCustomDynOutPortA.WriteText(p: PAnsiChar; cc: Integer): Boolean;
begin
  Result := WriteProc(p, cc * SizeOf(AnsiChar))
end;

function TCustomDynOutPortA.WriteText(const s: RawByteString): Boolean;
begin
  Result := WriteProc(Pointer(s), Length(s) * SizeOf(AnsiChar))
end;

end.

