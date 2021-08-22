unit RndWrite;

interface

uses
  SysUtils, Classes,
  DynTypes, DTDatum, DUtils, DTPort, DTPortW, DTFile;

type
  TDynOutPortRnd = class(TDynOutPortWr)
  public
    function BeginList(Kind: TListKind): IDynOutPort; override; stdcall;
    function WriteSpecial(v: TSpecialValue): Boolean; override; stdcall;
  protected
    function WriteChar(ch: Char): Boolean; override;
    function EscapeStr(p: PWideChar; cc: Integer): Boolean; override;
  end;

  TDynOutPortRndIndent = class(TDynOutPortIndent)
  public
    function IDynOutPort_BeginList(Kind: TListKind): IDynOutPort; override; stdcall;
  end;

function RndOutFn(AWriteProc: TWriteProc): IDynOutPort;
function RndOutStream(AStream: TStream): IDynObjStream;
function RndOutFile(FileName: string): IDynObjStream;

implementation

function RndOutFn(AWriteProc: TWriteProc): IDynOutPort;
begin
  Result := TDynOutPortRnd.Create(AWriteProc);
end;

function RndOutStream(AStream: TStream): IDynObjStream;
var
  Format: TDynOutPortRnd;
begin
  Format := TDynOutPortRnd.Create(nil);
  Result := TDynObjOutStream.Create(AStream, Format);
end;

function RndOutFile(FileName: string): IDynObjStream;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  Result := RndOutStream(Stream);
end;

{ TDynOutPortRnd }

function TDynOutPortRnd.BeginList(Kind: TListKind): IDynOutPort;
const
  Separ = ','#13#10;
var
  AOpen, ASepar, AClose: string;
begin
  case Kind of
    lkVector:
      begin
        AOpen  := '[ ';
        AClose := ']';
      end;
    lkDict:
      begin
        AOpen  := '{ ';
        AClose := '}';
      end;
    else //lkList: ;
      begin
        AOpen  := '( ';
        AClose := ')';
      end
  end;
  ASepar := Separ + '  ';
  Result := TDynOutPortRndIndent.Create(Self, AOpen, ASepar, AClose)
end;

function TDynOutPortRnd.WriteSpecial(v: TSpecialValue): Boolean;
var
  s: String;
begin
  case v of
    svFalse: s := 'false';
    svTrue:  s := 'true';
    svNil:   s := 'nil';
    else  Result := False;
          Exit;
  end;
  Result := WriteText(s)
end;

function TDynOutPortRnd.WriteChar(ch: Char): Boolean;
begin
  case ch of
    #$20..#$7E, #$00A0..#$FFFF:
          Result := WriteText('''' + ch + '''');
    else  Result := WriteText(Format('#%d', [Ord(ch)]));
  end;
end;

function TDynOutPortRnd.EscapeStr(p: PWideChar; cc: Integer): Boolean;
var
  ps, pe: PWideChar;
  s: string;
begin
  Result := True;

  pe := p + cc;
  ps := p;
  while p < pe do
  begin
    case p^ of
      //#$00: s := '\x0000;';      // the null character, U+0000
      #$07: s := '\a'; // alarm, U+0007
      #$08: s := '\b'; // backspace, U+0008
      #$09: s := '\t'; // character tabulation, U+0009
      #$0A: s := '\n'; // linefeed, U+000A
      #$0D: s := '\r'; // return, U+000D'
      '"' : s := '\"'; // double quote, U+0022
      '\' : s := '\\'; // backslash, U+005C
      '|' : s := '\|'; // vertical line, U+007C
      #$00..#$06, #$0B, #$0C, #$0E..#$1F, #$0080..#$009F:
            s := Format('\x%.3x;', [Ord(p^)]);
      // falta \ al final de una linea
    end;
    if s <> '' then
    begin
      if p > ps then
      begin
        Result := WriteText(ps, p - ps);
        if not Result then
          Exit;
      end;
      Result := WriteText(s);
      if not Result then
        Exit;
      s := ''; // hacerlo solo después de usarla
      ps := p + 1;
    end;
    Inc(p);
  end;
  if p > ps then
    Result := WriteText(ps, p - ps);
end;

{ TDynOutPortRndIndent }

function TDynOutPortRndIndent.IDynOutPort_BeginList(Kind: TListKind): IDynOutPort;
var
  AOpen, ASepar, AClose: string;
begin
  case Kind of
    lkVector:
      begin
        AOpen  := '[ ';
        AClose := ']';
      end;
    lkDict:
      begin
        AOpen  := '{ ';
        AClose := '}';
      end;
    else //lkList: ;
      begin
        AOpen  := '( ';
        AClose := ')';
      end
  end;
  ASepar := Separ + '  ';
  Result := TDynOutPortRndIndent.Create(Port, AOpen, ASepar, AClose)
end;

end.

