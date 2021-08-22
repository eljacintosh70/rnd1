unit LispWrite;

interface

uses
  SysUtils, Classes,
  DynTypes, DTDatum, DUtils, DTPort, DTPortW, DTFile;

type
  TDynOutPortLisp = class(TDynOutPortWr)
  public
    function BeginList(Kind: TListKind): IDynOutPort; override; stdcall;
  protected
    function WriteChar(ch: Char): Boolean; override;
    function EscapeStr(p: PWideChar; cc: Integer): Boolean; override;
  end;

  TDynOutPortLispIndent = class(TDynOutPortIndent)
  public
    function IDynOutPort_BeginList(Kind: TListKind): IDynOutPort; override; stdcall;
  end;

function LispOutFn(AWriteProc: TWriteProc): IDynOutPort;
function LispOutStream(AStream: TStream): IDynObjStream;
function LispOutFile(FileName: string): IDynObjStream;

implementation

function LispOutFn(AWriteProc: TWriteProc): IDynOutPort;
begin
  Result := TDynOutPortLisp.Create(AWriteProc);
end;

function LispOutStream(AStream: TStream): IDynObjStream;
var
  Format: TDynOutPortLisp;
begin
  Format := TDynOutPortLisp.Create(nil);
  Result := TDynObjOutStream.Create(AStream, Format);
end;

function LispOutFile(FileName: string): IDynObjStream;
var
  Stream: TFileStream;
begin
  Stream := TFileStream.Create(FileName, fmCreate);
  Result := LispOutStream(Stream);
end;

{ TDynOutPortLisp }

function TDynOutPortLisp.BeginList(Kind: TListKind): IDynOutPort;
const
  Separ = #13#10;
var
  AOpen, ASepar, AClose: string;
begin
  case Kind of
    lkVector:
      begin
        AOpen  := '#(';
        AClose := ')';
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
  Result := TDynOutPortLispIndent.Create(Self, AOpen, ASepar, AClose)
end;

function TDynOutPortLisp.WriteChar(ch: Char): Boolean;
begin
  case ch of
    #$00: Result := WriteText('#\null');      // the null character, U+0000
    #$07: Result := WriteText('#\alarm');     // U+0007
    #$08: Result := WriteText('#\backspace'); // U+0008
    #$09: Result := WriteText('#\tab');       // the tab character, U+0009
    #$0A: Result := WriteText('#\newline');   // the linefeed character, U+000A
    #$0D: Result := WriteText('#\return');    // the return character, U+000D
    #$1B: Result := WriteText('#\escape');    // U+001B
    #$20: Result := WriteText('#\space');     // the preferred way to write a space
    #$7F: Result := WriteText('#\delete');    // U+007F
    #$21..#$7E, #$00A0..#$FFFF:
          Result := WriteText('#\' + ch);
    else  Result := WriteText(Format('#\x%.3x', [Ord(ch)]));
  end;
end;

function TDynOutPortLisp.EscapeStr(p: PWideChar; cc: Integer): Boolean;
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

{ TDynOutPortLispIndent }

function TDynOutPortLispIndent.IDynOutPort_BeginList(Kind: TListKind): IDynOutPort;
var
  AOpen, ASepar, AClose: string;
begin
  case Kind of
    lkVector:
      begin
        AOpen  := '#(';
        AClose := ')';
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
  Result := TDynOutPortLispIndent.Create(Port, AOpen, ASepar, AClose)
end;

end.

