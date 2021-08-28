unit DTParser;

interface

uses
  DynTypes;

type
  TTokenKind = (tkEnd, tkLPar, tkRPar, tkDatum, tkPoint, tkVector, tkByteVector,
    tkQuote, tkQuasiQuote, tkUnQuote, tkUnQuoteSplicing);
  TQuoteTokens = tkQuote..tkUnQuoteSplicing;

type
  TTokenInfo = record
    Kind: TTokenKind;
    Ref: TDatumRef;
  end;

  TCustomLexer = class(TObject)
  public
    function GetNext(out Res: TTokenInfo): Boolean; virtual; abstract;
  end;

procedure CreateInt(x: Int64; out Res: TDatumRef);

function IsDigit(ch: WideChar): Boolean; inline;          overload;
function NeedChar(p: PWideChar; ch: WideChar): PWideChar; overload;
function SkipBlank(p: PWideChar): PWideChar;              overload;
function ScanIntB(p: PWideChar; var i: Int64): PWideChar; overload;
function ScanIntO(p: PWideChar; var i: Int64): PWideChar; overload;
function ScanInt (p: PWideChar; var i: Int64): PWideChar; overload;
function ScanIntX(p: PWideChar; var i: Int64): PWideChar; overload;

function IsDigit(ch: AnsiChar): Boolean; inline;          overload;
function NeedChar(p: PAnsiChar; ch: AnsiChar): PAnsiChar; overload;
function SkipBlank(p: PAnsiChar): PAnsiChar;              overload;
function ScanIntB(p: PAnsiChar; var i: Int64): PAnsiChar; overload;
function ScanIntO(p: PAnsiChar; var i: Int64): PAnsiChar; overload;
function ScanInt (p: PAnsiChar; var i: Int64): PAnsiChar; overload;
function ScanIntX(p: PAnsiChar; var i: Int64): PAnsiChar; overload;

implementation

procedure CreateInt(x: Int64; out Res: TDatumRef);
var
  Value: dyn;
begin
  Value := MakeInt64(x);
  Res := (Value);
end;

function IsDigit(ch: WideChar): Boolean; inline;
begin
  case ch of
    '0'..'9': Result := True;
    else      Result := False;
  end;
end;

function NeedChar(p: PWideChar; ch: WideChar): PWideChar;
begin
  repeat
    case p^ of
      #$01..#$20: ;
      ';': begin
             Inc(p);
             repeat
               case p^ of
                 #0: {if p >= pe then} Break;
                 #10, #13:
                   Break;
               end;
               Inc(p);
             until (False);
             Continue;
           end;
      else Break;
    end;
    Inc(p);
  until False;
  if p^ <> ch then
    DynError('NeedChar %s', [ch]);
  Inc(p);
  Result := p;
end;

function SkipBlank(p: PWideChar): PWideChar;
begin
  repeat
    case p^ of
      #$01..#$20: ;
      ';': begin
             Inc(p);
             repeat
               case p^ of
                 #0: {if p >= pe then} Break;
                 #10, #13:
                   Break;
               end;
               Inc(p);
             until (False);
             Continue;
           end;
      else Break;
    end;
    Inc(p);
  until False;
  Result := p;
end;

function ScanIntB(p: PWideChar; var i: Int64): PWideChar; overload;
var
  v: Int64;
begin
  Result := p;
  v := 0;
  repeat
    case Result^ of
      '0'..'1': v := 2 * v + Ord(Result^) - $30;
      else Break;
    end;
    Inc(Result);
  until False;
  i := v;
end;

function ScanIntO(p: PWideChar; var i: Int64): PWideChar; overload;
var
  v: Int64;
begin
  Result := p;
  v := 0;
  repeat
    case Result^ of
      '0'..'7': v := 8 * v + Ord(Result^) - $30;
      else Break;
    end;
    Inc(Result);
  until False;
  i := v;
end;

function ScanInt(p: PWideChar; var i: Int64): PWideChar; overload;
var
  v: Int64;
begin
  Result := p;
  v := 0;
  repeat
    case Result^ of
      '0'..'9': v := 10 * v + Ord(Result^) - $30;
      else Break;
    end;
    Inc(Result);
  until False;
  i := v;
end;

function ScanIntX(p: PWideChar; var i: Int64): PWideChar; overload;
var
  v: Int64;
begin
  Result := p;
  v := 0;
  repeat
    case Result^ of
      '0'..'9': v := $10 * v + Ord(Result^) - $30;
      'A'..'F': v := $10 * v + Ord(Result^) - ($41 - 10);
      'a'..'f': v := $10 * v + Ord(Result^) - ($61 - 10);
      else Break;
    end;
    Inc(Result);
  until False;
  i := v;
end;

function IsDigit(ch: AnsiChar): Boolean; inline;
begin
  case ch of
    '0'..'9': Result := True;
    else      Result := False;
  end;
end;

function NeedChar(p: PAnsiChar; ch: AnsiChar): PAnsiChar;
begin
  repeat
    case p^ of
      #$01..#$20: ;
      ';': begin
             Inc(p);
             repeat
               case p^ of
                 #0: {if p >= pe then} Break;
                 #10, #13:
                   Break;
               end;
               Inc(p);
             until (False);
             Continue;
           end;
      else Break;
    end;
    Inc(p);
  until False;
  if p^ <> ch then
    DynError('NeedChar %s', [ch]);
  Inc(p);
  Result := p;
end;

function SkipBlank(p: PAnsiChar): PAnsiChar;
begin
  repeat
    case p^ of
      #$01..#$20: ;
      ';': begin
             Inc(p);
             repeat
               case p^ of
                 #0: {if p >= pe then} Break;
                 #10, #13:
                   Break;
               end;
               Inc(p);
             until (False);
             Continue;
           end;
      else Break;
    end;
    Inc(p);
  until False;
  Result := p;
end;

function ScanIntB(p: PAnsiChar; var i: Int64): PAnsiChar; overload;
var
  v: Int64;
begin
  Result := p;
  v := 0;
  repeat
    case Result^ of
      '0'..'1': v := 2 * v + Ord(Result^) - $30;
      else Break;
    end;
    Inc(Result);
  until False;
  i := v;
end;

function ScanIntO(p: PAnsiChar; var i: Int64): PAnsiChar; overload;
var
  v: Int64;
begin
  Result := p;
  v := 0;
  repeat
    case Result^ of
      '0'..'7': v := 8 * v + Ord(Result^) - $30;
      else Break;
    end;
    Inc(Result);
  until False;
  i := v;
end;

function ScanInt(p: PAnsiChar; var i: Int64): PAnsiChar; overload;
var
  v: Int64;
begin
  Result := p;
  v := 0;
  repeat
    case Result^ of
      '0'..'9': v := 10 * v + Ord(Result^) - $30;
      else Break;
    end;
    Inc(Result);
  until False;
  i := v;
end;

function ScanIntX(p: PAnsiChar; var i: Int64): PAnsiChar; overload;
var
  v: Int64;
begin
  Result := p;
  v := 0;
  repeat
    case Result^ of
      '0'..'9': v := $10 * v + Ord(Result^) - $30;
      'A'..'F': v := $10 * v + Ord(Result^) - ($41 - 10);
      'a'..'f': v := $10 * v + Ord(Result^) - ($61 - 10);
      else Break;
    end;
    Inc(Result);
  until False;
  i := v;
end;


end.

