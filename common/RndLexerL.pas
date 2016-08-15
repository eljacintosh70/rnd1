unit RndLexerL;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, RndBase;

type
  TTokenKind = (tkLPar, tkRPar, tkOther, tkSymbol, tkNum);
  TTokenInfo = record
    pStart: PChar;
    len: Word;
    Kind: TTokenKind;
    Val: IDatum;
  end;

  TLexer = class
  protected
    Buf: String;
    pStart, pCurr, pEnd: PChar;
    function BuildToken(ps, p: PChar; out Token: TTokenInfo; Kind: TTokenKind;
      const Val: IDatum): Boolean;
    function ScanIdent(ps, p: PChar; out Token: TTokenInfo): Boolean;
    function ScanNum(ps, p: PChar; out Token: TTokenInfo): Boolean;
  public
    constructor Create(s: String);
    function GetNext(out Token: TTokenInfo): Boolean;
  end;

implementation

{ TLexer }

constructor TLexer.Create(s: String);
begin
  Buf := s;
  pStart := Pointer(Buf);
  pCurr := pStart;
  pEnd := pStart + Length(Buf);
end;

function TLexer.BuildToken(ps, p: PChar; out Token: TTokenInfo;
  Kind: TTokenKind; const Val: IDatum): Boolean;
begin
  pCurr := p;
  Token.Kind := Kind;
  Token.pStart := ps;
  Token.len := p - ps;
  Token.Val := Val;
  Result := True;
end;

function TLexer.ScanIdent(ps, p: PChar; out Token: TTokenInfo): Boolean;
var
  s: String;
  Val: IDatum;
begin
  while (p < pEnd) do
  begin
    case p^ of
      '(', ')', #0..#$20: break;
    end;
    Inc(p);
  end;
  SetString(s, ps, p - ps);
  Val := MakeSymbol(s);
  Result := BuildToken(ps, p, Token, tkSymbol, Val);
end;

function TLexer.ScanNum(ps, p: PChar; out Token: TTokenInfo): Boolean;
var
  s: String;
  Num: Double;
  Val: IDatum;
begin
  while (p < pEnd) do
  begin
    case p^ of
      '(', ')', #0..#$20: break;
    end;
    Inc(p);
  end;
  SetString(s, ps, p - ps);
  if TryStrToFloat(s, Num) then
  begin
    Val := MakeNumber(Num);
    Result := BuildToken(ps, p, Token, tkNum, Val);
  end
  else
  begin       // o es mejor regresar false ?
    Val := MakeSymbol(s);
    Result := BuildToken(ps, p, Token, tkSymbol, Val);
  end;
end;

function TLexer.GetNext(out Token: TTokenInfo): Boolean;
var
  p, ps: PChar;
begin
  Result := False;
  p := pCurr;
  while (p < pEnd) do
  begin
    while (p^ <= #$20) do
    begin
      Inc(p);
      if (p >= pEnd) then
        Exit;
    end;
    ps := p;
    case p^ of
      ';': begin   // saltar comentarios
             while (p < pEnd) do
               case p^ of
                 #10: begin
                        Inc(p);
                        break;
                      end
               else Inc(p);
               end;
             Continue;
           end;
      '(': begin
             Inc(p);
             Result := BuildToken(ps, p, Token, tkLPar, nil);
           end;
      ')': begin
             Inc(p);
             Result := BuildToken(ps, p, Token, tkRPar, nil);
           end;
      '0'..'9': Result := ScanNum(ps, p, Token);
      '-': begin
             Inc(p);
             case p^ of
               '0'..'9': Result := ScanNum(ps, p, Token);
             else
               Result := ScanIdent(ps, p, Token);
             end;
           end
       else   Result := ScanIdent(ps, p, Token);
    end;
    Exit;
  end;
end;

end.
