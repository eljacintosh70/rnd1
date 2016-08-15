unit RndLexer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, RndBase,
  Generics.Collections;

type
  TTokenKind = (tkOther, tkSpace, tkIdent, tkNum,
    tkCol,                            // :  ->  :=
    tkLt,                             // <  ->  <= <>
    tkGt,                             // >  ->  >=
    // :=     <=     <>     >=
    tkAsig, tkLEq, tkDif, tkGEq,
    tkSymbol,
    // ;      ,      .
    tkSeCo, tkCom, tkPoint,
    // (       )       [       ]         {       }
    tkLPar, tkRPar, tkLSqBr, tkRSqBr, tkLCuBr, tkRCuBr,
    // =    +     -      *      /
    tkEq, tkAdd, tkSub, tkMul, tkDvd,
    // 'not'
    tkNot,
    // 'or'  | 'xor' | 'div' | 'mod' | 'and' | 'shl' | 'shr' | 'as' | 'in' | 'is'
    tkor,     tkxor,  tkdiv,  tkmod,  tkand,  tkshl,  tkshr,  tkas,  tkin,  tkis );

  TTokenInfo = record
    pStart: PChar;
    len: Word;
    Kind: TTokenKind;
    Val: IDatum;
  end;

  TTokenEntry = record
    Val: IDatum;
    Kind: TTokenKind;
  end;

  { TLexer }

  TLexer = class
  protected
    Buf: String;
    pStart, pCurr, pEnd: PChar;
    function BuildToken(ps, p: PChar; out Token: TTokenInfo; Kind: TTokenKind;
      const Val: IDatum): Boolean;
    function ScanIdent(var p: PChar; out Token: TTokenInfo): Boolean;
    function SingCharOp(var p: PChar; out Token: TTokenInfo; Kind: TTokenKind): Boolean;
    function TwoCharOp(var p: PChar; out Token: TTokenInfo; Kind: TTokenKind): Boolean;
    function ScanNum(var p: PChar; out Token: TTokenInfo): Boolean;
  public
    procedure DefineIdents(const Names: array of String; const IDs: array of TTokenKind);
    procedure InitTokens;
    constructor Create(s: String);
    // ':='  '<='  '<>'   '>='
    function GetNext(out Token: TTokenInfo): Boolean;
  end;

var
  TokenDict: TDictionary<String, TTokenEntry>;

procedure MakeIdent(Name: String; Kind: TTokenKind);
function GetIdent(Name: String): TTokenEntry;

implementation

procedure MakeIdent(Name: String; Kind: TTokenKind);
var
  Entry: TTokenEntry;
begin
  if not TokenDict.TryGetValue(Name, Entry) then
  begin
    Entry.Val := MakeSymbol(Name);
    Entry.Kind := Kind;
    TokenDict.Add(Name, Entry);
  end
  else
  begin
    Entry.Kind := Kind;
    TokenDict[Name] := Entry;
  end
end;

function GetIdent(Name: String): TTokenEntry;
begin
  if not TokenDict.TryGetValue(Name, Result) then
  begin
    Result.Val := MakeSymbol(Name);
    Result.Kind := tkIdent;
  end;
end;

{ TLexer }

constructor TLexer.Create(s: String);
begin
  Buf := s;
  pStart := Pointer(Buf);
  pCurr := pStart;
  pEnd := pStart + Length(Buf);
  InitTokens;
end;

procedure TLexer.DefineIdents(const Names: array of String;
  const IDs: array of TTokenKind);
var
  i: Integer;
begin
  for i := Low(Names) to High(Names) do
    MakeIdent(Names[i], IDs[i]);
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

function TLexer.ScanIdent(var p: PChar; out Token: TTokenInfo): Boolean;
var
  s: String;
  ps: PChar;
  Val: IDatum;
  Entry: TTokenEntry;
begin
  ps := p;
  while (p < pEnd) do
  begin
    case p^ of
      'A'..'Z', 'a'..'z', '_', '0', '9': ;
      else Break;
    end;
    Inc(p);
  end;
  SetString(s, ps, p - ps);
  Entry := GetIdent(s);
  Result := BuildToken(ps, p, Token, Entry.Kind, Entry.Val);
end;

function TLexer.SingCharOp(var p: PChar; out Token: TTokenInfo; Kind: TTokenKind): Boolean;
var
  s: String;
  Val: IDatum;
begin
  s := p[0];
  Val := MakeSymbol(s);
  Result := BuildToken(p, p + 1, Token, Kind, Val);
  Inc(p);
end;

function TLexer.TwoCharOp(var p: PChar; out Token: TTokenInfo; Kind: TTokenKind): Boolean;
var
  s: String;
  Val: IDatum;
begin
  s := p[0] + p[1];
  Val := MakeSymbol(s);
  Result := BuildToken(p, p + 2, Token, Kind, Val);
  Inc(p, 2);
end;

function TLexer.ScanNum(var p: PChar; out Token: TTokenInfo): Boolean;
var
  s: String;
  ps: PChar;
  Num: Double;
  Val: IDatum;
begin                        // { digit } [ '.' { digit } ] [ 'E' [ '+' | '-' ] { digit } ]
  ps := p;
  while (p < pEnd) do
    case p^ of
      '0'..'9': Inc(p);      // { digit }
      else break;
    end;

  if p^ = '.' then
  begin                      //  [ '.'
    Inc(p);
    while (p < pEnd) do
      case p^ of
        '0'..'9': Inc(p);    //    { digit }
        else break;
      end;
  end;                       //  ]

  case p^ of
    'e', 'E':                //  [ 'E'
      begin
        Inc(p);
        case p^ of
          '-', '+': Inc(p);  //     [ '+' | '-' ]
        end;
        while (p < pEnd) do
          case p^ of
            '0'..'9': Inc(p);//       { digit }
            else break;
          end;
      end;                   //  ]
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

var
  TokenKindTab: array [#0..#$7F] of TTokenKind;

procedure TLexer.InitTokens;
var
  ch: AnsiChar;
begin
  for ch in [#1..#$20] do
    TokenKindTab[ch] := tkSpace;
  for ch in ['A'..'Z', 'a'..'z', '_'] do
    TokenKindTab[ch] := tkIdent;
  for ch in ['0'..'9'] do
    TokenKindTab[ch] := tkNum;

  TokenKindTab[':'] := tkCol;  // :  ->  :=
  TokenKindTab['<'] := tkLt;   // <  ->  <= <>
  TokenKindTab['>'] := tkGt;   // >  ->  >=
  TokenKindTab[';'] := tkSeCo;

  // (       )       [       ]         {       }                                   .
  TokenKindTab['('] := tkLPar;   TokenKindTab[')'] := tkRPar;
  TokenKindTab['['] := tkLSqBr;  TokenKindTab[']'] := tkRSqBr;
  TokenKindTab['{'] := tkLCuBr;  TokenKindTab['}'] := tkRCuBr;

  // =    +     -      *      /
  TokenKindTab['='] := tkEq;
  TokenKindTab['+'] := tkAdd;
  TokenKindTab['-'] := tkSub;
  TokenKindTab['*'] := tkMul;
  TokenKindTab['/'] := tkDvd;

  DefineIdents(['or', 'xor',    'div', 'mod', 'and', 'shl', 'shr', 'as',    'in', 'is', 'not'],
              [tkor, tkxor,    tkdiv, tkmod, tkand, tkshl, tkshr, tkas,    tkin, tkis, tkNot]);
end;

function TLexer.GetNext(out Token: TTokenInfo): Boolean;
var
  p, ps: PChar;
  Kind: TTokenKind;
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
    Kind := TokenKindTab[p^];
    case Kind of
      tkLCuBr:
        begin   // saltar comentarios
           while (p < pEnd) do
             case p^ of
               '}': begin
                      Inc(p);
                      break;
                    end
             else Inc(p);
             end;
           Continue;
         end;
      tkIdent:
        Result := ScanIdent(p, Token);
      tkNum:
        Result := ScanNum(p, Token);
      tkCol:                            // :  ->  :=
        case p[1] of
          '=': Result := TwoCharOp(p, Token, tkAsig);  // :=
        else   Result := SingCharOp(p, Token, Kind);
        end;
      tkLt:                             // <  ->  <= <>
        case p[1] of
          '=': Result := TwoCharOp(p, Token, tkLEq);  // <=
          '>': Result := TwoCharOp(p, Token, tkDif);  // <>
        else   Result := SingCharOp(p, Token, Kind);
        end;
      tkGt:                             // >  ->  >=
        case p[1] of
          '=': Result := TwoCharOp(p, Token, tkGEq);  // >=
        else   Result := SingCharOp(p, Token, Kind);
        end;
      else Result := SingCharOp(p, Token, Kind);
    end;
    Exit;
  end;
end;

initialization
  TokenDict := TDictionary<String, TTokenEntry>.Create;
end.

