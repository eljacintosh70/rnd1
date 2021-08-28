unit RndLexer;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils, Generics.Collections,
  DynTypes, DTParser;

type
  TTokenKind = (tkOther, tkSpace, tkIdent,
    tkNil, tkFalse, tkTrue, tkNum, tkChar, tkStr, tkMem,      // tkMinVal..tkMaxVal
    tkSharp,                          // #
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

const
  tkMinVal = tkNil;
  tkMaxVal = tkMem;

type
  TTokenInfo = record
    Kind: TTokenKind;
    Val: dyn;
    pStart: PChar;
    len: Word;
  end;

  TTokenEntry = record
    Kind: TTokenKind;
    Val: dyn;
  end;

  { TLexer }

  TLexer = class
  protected
    Buf: String;
    pStart, pCurr, pEnd: PChar;
    function BuildToken(ps, p: PChar; out Token: TTokenInfo; Kind: TTokenKind;
      const Val: dyn): Boolean;
    function ScanIdent(var p: PChar; out Token: TTokenInfo): Boolean;
    function SingCharOp(var p: PChar; out Token: TTokenInfo; Kind: TTokenKind): Boolean;
    function TwoCharOp(var p: PChar; out Token: TTokenInfo; Kind: TTokenKind): Boolean;
    function ScanNum(var p: PChar; out Token: TTokenInfo): Boolean;
    function ScanChar(var p: PChar; out Token: TTokenInfo): Boolean;
    function ScanStr(var p: PChar; out Token: TTokenInfo): Boolean;
    function ScanMem(var p: PChar; out Token: TTokenInfo): Boolean;
    function ScanMem8(ps: PChar; var p: PChar; out Token: TTokenInfo): Boolean;
    function ScanMem8b(ps: PChar; var p: PChar; out Token: TTokenInfo): Boolean;
  public
    procedure DefineIdents(const Names: array of String; const IDs: array of TTokenKind);
    procedure InitTokens;
    constructor Create(s: String);
    // ':='  '<='  '<>'   '>='
    function GetNext(out Token: TTokenInfo): Boolean;
  end;

var
  TokenDict: TDictionary<String, TTokenEntry>;

function MakeSymbol(Name: String): dyn;
procedure MakeIdent(Name: String; Kind: TTokenKind);
function GetIdent(Name: String): TTokenEntry;

implementation

var
  TokenKindTab: array [#0..#$7F] of TTokenKind;

function MakeSymbol(Name: String): dyn;
begin
  Result := InitSymbol(UTF8String(Name))
end;

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
  Kind: TTokenKind; const Val: dyn): Boolean;
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
  Val: dyn;
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
  Val: dyn;
begin
  s := p[0];
  Val := MakeSymbol(s);
  Result := BuildToken(p, p + 1, Token, Kind, Val);
  Inc(p);
end;

function TLexer.TwoCharOp(var p: PChar; out Token: TTokenInfo; Kind: TTokenKind): Boolean;
var
  s: String;
  Val: dyn;
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
  NumI: Int64;
  Val: dyn;
  IsFloat: Boolean;
begin                        // { digit } [ '.' { digit } ] [ 'E' [ '+' | '-' ] { digit } ]
  IsFloat := False;
  ps := p;
  while (p < pEnd) do
    case p^ of
      '0'..'9': Inc(p);      // { digit }
      else break;
    end;

  if p^ = '.' then           //  [ '.'
  begin
    IsFloat := True;
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
        IsFloat := True;
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
  if IsFloat then
  begin
    Num := StrToFloat(s);
    Val := MakeDouble(Num);
  end
  else
  begin
    NumI := StrToInt64(s);
    Val := MakeInt64(NumI);
  end;
  Result := BuildToken(ps, p, Token, tkNum, Val);
end;

function TLexer.ScanChar(var p: PChar; out Token: TTokenInfo): Boolean;
var
  ps: PChar;
  Val: dyn;
begin
  ps := p;
  Inc(p);
  Val := MakeChar(p^);
  Inc(p);
  Inc(p);
  Result := BuildToken(ps, p, Token, tkChar, Val);
end;

function TLexer.ScanStr(var p: PChar; out Token: TTokenInfo): Boolean;
var
  ps: PChar;
  pe: PChar;
  s: IDynString;
begin
  if p^ = '"' then
    Inc(p);
  ps := p;
  pe := pEnd;
  while p < pe do
    case p^ of
      '"':
        begin
          s := make_string(ps, p - ps);
          Inc(p);
          Result := BuildToken(ps, p, Token, tkStr, s);
          Exit;
        end;
      else
        Inc(p);
    end;

  s := make_string(ps, p - ps);
  Result := BuildToken(ps, p, Token, tkStr, s);
end;

function TLexer.ScanMem(var p: PChar; out Token: TTokenInfo): Boolean;
var
  ps: PChar;
begin
  ps := p;
  Inc(p, 2);       // #m
  case p^ of
    ':':
      begin
        Inc(p);
        case p^ of
          'b', 'B':
             begin
               Inc(p);
               p := NeedChar(p, '(');
               Result := ScanMem8b(ps, p, Token);
             end;
        else
          DynError('NeedChar "b" after #m:', []);
          Result := False;
        end;
      end;
    '(':
      begin
        Inc(p);
        Result := ScanMem8(ps, p, Token);
      end;
  else
    DynError('NeedChar "(" or ":" after #m', []);
    Result := False;
  end;
end;

function TLexer.ScanMem8(ps: PChar; var p: PChar; out Token: TTokenInfo): Boolean;
var
  ch: Char;
  v: Byte;
  Datum: IDynMemory;
  List: dyn;
begin
  List := nil;
  repeat
    p := SkipBlank(p);
    v := 0;
    ch := p^; Inc(p);
    case ch of
      '0'..'9': v := Ord(ch) - $30;
      'A'..'F': v := Ord(ch) - ($41 - 10);
      'a'..'f': v := Ord(ch) - ($61 - 10);
      ')': Break;
      else
        DynError('Hex value required: %s', [ch]);
    end;
    ch := p^; Inc(p);
    case ch of
      '0'..'9': v := $10 * v + Ord(ch) - $30;
      'A'..'F': v := $10 * v + Ord(ch) - ($41 - 10);
      'a'..'f': v := $10 * v + Ord(ch) - ($61 - 10);
      #1..#$20: ;
      ')': Break;
      else
        DynError('Hex value required: %s', [ch]);
    end;
    List := Cons(v, List);
  until False;
  Datum := ListRevToDynMemory(List);
  Result := BuildToken(ps, p, Token, tkMem, Datum);
end;

function TLexer.ScanMem8b(ps: PChar; var p: PChar; out Token: TTokenInfo): Boolean;
var
  ch: Char;
  v: Byte;
  Datum: IDynMemory;
  List: dyn;
  i: Integer;
begin
  List := nil;
  repeat
    p := SkipBlank(p);
    v := 0;
    ch := p^; Inc(p);
    case ch of
      '0'..'1': v := Ord(ch) - $30;
      ')': Break;
      else
        DynError('Binary value required: %s', [ch]);
    end;
    for i := 1 to 7 do
    begin
      ch := p^; Inc(p);
      case ch of
        '0'..'1': v := 2 * v + Ord(ch) - $30;
        #1..#$20: Break;
        ')': Break;
        else
          DynError('Binary value required: %s', [ch]);
      end;
    end;
    List := Cons(v, List);
  until False;
  Datum := ListRevToDynMemory(List);
  Result := BuildToken(ps, p, Token, tkMem, Datum);
end;

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

  TokenKindTab['''']:= tkChar;
  TokenKindTab['"'] := tkStr;
  TokenKindTab['#'] := tkSharp;

  TokenKindTab[','] := tkCom;
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

  DefineIdents(['nil', 'false', 'true', 'or', 'xor',    'div', 'mod', 'and', 'shl', 'shr', 'as',    'in', 'is', 'not'],
              [tkNil, tkFalse, tkTrue, tkor, tkxor,    tkdiv, tkmod, tkand, tkshl, tkshr, tkas,    tkin, tkis, tkNot]);
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
      tkChar:
        Result := ScanChar(p, Token);
      tkSharp:                          // #
        case p[1] of
          'm': Result := ScanMem(p, Token);  // #m()
        else   Result := SingCharOp(p, Token, Kind);
        end;
      tkStr:
        Result := ScanStr(p, Token);
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

