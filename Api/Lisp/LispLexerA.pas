unit LispLexerA;

interface

uses
  SysUtils,
  DynTypes, DTParser;

type
  TChar = AnsiChar;
  PTChar = PAnsiChar;

  TLexer = class(TCustomLexer)
  protected
    pStart: PTChar;
    pCurr: PTChar;
    pEnd: PTChar;
    function ScanIntB(p: PTChar; out Res: TDatumRef): PTChar;
    function ScanIntO(p: PTChar; out Res: TDatumRef): PTChar;
    function ScanIntX(p: PTChar; out Res: TDatumRef): PTChar;
    function ScanMem(p: PTChar; out Res: TDatumRef): PTChar;
    function ScanMem8(p: PTChar; out Res: TDatumRef): PTChar;
    function ScanMem8b(p: PTChar; out Res: TDatumRef): PTChar;
    function ScanNum(p: PTChar; out Res: TDatumRef): PTChar;
    function ScanIdent(p: PTChar; out Res: TDatumRef): PTChar;
    function ScanString(p: PTChar; out Res: TDatumRef): PTChar;
  public
    procedure Init(p: PTChar; cc: Integer);
    function GetNext(out Res: TTokenInfo): Boolean; override;
    property CurPos: PTChar read pCurr write pCurr;
  end;

implementation

{ TLexer }

function TLexer.ScanIntB(p: PTChar; out Res: TDatumRef): PTChar;
var
  X: Int64;
begin
  Result := DTParser.ScanIntB(p, X);
  CreateInt(X, Res);
end;

function TLexer.ScanIntO(p: PTChar; out Res: TDatumRef): PTChar;
var
  X: Int64;
begin
  Result := DTParser.ScanIntO(p, X);
  CreateInt(X, Res);
end;

function TLexer.ScanIntX(p: PTChar; out Res: TDatumRef): PTChar;
var
  X: Int64;
begin
  Result := DTParser.ScanIntX(p, X);
  CreateInt(X, Res);
end;

function TLexer.ScanMem(p: PTChar; out Res: TDatumRef): PTChar;
begin
  case p^ of
    ':':
      begin
        Inc(p);
        case p^ of
          'b', 'B':
             begin
               Inc(p);
               p := NeedChar(p, '(');
               Result := ScanMem8b(p, res);
             end;
        else
          DynError('NeedChar "b" after #m:', []);
          Result := p;
        end;
      end;
    '(':
      begin
        Inc(p);
        Result := ScanMem8(p, res);
      end;
  else
    DynError('NeedChar "(" or ":" after #m', []);
    Result := p;
  end;
end;

function TLexer.ScanMem8(p: PTChar; out Res: TDatumRef): PTChar;
var
  ch: TChar;
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
  Res.Assign(Datum);
  Result := p;
end;

function TLexer.ScanMem8b(p: PTChar; out Res: TDatumRef): PTChar;
var
  ch: TChar;
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
  Res.Assign(Datum);
  Result := p;
end;

function TLexer.ScanNum(p: PTChar; out Res: TDatumRef): PTChar;
var
  ps: PTChar;
  s: String;
  X: Real;
begin
  ps := p;
  if (p^ = '-') then                 // [- ]
    Inc(p);
  while IsDigit(p^) do               // [n*]
    Inc(p);
  if (p^ = '.') then                 // [.[n*]]
    repeat
      Inc(p);
    until not IsDigit(p^);
  case p^ of                         // [E
    'e', 'E':
      begin
        Inc(p);
        case p^ of                   //   [(+-)]
         '+', '-': Inc(p);
        end;
        while IsDigit(p^) do         //   [n*]
          Inc(p);
      end;
  end;                               // ]
  SetString(s, ps, p - ps);
  X := StrToFloat(s);

  { TODO : por ahora... cambiar para que 1.0 -> real 1E+0 -> real}
  if X = Trunc(X) then
    Res.Assign(MakeInt64(Trunc(X)))
  else
    Res.Assign(MakeDouble(X));
  Result := p;
end;

function TLexer.ScanIdent(p: PTChar; out Res: TDatumRef): PTChar;
var
  ps: PTChar;
  s: String;
  pe: PTChar;
begin
  ps := p;
  pe := pEnd;
  while p < pe do
    case p^ of
      #0..#$20,
      '[', ']',
      '(', ')':
        Break;
      else
        Inc(p);
    end;

  SetString(s, ps, p - ps);
  Res.Assign(InitSymbol(Utf8String(s)));
  Result := p;
end;

function TLexer.ScanString(p: PTChar; out Res: TDatumRef): PTChar;
var
  ps: PTChar;
  pe: PTChar;
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
          Res.Assign(s);
          Inc(p);
          Result := p;
          Exit;
        end;
      else
        Inc(p);
    end;

  s := make_string(ps, p - ps);
  Res.Assign(s);
  Result := p;
end;

function TLexer.GetNext(out Res: TTokenInfo): Boolean;
// http://www.r6rs.org/final/html/r6rs/r6rs-Z-H-7.html
var
  p: PTChar;
  pe: PTChar;
  ch: TChar;
begin
  p := pCurr;
  pe := pEnd;
  Res.Kind := tkDatum;
  Res.Ref.Assign(Unbound);
  while p < pe do
  begin
    ch := p^;
    Inc(p);
    case ch of
      '''':     // '
        Res.Kind := tkQuote;
      '`':      // `
        Res.Kind := tkQuasiQuote;
      ',':
        begin
          if p^ = '@' then
          begin
            Inc(p);
            Res.Kind := tkUnQuoteSplicing;  // ,@
          end
          else
            Res.Kind := tkUnQuote;          // ,
          Res.Ref.Assign(Unbound);
        end;
      #0..#$20:
        begin
          repeat
            case p^ of
              #0:
                if p >= pe then Break;
              #1..#$20:
                ;
              else
                Break;
            end;
            Inc(p);
          until (False);
          Continue;
        end;
      ';':
        begin
          repeat
            case p^ of
              #0:
                if p >= pe then Break;
              #10, #13:
                Break;
            end;
            Inc(p);
          until (False);
          Continue;
        end;
      '(', '[':
          Res.Kind := tkLPar;
      ')', ']':
        begin
          Res.Kind := tkRPar;
          Res.Ref.Assign(_null);
        end;
      // '\':
      '#':
        begin
          ch := p^;
          Inc(p);
          case Upcase(ch) of
            {  I  inexact
               E  exact
             }
            'B':  // #b  <radix 2>
              p := ScanIntB(p, Res.Ref);
            'O':  // #o  <radix 8>
              p := ScanIntO(p, Res.Ref);
            'D':  // #d  <radix 10>
              p := ScanNum(p, Res.Ref);
            'X':  // #x  <radix 16>
              p := ScanIntX(p, Res.Ref);
            'M':
              p := ScanMem(p, Res.Ref);
            '\':
              begin
                ch := p^;
                Inc(p);
                case ch of
                  'x':
                    case p^ of
                      '0'..'9', 'A'..'F', 'a'..'f':
                        begin
                          p := ScanIntX(p, Res.Ref);
                          Res.Ref.Assign(MakeChar(
                            WideChar(FixNumValue(Res.Ref.Value))));
                        end;
                      else
                        Res.Ref.Assign(MakeChar(WideChar(ch)));
                    end;
                  else
                    Res.Ref.Assign(MakeChar(WideChar(ch)));
                end;
              {
                \c  char
                \name
                \xHexCode

                 #\nul 	U+0000
                 #\alarm 	U+0007
                 #\backspace 	U+0008
                 #\tab 	U+0009
                 #\linefeed 	U+000A
                 #\newline 	U+000A
                 #\vtab 	U+000B
                 #\page 	U+000C
                 #\return 	U+000D
                 #\esc 	U+001B
                 #\space 	U+0020
                 #\delete 	U+007F
              }
              end;
            'T':
              Res.Ref.Assign(_t);
            'F':
              Res.Ref.Assign(_f);
            'V':                    // #vu8(
              if (Upcase(p[0]) = 'U') then
                if (p[1] = '8') then
                begin
                  Res.Kind := tkByteVector;
                  Inc(p, 2);
                end;
            '(', '[':               // #(  => vector
              Res.Kind := tkVector;
            else
              p := ScanIdent(p - 2, Res.Ref);
          end;
        end;
      '0'..'9':
        p := ScanNum(p - 1, Res.Ref);
      '-':
        case p^ of
          '0'..'9':
            p := ScanNum(p - 1, Res.Ref);
          else
            p := ScanIdent(p - 1, Res.Ref);
        end;
      '"':
        p := ScanString(p - 1, Res.Ref);
      else
        p := ScanIdent(p - 1, Res.Ref);
    end;
    pCurr := p;
    Result := True;
    Exit;
  end;
  Res.Kind := tkEnd;
  Res.Ref.Assign(Unbound);
  pCurr := p;
  Result := False;
end;

procedure TLexer.Init(p: PTChar; cc: Integer);
begin
  pStart := p;
  pCurr := p;
  pEnd := p + cc;
end;

end.

