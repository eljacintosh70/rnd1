unit LispParser;
interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils,
  DynTypes, DTParser;

type
  TTokenKind   = DTParser.TTokenKind;
  TTokenInfo   = DTParser.TTokenInfo;

  IParser = interface(IInterface)
    function GetNextTerm(p: PWideChar; out Res: TTokenInfo): PWideChar;
  end;

  TCustomLispParser = class(TInterfacedObject)
  protected
    Lexer: TCustomLexer;
    function GetNextTerm(out Res: TTokenInfo): Boolean; overload;
    function EvalList(var Res: TDatumRef): Boolean; virtual;
    function Eval(out Res: TDatumRef): Boolean;
  public
    destructor Destroy; override;
  end;

  TLispParser = class(TCustomLispParser, IParser)
  public
    function GetNextTerm(p: PWideChar; out Res: TTokenInfo): PWideChar;
  public
    constructor Create;
    function Evaluate(out AResult: TDatumRef; const s: String): PWideChar; overload;
    function Evaluate(out AResult: TDatumRef; p: PWideChar; cc: Integer): PWideChar; overload;
  end;

const
  QuoteFnName: array[TQuoteTokens] of Utf8String = (
    'quote', 'quasiquote', 'unquote', 'unquote-splicing');
var
  QuoteFn: array[TQuoteTokens] of TDatumRef;

procedure InitQuoteFn;

implementation /////////////////////////////////////////////////////////////////

uses
  LispLexer;

procedure InitQuoteFn;
var
  i: TQuoteTokens;
begin
  if IsSymbol(QuoteFn[High(QuoteFn)].Value) then Exit;
  for i := Low(QuoteFn) to High(QuoteFn) do
    QuoteFn[i] := (InitSymbol(QuoteFnName[i]));
end;

{ TCustomLispParser }

function TCustomLispParser.GetNextTerm(out Res: TTokenInfo): Boolean;
var
  TokenKind: TTokenKind;
  L: IDynPair;
begin
  Result := Lexer.GetNext(Res);
  TokenKind := Res.Kind;
  case TokenKind of
    tkLPar:
      begin
        Result := EvalList(Res.Ref);
        Res.Kind := tkDatum;
      end;
    tkVector:
      begin
        Result := EvalList(Res.Ref);
        Res.Ref := (ListToDynArray(Res.Ref.Value));
        Res.Kind := tkDatum;
      end;
    Low(TQuoteTokens)..High(TQuoteTokens):
      begin
        Result := GetNextTerm(Res);
        L := Cons(Res.Ref.Value, _null);
        Res.Ref.Cons(QuoteFn[TokenKind].Value, L);
      end;
  end;
end;

function TCustomLispParser.EvalList(var Res: TDatumRef): Boolean;
var
  Token: TTokenInfo;
  List: IDynPair;
begin
  List := nil;
  repeat
    Result := GetNextTerm(Token);
    case Token.Kind of
      tkDatum: ;
      else //tkEnd, tkRPar,
        Res := (Reverse(List));
        Break;
    end;

    List := cons(Token.Ref.Value, List);
  until (False);
end;

function TCustomLispParser.Eval(out Res: TDatumRef): Boolean;
var
  Token: TTokenInfo;
begin
  Result := GetNextTerm(Token);
  Res := (Token.Ref);
end;

destructor TCustomLispParser.Destroy;
begin
  Lexer.Free;
end;

{ TLispParser }

constructor TLispParser.Create;
begin
  Lexer := TLexer.Create;
  InitQuoteFn;
end;

function TLispParser.Evaluate(out AResult: TDatumRef; const s: String): PWideChar;
var
  p: PWideChar;
  cc: Integer;
begin
  p := Pointer(s);
  cc := Length(s);
  TLexer(Lexer).Init(p, cc);
  Eval(AResult);
  Result := TLexer(Lexer).CurPos;
end;

function TLispParser.Evaluate(out AResult: TDatumRef; p: PWideChar;
  cc: Integer): PWideChar;
begin
  TLexer(Lexer).Init(p, cc);
  Eval(AResult);
  Result := TLexer(Lexer).CurPos;
end;

function TLispParser.GetNextTerm(p: PWideChar; out Res: TTokenInfo): PWideChar;
begin
  TLexer(Lexer).CurPos := p;
  inherited GetNextTerm(Res);
  Result := TLexer(Lexer).CurPos;
end;

end. ///////////////////////////////////////////////////////////////////////////

