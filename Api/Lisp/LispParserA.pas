unit LispParserA;
interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils,
  DynTypes, DTPair, DTParser, LispParser;

type
  TTokenKind   = DTParser.TTokenKind;
  TTokenInfo   = DTParser.TTokenInfo;

  IParser = interface(IInterface)
    function GetNextTerm(p: PAnsiChar; out Res: TTokenInfo): PAnsiChar;
  end;

  TLispParser = class(TCustomLispParser, IParser)
  public
    function GetNextTerm(p: PAnsiChar; out Res: TTokenInfo): PAnsiChar; overload;
  public
    constructor Create;
    function Evaluate(out AResult: TDatumRef; const s: RawByteString): PAnsiChar; overload;
    function Evaluate(out AResult: TDatumRef; p: PAnsiChar; cc: Integer): PAnsiChar; overload;
  end;

  PTChar = PAnsiChar;

  TLazyLispParser = class(TLispParser)
  protected
    function EvalList(var Res: TDatumRef): Boolean; override;
    function EvalLater(p: PTChar; var Res: TDatumRef; Last: PDatumRef): PTChar; virtual; abstract;
  public
    MaxItemsToEval: Integer;
  end;

  TLazyNode = class(TDynPair)
  public
    function DatumType: TDatumType; override;
  public
    Parser: IParser;
    pNext: PTChar;
    constructor Create(const AParser: IParser; Value: TDynDatum; p: PTChar);
    function Rest: IDynSeq; override;
    function Getcdr: TDynDatum; override;
  end;


var
  QuoteFn: array[TQuoteTokens] of TDatumRef;
procedure InitQuoteFn;

function LazyNode(Parser: IParser; p: PTChar): TLazyNode;

implementation /////////////////////////////////////////////////////////////////

uses
  LispLexerA;

procedure InitQuoteFn;
var
  i: TQuoteTokens;
  Obj: IDynSymbol;
begin
  if QuoteFn[High(QuoteFn)].Ptr <> nil then Exit;
  //if IsSymbol(QuoteFn[High(QuoteFn)].Value) then Exit;
  for i := Low(QuoteFn) to High(QuoteFn) do
  begin
    Obj := InitSymbol(QuoteFnName[i]);
    QuoteFn[i] := Obj;
  end;
end;

{ TLispParser }

constructor TLispParser.Create;
begin
  Lexer := TLexer.Create;
  InitQuoteFn;
end;

function TLispParser.Evaluate(out AResult: TDatumRef; const s: RawByteString):
    PAnsiChar;
var
  p: PAnsiChar;
  cc: Integer;
begin
  p := Pointer(s);
  cc := Length(s);
  TLexer(Lexer).Init(p, cc);
  Eval(AResult);
  Result := TLexer(Lexer).CurPos;
end;

function TLispParser.Evaluate(out AResult: TDatumRef; p: PAnsiChar;
  cc: Integer): PAnsiChar;
begin
  TLexer(Lexer).Init(p, cc);
  Eval(AResult);
  Result := TLexer(Lexer).CurPos;
end;

function TLispParser.GetNextTerm(p: PAnsiChar; out Res: TTokenInfo): PAnsiChar;
begin
  TLexer(Lexer).CurPos := p;
  inherited GetNextTerm(Res);
  Result := TLexer(Lexer).CurPos;
end;

{ TLazyLispParser }

function TLazyLispParser.EvalList(var Res: TDatumRef): Boolean;
var
  Pair: IDynDatum;
  Ref: PDatumRef;
  Token: TTokenInfo;
  i: Integer;
begin
  Result := True;

  Ref := @Res;
  for i := 0 to MaxItemsToEval - 1 do
  begin
    Result := GetNextTerm(Token);
    case Token.Kind of
      tkDatum: ;
      else //tkEnd, tkRPar,
        Ref^ := (_Null);
        Exit;
    end;
    Pair := cons(Token.Ref.Value, _null);
    Ref^ := (Pair);
    Ref := CdrRef(Pointer(Pair));
  end;
  EvalLater(TLexer(Lexer).CurPos, Res, Ref);
end;

function LazyNode(Parser: IParser; p: PTChar): TLazyNode;
var
  Token: TTokenInfo;
begin
  p := Parser.GetNextTerm(p, Token);
  case Token.Kind of
    tkDatum:
      Result := TLazyNode.Create(Parser, Token.Ref.Value, p);
    else //tkEnd, tkRPar,
      Result := nil
  end;
end;

{ TLazyNode }

constructor TLazyNode.Create(const AParser: IParser; Value: TDynDatum; p: PTChar);
begin
  Parser := AParser;
  FCar := (Value);
  pNext := p;
end;

function TLazyNode.DatumType: TDatumType;
begin
  Result := atSeq
end;

function TLazyNode.Getcdr: TDynDatum;
begin
  if FCdr = nil then
    Setcdr(Pointer(Rest));
  Result := FCdr.Ptr;
end;

function TLazyNode.Rest: IDynSeq;
var
  Node: TLazyNode;
begin
  Node := LazyNode(Parser, pNext);
  Result := Node.AsIDynSeq;
end;

end. ///////////////////////////////////////////////////////////////////////////

