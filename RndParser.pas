unit RndParser;

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
  end;

  TTokenizer = class
  protected
    Buf: String;
    pStart, pCurr, pEnd: PChar;
  public
    constructor Create(s: String);
    function GetNext(out Token: TTokenInfo): Boolean;
  end;

  TParser = class
  protected
    Tokenizer: TTokenizer;
  public
    constructor Create(s: String);
    destructor Destroy; override;
    function GetNextTerm(out Term: IDatum): Boolean;
    function GetNextList: INode;
  end;

implementation

{ TTokenizer }

constructor TTokenizer.Create(s: String);
begin
  Buf := s;
  pStart := Pointer(Buf);
  pCurr := pStart;
  pEnd := pStart + Length(Buf);
end;

function TTokenizer.GetNext(out Token: TTokenInfo): Boolean;
var
  p: PChar;
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
    Token.pStart := p;
    case p^ of
      '(': begin
             Token.Kind := tkLPar;
             Inc(p);
           end;
      ')': begin
             Token.Kind := tkRPar;
             Inc(p);
           end;
    else   begin
             Token.Kind := tkOther;
             repeat
               Inc(p);
               case p^ of
                 '(', ')', #0..#$20: break;
               end;
             until (p >= pEnd);
           end;
    end;
    pCurr := p;
    Token.len := p - Token.pStart;
    Result := True;
    Exit;
  end;
end;

{ TParser }

constructor TParser.Create(s: String);
begin
  Tokenizer := TTokenizer.Create(s);
end;

destructor TParser.Destroy;
begin
  Tokenizer.Free;
  inherited Destroy;
end;

function TParser.GetNextList: INode;
var
  Term: IDatum;
begin
  Result := nil;
  while GetNextTerm(Term) do
    Result := cons(Term, Result);
  Result := Reverse(Result);
end;

function TParser.GetNextTerm(out Term: IDatum): Boolean;
var
  Token: TTokenInfo;
  p: PChar;
  s: String;
  Num: Double;
begin
  Result := False;
  if Tokenizer.GetNext(Token) then
  begin
    case Token.Kind of
      tkLPar: Term := GetNextList;
      tkRPar: Exit;
      else
        p := Token.pStart;
        SetString(s, p, Token.len);
        case p^ of
          '-', '0'..'9':
            if TryStrToFloat(s, Num) then
              Term := MakeNumber(Num)
            else
              Term := MakeSymbol(s);
        else
              Term := MakeSymbol(s);
        end;
    end;
    Result := True;
  end;
end;

end.
