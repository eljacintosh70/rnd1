unit RndParserL;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, RndBase, RndLexerL;

type
  TParser = class
  protected
    Lexer: TLexer;
  public
    constructor Create(s: String);
    destructor Destroy; override;
    function GetNextTerm(out Term: IDatum): Boolean;
    function GetNextList: INode;
  end;

implementation

{ TParser }

constructor TParser.Create(s: String);
begin
  Lexer := TLexer.Create(s);
end;

destructor TParser.Destroy;
begin
  Lexer.Free;
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
begin
  Result := False;
  if Lexer.GetNext(Token) then
  begin
    case Token.Kind of
      tkLPar: Term := GetNextList;
      tkRPar: Exit;
      else
        Term := Token.Val;
    end;
    Result := True;
  end;
end;

end.
