unit RndParser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  Classes, SysUtils, RndBase, RndLexer;

(*
StatementList                = [Statement], {';', [Statement]} -> (Statement %1 %2)
                             ;
SimpleStatement              = Designator, ':=', Expression  -> (list set! %1 %3)
                             | Designator                    -> %1              // Call
                             ;
ExpressionList               = Expression, {',', Expression}  -> (list %1 & %2)
                             ;
Expression                   = SimpleExpression, [RelOp, SimpleExpression]   -> (LTR-opers %1 %2)
                             ;
SimpleExpression             = Term, {AddOp, Term}     -> (LTR-opers %1 %2)
                             ;
Term                         = Factor, {MulOp, Factor} -> (LTR-opers %1 %2)
                             ;
Factor                       = 'not', Factor           -> (list not %2)
                             | '+',   Factor           -> (list + %2)
                             | '-',   Factor           -> (list - %2)
                             | Number                  -> %1
                             | '(', Expression, ')'    -> %2
                             | Designator              -> %1
                             ;
Designator                   = Ident, {DesignatorItem} -> (LTR-opers %1 & %2)
                             ;
DesignatorItem               = '.', Ident                     -> (list . %2)
                             | '[', ExpressionList, ']'       -> (list* elem %2)
                             | '(', ExpressionList, ')'       -> (list* call %2)
                             ;
Ident

AddOp                        = ( '+' | '-' | 'or'  | 'xor' )                                -> %1
                             ;
MulOp                        = ( '*' | '/' | 'div' | 'mod' | 'and' | 'shl' | 'shr' | 'as' ) -> %1
                             ;
RelOp                        = ( '<' | '>' | '<='  | '>='  | '<>'  | '='   | 'in'  | 'is' ) -> %1
                             ;
_________________________________________________________________________________________________________
01_07-01-_Predictive_Parsing_19m37s.pdf

Left Factor(SimpleStatement)

SimpleStatement              = Designator, ':=', Expression  -> (list set! %1 %3)
                             | Designator                    -> %1              // Call
                             ;
-->

SimpleStatement              = Designator, SimpleStatement2
                             ;
SimpleStatement2             = ':=', Expression
                             | €
                             ;
_________________________________________________________________________________________________________

SimpleStatement              = Designator, [ ':=', Expression ] -> (SimpleStatement %1 %2)
                             ;
o

SimpleStatement              = Designator, ( ':=', Expression  -> (list set! %1 %3)
                                           | €                 -> %1              // Call
                                           )
                             ;

*)

type
  TParser = class
  protected
    Lexer: TLexer;
  protected
    // 04_06-04-_Recursive_Descent_Algorithm_13m28s.pdf
    CurToken: TTokenInfo;
    procedure Next;
    function Terminal(Kind: TTokenKind): Boolean;
    function PeekTerminal(Kind: TTokenKind): Boolean;
    // StatementList                = [Statement], {';', [Statement]} -> (Statement %1 %2)
    //                              ;
    function StatementList(var Res: IDatum): Boolean;
    function StatementList2(S1: IDatum; var Res: IDatum): Boolean;
    // Statement                    =  SimpleStatement
    //                              | Expression
    //                              ;
    function Statement(var Res: IDatum): Boolean;
    // SimpleStatement              = Designator, ':=', Expression  -> (list set! %1 %3)
    //                              | Designator                    -> %1              // Call
    //                              ;
    function SimpleStatement(var Res: IDatum): Boolean;
    function SimpleStatement2(S1: IDatum; var Res: IDatum): Boolean;
    // ExpressionList               = Expression, {',', Expression}  -> (list %1 & %2)
    //                              ;
    function ExpressionList(var Res: IDatum): Boolean;
    // Expression                   = SimpleExpression, [RelOp, SimpleExpression]   -> (LTR-opers %1 %2)
    //                              ;
    function Expression(var Res: IDatum): Boolean;
    // SimpleExpression             = Term, {AddOp, Term}     -> (LTR-opers %1 %2)
    //                              ;
    function SimpleExpression(var Res: IDatum): Boolean;
    // Term                         = Factor, {MulOp, Factor} -> (LTR-opers %1 %2)
    //                              ;
    function Term(var Res: IDatum): Boolean;
    // Factor                       = 'not', Factor           -> (list not %2)
    //                              | '+',   Factor           -> (list + %2)
    //                              | '-',   Factor           -> (list - %2)
    //                              | Number                  -> %1
    //                              | '(', Expression, ')'    -> %2
    //                              | Designator              -> %1
    //                              ;
    function Factor(var Res: IDatum): Boolean;
    // Designator                   = Ident, {DesignatorItem} -> (LTR-opers %1 & %2)
    //                              ;
    function Designator(var Res: IDatum): Boolean;
    // DesignatorItem               = '.', Ident                     -> (list . %2)
    //                              | '[', ExpressionList, ']'       -> (list* elem %2)
    //                              | '(', ExpressionList, ')'       -> (list* call %2)
    //                              ;
    function DesignatorItem(var Op, Item: IDatum): Boolean;
    // Ident
    function Ident(var Res: IDatum): Boolean;
    // AddOp                        = ( '+' | '-' | 'or'  | 'xor' )                                -> %1
    //                              ;
    function AddOp(var Res: IDatum): Boolean;
    // MulOp                        = ( '*' | '/' | 'div' | 'mod' | 'and' | 'shl' | 'shr' | 'as' ) -> %1
    //                              ;
    function MulOp(var Res: IDatum): Boolean;
    // RelOp                        = ( '<' | '>' | '<='  | '>='  | '<>'  | '='   | 'in'  | 'is' ) -> %1
    //                              ;
    function RelOp(var Res: IDatum): Boolean;
  public
    constructor Create(s: String);
    destructor Destroy; override;
    function GetNextTerm(out Term: IDatum): Boolean;
  end;

implementation

var
  sySetX, SyPoint, SyElem, SyCall: ISymbol;

{ TParser }

constructor TParser.Create(s: String);
begin
  Lexer := TLexer.Create(s);
  sySetX := MakeSymbol('set!');
  SyPoint := MakeSymbol('.');
  SyElem := MakeSymbol('elem');
  SyCall := MakeSymbol('call');
end;

destructor TParser.Destroy;
begin
  Lexer.Free;
  inherited Destroy;
end;

function TParser.GetNextTerm(out Term: IDatum): Boolean;
begin
  Result := False;
  if Lexer.GetNext(CurToken) then
    if StatementList(Term) then
      Result := True;
end;

procedure TParser.Next;
begin
  Lexer.GetNext(CurToken)
end;

function TParser.Terminal(Kind: TTokenKind): Boolean;
begin
  Result := (CurToken.Kind = Kind);
  if Result then
    Next;
end;

function TParser.PeekTerminal(Kind: TTokenKind): Boolean;
begin
  Result := (CurToken.Kind = Kind);
end;

function TParser.StatementList(var Res: IDatum): Boolean;
begin
  // StatementList                = [Statement], {';', [Statement]} -> (Statement %1 %2)
  //                              ;
  Result := Statement(Res);
  if PeekTerminal(tkSeCo) then
    Result := StatementList2(Res, Res)
end;

function TParser.StatementList2(S1: IDatum; var Res: IDatum): Boolean;
var
  Item: IDatum;
  List: INode;
begin
  List := cons(S1, nil);
  while Terminal(tkSeCo) do
  begin
    if not Statement(Res) then
      Break;
    List := cons(Item, List);
  end;
  List := Reverse(List); // invertir porque los items se agregan al principio
  Result := True;
end;

function TParser.Statement(var Res: IDatum): Boolean;
begin
  // Statement                    =  SimpleStatement
  //                              | Expression
  //                              ;
  if CurToken.Kind = tkIdent then
    Result := SimpleStatement(Res)
  else
    Result := Expression(Res)  // permitir evaluar cosas como 2+2 que no son statements
end;

function TParser.SimpleStatement(var Res: IDatum): Boolean;
begin
  // SimpleStatement              = Designator, ':=', Expression  -> (list set! %1 %3)
  //                              | Designator                    -> %1              // Call
  //                              ;
  {_________________________________________________________________________________________________________
  01_07-01-_Predictive_Parsing_19m37s.pdf

  Left Factor(SimpleStatement)
  -->

  SimpleStatement              = Designator, SimpleStatement2
                               ;
  SimpleStatement2(%1)         = ':=', Expression                -> (list set! %1 %3)
                               | €                               -> %1              // Call
                               ;
  _________________________________________________________________________________________________________}

  Result := Designator(Res);
  if PeekTerminal(tkAsig) then
    Result := SimpleStatement2(Res, Res)
end;

function TParser.SimpleStatement2(S1: IDatum; var Res: IDatum): Boolean;
begin
  Terminal(tkAsig);
  if Expression(Res) then
  begin
    Res := List([sySetX, S1, Res]);
    Result := True;
  end
  else
    Result := False;  // := de más al final, es error
end;

function TParser.ExpressionList(var Res: IDatum): Boolean;
var
  Item: IDatum;
  L: INode;
begin
  // ExpressionList               = Expression, {',', Expression}  -> (list %1 & %2)
  Result := Expression(Item);
  if not Result then Exit;

  L := cons(Item, nil);
  while Terminal(tkCom) do
    if Expression(Item) then
      L := cons(Item, L)
    else
      Break;
  Res := Reverse(L);
end;

function TParser.Expression(var Res: IDatum): Boolean;
var
  Op, Item: IDatum;
begin
  // Expression                   = SimpleExpression, [RelOp, SimpleExpression]   -> (LTR-opers %1 %2)
  Result := SimpleExpression(Res);
  if not Result then Exit;

  if RelOp(Op) then
    if SimpleExpression(Item) then
      Res := List([Op, Res, Item]);
end;

function TParser.SimpleExpression(var Res: IDatum): Boolean;
var
  Op, Item: IDatum;
begin
  // SimpleExpression             = Term, {AddOp, Term}     -> (LTR-opers %1 %2)
  Result := Term(Res);
  if not Result then Exit;

  while AddOp(Op) do
  begin
    Result := Term(Item);
    if not Result then Exit;
    Res := List([Op, Res, Item]);
  end;
end;

function TParser.Term(var Res: IDatum): Boolean;
var
  Op, Item: IDatum;
begin
  // Term                         = Factor, {MulOp, Factor} -> (LTR-opers %1 %2)
  Result := Factor(Res);
  if not Result then Exit;

  while MulOp(Op) do
  begin
    Result := Factor(Item);
    if not Result then Exit;
    Res := List([Op, Res, Item]);
  end;
end;

function TParser.Factor(var Res: IDatum): Boolean;
var
  Op, Item: IDatum;
begin
  // Factor                       = 'not', Factor           -> (list not %2)
  //                              | '+',   Factor           -> (list + %2)
  //                              | '-',   Factor           -> (list - %2)
  //                              | Number                  -> %1
  //                              | '(', Expression, ')'    -> %2
  //                              | Designator              -> %1
  //                              ;
  case CurToken.Kind of
    tkNot, tkAdd, tkSub:
      begin
        Op := CurToken.Val;
        Next;

        Result := Factor(Item);
        if Result then
          Res := List([Op, Item]);
      end;
    tkNum:
      begin
        Res := CurToken.Val;
        Next;
        Result := True;
      end;
    tkLPar:                       // '(', Expression, ')'    -> %2
      begin
        Next;
        Result := Expression(Res);
        if Result then
          Result := Terminal(tkRPar);
      end;
    else
      Result := Designator(Res);
  end;
end;

function TParser.Designator(var Res: IDatum): Boolean;
var
  Op, Item: IDatum;
begin
  // Designator                   = Ident, {DesignatorItem} -> (LTR-opers %1 & %2)
  Result := Ident(Res);
  while DesignatorItem(Op, Item) do
  begin
    Res := List([Op, Res, Item]);
  end;
end;

function TParser.DesignatorItem(var Op, Item: IDatum): Boolean;
begin
  // DesignatorItem               = '.', Ident                     -> (list . %2)
  //                              | '[', ExpressionList, ']'       -> (list* elem %2)
  //                              | '(', ExpressionList, ')'       -> (list* call %2)
  //                              ;
  Result := False;
  case CurToken.Kind of
    tkPoint:
      begin
        Op := SyPoint;
        Next;
        Result := Ident(Item);
      end;
    tkLSqBr:
      begin
        Op := SyElem;
        Next;
        if not ExpressionList(Item) then
          Exit;
        Result := Terminal(tkRSqBr);
      end;
    tkLPar:
      begin
        Op := SyCall;
        Next;
        if not ExpressionList(Item) then
          Exit;
        Result := Terminal(tkRPar);
      end;
  end;
end;

function TParser.Ident(var Res: IDatum): Boolean;
begin
  Result := (CurToken.Kind = tkIdent);
  if not Result then
    Exit;
  Res := CurToken.Val;
  Next;
end;

function TParser.AddOp(var Res: IDatum): Boolean;
begin
  // AddOp                        = ( '+' | '-' | 'or'  | 'xor' )                                -> %1
  Result := False;
  case CurToken.Kind of
    tkAdd, tkSub, tkor, tkxor:
      begin
        Res := CurToken.Val;
        Next;
        Result := True;
      end;
  end;
end;

function TParser.MulOp(var Res: IDatum): Boolean;
begin
  // MulOp                        = ( '*' | '/' | 'div' | 'mod' | 'and' | 'shl' | 'shr' | 'as' ) -> %1
  Result := False;
  case CurToken.Kind of
    tkMul, tkDvd, tkdiv, tkmod, tkand, tkshl, tkshr, tkas:
      begin
        Res := CurToken.Val;
        Next;
        Result := True;
      end;
  end;
end;

function TParser.RelOp(var Res: IDatum): Boolean;
begin
  // RelOp                        = ( '<' | '>' | '<='  | '>='  | '<>'  | '='   | 'in'  | 'is' ) -> %1
  Result := False;
  case CurToken.Kind of
    tkLt, tkGt, tkLEq, tkGEq, tkDif, tkEq, tkin, tkis:
      begin
        Res := CurToken.Val;
        Next;
        Result := True;
      end;
  end;
end;

end.
