unit RndParser;

{$IFDEF FPC}
  {$MODE Delphi}
{$ENDIF}

interface

uses
  SysUtils,
  DynTypes, RndLexer;

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
    function StatementList(var Res: dyn): Boolean;
    function StatementList2(S1: dyn; var Res: dyn): Boolean;
  //public
    // Statement                    =  SimpleStatement
    //                              | Expression
    //                              ;
    function Statement(var Res: dyn): Boolean;
  protected
    // SimpleStatement              = Designator, ':=', Expression  -> (list set! %1 %3)
    //                              | Designator                    -> %1              // Call
    //                              ;
    function SimpleStatement(var Res: dyn): Boolean;
    function SimpleStatement2(S1: dyn; var Res: dyn): Boolean;
    // ExpressionList               = Expression, {',', Expression}  -> (list %1 & %2)
    //                              ;
    function ExpressionList(var Res: dyn): Boolean;
    // Expression                   = SimpleExpression, [RelOp, SimpleExpression]   -> (LTR-opers %1 %2)
    //                              ;
    function Expression(var Res: dyn): Boolean;
    // SimpleExpression             = Term, {AddOp, Term}     -> (LTR-opers %1 %2)
    //                              ;
    function SimpleExpression(var Res: dyn): Boolean;
    // Term                         = Factor, {MulOp, Factor} -> (LTR-opers %1 %2)
    //                              ;
    function Term(var Res: dyn): Boolean;
    // Factor                       = 'not', Factor           -> (list not %2)
    //                              | '+',   Factor           -> (list + %2)
    //                              | '-',   Factor           -> (list - %2)
    //                              | Number                  -> %1
    //                              | '[', ExpressionList, ']' -> %2
    //                              | '(', ExpressionList, ')' -> %2
    //                              | Designator              -> %1
    //                              ;
    function Factor(var Res: dyn): Boolean;
    // Designator                   = Ident, {DesignatorItem} -> (LTR-opers %1 & %2)
    //                              ;
    function Designator(var Res: dyn): Boolean;
    // DesignatorItem               = '.', Ident                     -> (list . %2)
    //                              | '[', ExpressionList, ']'       -> (list* elem %2)
    //                              | '(', ExpressionList, ')'       -> (list* call %2)
    //                              ;
    function DesignatorItem(var Op, Item: dyn): Boolean;
    // Ident
    function Ident(var Res: dyn): Boolean;
    // AddOp                        = ( '+' | '-' | 'or'  | 'xor' )                                -> %1
    //                              ;
    function AddOp(var Res: dyn): Boolean;
    // MulOp                        = ( '*' | '/' | 'div' | 'mod' | 'and' | 'shl' | 'shr' | 'as' ) -> %1
    //                              ;
    function MulOp(var Res: dyn): Boolean;
    // RelOp                        = ( '<' | '>' | '<='  | '>='  | '<>'  | '='   | 'in'  | 'is' ) -> %1
    //                              ;
    function RelOp(var Res: dyn): Boolean;
  public
    constructor Create(s: String);
    destructor Destroy; override;
    function GetNextTerm(out Term: dyn): Boolean;
  end;

implementation

var
  syBegin, syAsign, syDefine, SyPoint, SyElem, SyCall: dyn;

  DebStr: string;

{ TParser }

constructor TParser.Create(s: String);
begin
  Lexer := TLexer.Create(s);
  syBegin := MakeSymbol('begin');
  syDefine := MakeSymbol('define');
  syAsign := MakeSymbol(':=');
  SyPoint := MakeSymbol('.');
  SyElem := MakeSymbol('elem');
  SyCall := MakeSymbol('call');
end;

destructor TParser.Destroy;
begin
  Lexer.Free;
  inherited Destroy;
end;

function TParser.GetNextTerm(out Term: dyn): Boolean;
begin
  Result := False;
  if Lexer.GetNext(CurToken) then
    if StatementList(Term) then
    begin
      if list_length(Term) = 1 then
        Term := car(Term)
      else
        Term := cons(SyBegin, Term);
      Result := True;
    end;
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

function TParser.StatementList(var Res: dyn): Boolean;
begin
  // StatementList                = [Statement], {';', [Statement]} -> (Statement %1 %2)
  //                              ;
  Result := Statement(Res);
  if PeekTerminal(tkSeCo) then
    Result := StatementList2(Res, Res)
  else
    if Result then
      Res := cons(Res, nil);
  //DebStr := Deb(Res);
end;

function TParser.StatementList2(S1: dyn; var Res: dyn): Boolean;
var
  S2: dyn;
  List: IDynPair;
begin
  List := cons(S1, nil);
  while Terminal(tkSeCo) do
  begin
    if not Statement(S2) then
      Break;
    List := cons(S2, List);
  end;
  Res := Reverse(List); // invertir porque los items se agregan al principio
  Result := True;
end;

function TParser.Statement(var Res: dyn): Boolean;
begin
  // Statement                    =  SimpleStatement
  //                              | Expression
  //                              ;
  {if CurToken.Kind = tkIdent then
  begin
    Result := SimpleStatement(Res);
  end
  else
    Result := Expression(Res)  // permitir evaluar cosas como 2+2 que no son statements
  }

  Result := Expression(Res);
  if PeekTerminal(tkAsig) then
    Result := SimpleStatement2(Res, Res)
end;

function TParser.SimpleStatement(var Res: dyn): Boolean;
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

function TParser.SimpleStatement2(S1: dyn; var Res: dyn): Boolean;
begin
  Terminal(tkAsig);
  if Expression(Res) then
  begin
    Res := List([syAsign, S1, Res]);
    Result := True;
  end
  else
    Result := False;  // := de más al final, es error
end;

function TParser.ExpressionList(var Res: dyn): Boolean;
var
  Item: dyn;
  L: IDynPair;
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

function TParser.Expression(var Res: dyn): Boolean;
var
  Op, Item: dyn;
begin
  // Expression                   = SimpleExpression, [RelOp, SimpleExpression]   -> (LTR-opers %1 %2)
  Result := SimpleExpression(Res);
  if not Result then Exit;

  if RelOp(Op) then
    if SimpleExpression(Item) then
      Res := List([Op, Res, Item]);

  // !!!!! bueno... estoy colocando el asign como expresión... no debería
  if PeekTerminal(tkAsig) then
    Result := SimpleStatement2(Res, Res)
end;

function TParser.SimpleExpression(var Res: dyn): Boolean;
var
  Op, Item: dyn;
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

function TParser.Term(var Res: dyn): Boolean;
var
  Op, Item: dyn;
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

function TParser.Factor(var Res: dyn): Boolean;
var
  Op, Item: dyn;
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
    tkNil:
      begin
        Res := nil;
        Next;
        Result := True;
      end;
    tkFalse:
      begin
        Res := _f;
        Next;
        Result := True;
      end;
    tkTrue:
      begin
        Res := _t;
        Next;
        Result := True;
      end;
    tkNum, tkChar, tkStr, tkMem:
      begin
        Res := CurToken.Val;
        Next;
        Result := True;
      end;
    tkLPar:                       // '(', ExpressionList, ')'   -> (list *%2)
      begin
        Next;
        Result := ExpressionList(Res);
        if Result then
          Result := Terminal(tkRPar);
      end;
    tkLSqBr:                    // '[', ExpressionList, ']'    -> (dynarray *%2)
      begin
        Op := SyElem;
        Next;
        Result := ExpressionList(Res);
        if not Result then
          Exit;
        Res := ListToDynArray(Res);
        Result := Terminal(tkRSqBr);
      end;
    else
      Result := Designator(Res);
  end;
end;

function TParser.Designator(var Res: dyn): Boolean;
var
  Op, Item: dyn;
begin
  // Designator                   = Ident, {DesignatorItem} -> (LTR-opers %1 & %2)
  Result := Ident(Res);
  while DesignatorItem(Op, Item) do
  begin
    Res := List([Op, Res, Item]);
  end;
end;

function TParser.DesignatorItem(var Op, Item: dyn): Boolean;
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

function TParser.Ident(var Res: dyn): Boolean;
begin
  Result := (CurToken.Kind = tkIdent);
  if not Result then
    Exit;
  Res := CurToken.Val;
  Next;
end;

function TParser.AddOp(var Res: dyn): Boolean;
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

function TParser.MulOp(var Res: dyn): Boolean;
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

function TParser.RelOp(var Res: dyn): Boolean;
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
