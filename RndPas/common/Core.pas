unit Core;

interface

uses
  SysUtils,
  DTPort, DTPortW, DUtils, DynTypes,
  LispWrite, RndWrite,
  LispSintax, LispFunc, RndSintax,
  LispInterpreter;

var
  Scope: IInterpreter;

procedure InitCore;

function DisplayL(obj: dyn): string;
function DisplayR(obj: dyn): string;

implementation

procedure InitCore;
begin
  Scope := TInterpreter.Create(nil);

  Scope.RegisterSintax(TSintax1.Create);
  Scope.RegisterSintax(TRndSintax.Create);
  Scope.RegisterFunctions(TBasicFunctions.Create);
  Scope.RegisterFunctions(TMathOpers.Create);
  Scope.Rename(['_if', 'SetVal'],
               ['if',  'set!'  ]);
  Scope.Rename(['Add', 'Subst', 'Mult', 'Divide', 'Pow', 'dot', 'DotSet'],
               ['+',   '-',     '*',    '/',      '^',   '.',   '.set!']);
  Scope.Rename(['nLT', 'nLE', 'nEQ', 'nGE', 'nGT'],
               ['<',   '<=',  '=',    '>=', '>'  ]);
end;

function DisplayL(obj: dyn): string;
var
  FTextOut: TStrTextOutW;
  FDynOutPort: TDynOutPortLisp;
  Ref: IDynOutPort;
begin
  FTextOut := TStrTextOutW.Create(200);
  FDynOutPort := TDynOutPortLisp.Create(FTextOut.WriteProc);
  Ref := FDynOutPort;

  FDynOutPort.Write(Obj);
  Result := FTextOut.GetText;

  Ref := nil; //FDynOutPort.Free;
  FTextOut.Free;
end;

function DisplayR(obj: dyn): string;
var
  FTextOut: TStrTextOutW;
  FDynOutPort: TDynOutPortRnd;
  Ref: IDynOutPort;
begin
  FTextOut := TStrTextOutW.Create(200);
  FDynOutPort := TDynOutPortRnd.Create(FTextOut.WriteProc);
  Ref := FDynOutPort;

  FDynOutPort.Write(Obj);
  Result := FTextOut.GetText;

  Ref := nil; //FDynOutPort.Free;
  FTextOut.Free;
end;


end.

