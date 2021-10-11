unit Core;

interface

uses
  SysUtils,
  DTPort, DTPortW, DUtils, DynTypes,
  LispWrite, RndWrite,
  LispSintax, LispFiles, LispFunc, RndSintax,
  LispInterpreter;

var
  Scope: IInterpreter;

procedure InitCore;

function DisplayL(obj: dyn): string;
function DisplayR(obj: dyn): string;

implementation

uses LispEnv;

procedure InitCore;
begin
  Scope := TInterpreter.Create(nil);

  LispEnv.RegisterFunctions(Scope, FileFunctions);
  LispEnv.RegisterSyntax   (Scope, Sintax1);
  LispEnv.RegisterSyntax   (Scope, TRndSintax);
  LispEnv.RegisterFunctions(Scope, TBasicFunctions);
  LispEnv.RegisterFunctions(Scope, TMathOpers);
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

