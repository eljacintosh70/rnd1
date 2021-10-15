
type
  PDestroyed = ^TDestroyedRec;
  TDestroyedRec = record
    VMT: TClass;
    OldVMT: TClass;
  end;

  TDestroyed = class(TDyn)
  // TInterfaceHelper = class(TObject, IInterface)
  public
    function QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    function _AddRef: Integer; override; {$IFDEF LINUX} Cdecl {$ELSE} stdcall {$ENDIF};
    function _Release: Integer; override; {$IFDEF LINUX} Cdecl {$ELSE} stdcall {$ENDIF};
  // TDatumHelper = class(TInterfaceHelper, IDatum)
  public
    function DatumType: TDatumType; override;
    function DisplayStr(NeededChars: Integer): String; override;
    function WriteStr(NeededChars: Integer): String; override;
  // extra
  public
    procedure Vmt1; virtual; // function Rest: IDynSeq; virtual; abstract;
    procedure Vmt2; virtual; // function First: TDynDatum; virtual; abstract;
    procedure Vmt3; virtual; // function HasData: Boolean; virtual;
    procedure Vmt4; virtual;
    procedure Vmt5; virtual;
    procedure Vmt6; virtual;
    procedure Vmt7; virtual;
    procedure Vmt8; virtual;
    procedure Vmt9; virtual;
  public
    class procedure Patch(Dest: TCustomInterface);
  end;

function TDestroyed.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  raise Exception.Create('Failed QueryInterface');
end;
function TDestroyed._AddRef: Integer;
begin
  raise Exception.Create('Failed _AddRef');
end;
function TDestroyed._Release: Integer;
begin
  raise Exception.Create('Failed _Release');
end;
function TDestroyed.DatumType: TDatumType;
begin
  raise Exception.Create('Failed DatumType');
end;
function TDestroyed.DisplayStr(NeededChars: Integer): String;
begin
  raise Exception.Create('Failed DisplayStr');
end;
function TDestroyed.WriteStr(NeededChars: Integer): String;
begin
  raise Exception.Create('Failed WriteStr');
end;
procedure TDestroyed.Vmt1; // function Rest: IDynSeq; virtual; abstract;
begin
  raise Exception.Create('Failed Vmt1');
end;
procedure TDestroyed.Vmt2;  // function First: TDynDatum; virtual; abstract;
begin
  raise Exception.Create('Failed Vmt2');
end;
procedure TDestroyed.Vmt3;  // function HasData: Boolean; virtual;
begin
  raise Exception.Create('Failed Vmt3');
end;
procedure TDestroyed.Vmt4;
begin
  raise Exception.Create('Failed Vmt4');
end;
procedure TDestroyed.Vmt5;
begin
  raise Exception.Create('Failed Vmt5');
end;
procedure TDestroyed.Vmt6;
begin
  raise Exception.Create('Failed Vmt6');
end;
procedure TDestroyed.Vmt7;
begin
  raise Exception.Create('Failed Vmt7');
end;
procedure TDestroyed.Vmt8;
begin
  raise Exception.Create('Failed Vmt8');
end;
procedure TDestroyed.Vmt9;
begin
  raise Exception.Create('Failed Vmt9');
end;

var
  LastDestroyedObj: TCustomInterface;
  LastDestroyed: PDestroyed;

class procedure TDestroyed.Patch(Dest: TCustomInterface);
var
  d: PDestroyed;
begin
  LastDestroyedObj := Dest;
  d := PDestroyed(Dest);
  LastDestroyed := d;
  d.OldVMT := d.VMT;
  d.VMT := TDestroyed;
end;

(*

type
  PDestroyed = ^TDestroyed;
  TDestroyed = record
    VMT: Pointer;
    OldVMT: Pointer;
  end;

procedure FailedQueryInterface(Self: Pointer); stdcall;
begin
  raise Exception.Create('Failed QueryInterface');
end;
procedure Failed_AddRef(Self: Pointer); stdcall;
begin
  raise Exception.Create('Failed _AddRef');
end;
procedure Failed_Release(Self: Pointer); stdcall;
begin
  raise Exception.Create('Failed _Release');
end;
procedure Failed(Self: Pointer); stdcall;
begin
  raise Exception.Create('Failed Call');
end;

const
  FailedVMT: array[0..10] of Pointer = (
    @FailedQueryInterface, // function QueryInterface(const IID: TGUID; out Obj): HResult; stdcall;
    @Failed_AddRef,        // function _AddRef: Integer; stdcall;
    @Failed_Release,       // function _Release: Integer; stdcall;
    @Failed, @Failed, @Failed, @Failed,
    @Failed, @Failed, @Failed, @Failed);
*)

