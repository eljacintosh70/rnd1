unit DTFile;

interface

uses
  Classes,
  DynTypes, DTDatum, DTPort;

type
  IDynStream = interface(IDynDatum)
    function GetSize: Int64;
    procedure SetSize(const Value: Int64);
    property Size: Int64 read GetSize write SetSize;

    function GetPosition: Int64;
    procedure SetPosition(const Pos: Int64);
    property Position: Int64 read GetPosition write SetPosition;

    function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;

    function Read(var Buffer; Count: Longint): Longint;
    function Write(const Buffer; Count: Longint): Longint;
  end;

  IDynObjStream = interface(IDynStream)
    function ReadObj(var Obj: dyn): Longint;
    function WriteObj(const Obj: dyn): Longint;
  end;

  TDynStream = class(TDyn, IDynStream)
  protected
    FStream: TStream;
  public
    constructor Create(AStream: TStream);
    destructor Destroy; override;
    function WriteProc(p: Pointer; cb: Integer): Boolean;
    property Stream: TStream read FStream implements IDynStream;
  end;

  TDynObjOutStream = class(TDynStream, IDynObjStream)
  protected
    FFormat: TCustomDynOutPort;
  public
    constructor Create(AStream: TStream; AFormat: TCustomDynOutPort);
    function ReadObj(var Obj: dyn): Longint;
    function WriteObj(const Obj: dyn): Longint;
    property Stream read FStream implements IDynObjStream;
  end;

implementation

{ TDynStream }

constructor TDynStream.Create(AStream: TStream);
begin
  FStream := AStream;
end;

destructor TDynStream.Destroy;
begin
  FStream.Free;
  inherited Destroy;
end;

function TDynStream.WriteProc(p: Pointer; cb: Integer): Boolean;
begin
  FStream.Write(p^, cb);
  Result := True;
end;

{ TDynObjStream }

constructor TDynObjOutStream.Create(AStream: TStream; AFormat: TCustomDynOutPort);
begin
  inherited Create(AStream);
  FFormat := AFormat;
  FFormat.WriteProc := WriteProc;
end;

function TDynObjOutStream.ReadObj(var Obj: dyn): Longint;
begin
  DynError('Stream is not readable', []);
  Result := 0;
end;

function TDynObjOutStream.WriteObj(const Obj: dyn): Longint;
begin
  FFormat.Write(Obj);
  Result := FStream.Position;
end;

end.

