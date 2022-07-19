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
    function DatumType: TDatumType; override;
    function WriteProc(p: Pointer; cb: Integer): Boolean;
    property Stream: TStream read FStream {$IFNDEF FPC} implements IDynStream {$ENDIF};
  {$IFDEF FPC}
  protected
  // IDynStream = interface(IDynDatum)
  function GetSize: Int64;
  procedure SetSize(const Value: Int64);
  property Size: Int64 read GetSize write SetSize;
  function GetPosition: Int64;
  procedure SetPosition(const Pos: Int64);
  property Position: Int64 read GetPosition write SetPosition;
  function Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;
  function Read(var Buffer; Count: Longint): Longint;
  function Write(const Buffer; Count: Longint): Longint;
  {$ENDIF}
  end;

  TDynObjOutStream = class(TDynStream, IDynObjStream)
  protected
    FFormat: TCustomDynOutPort;
  public
    constructor Create(AStream: TStream; AFormat: TCustomDynOutPort);
    function ReadObj(var Obj: dyn): Longint;
    function WriteObj(const Obj: dyn): Longint;
    property Stream read FStream {$IFNDEF FPC} implements IDynObjStream {$ENDIF};
  end;

implementation

{ TDynStream }

constructor TDynStream.Create(AStream: TStream);
begin
  FStream := AStream;
end;

function TDynStream.DatumType: TDatumType;
begin
  Result := atOpaque
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

{$IFDEF FPC}
function TDynStream.GetSize: Int64;                                         begin Result := FStream.Size; end;
procedure TDynStream.SetSize(const Value: Int64);                           begin           FStream.Size := Value; end;
function TDynStream.GetPosition: Int64;                                     begin Result := FStream.Position; end;
procedure TDynStream.SetPosition(const Pos: Int64);                         begin           FStream.Position := Pos; end;
function TDynStream.Seek(const Offset: Int64; Origin: TSeekOrigin): Int64;  begin Result := FStream.Seek(Offset, Origin); end;
function TDynStream.Read(var Buffer; Count: Longint): Longint;              begin Result := FStream.Read(Buffer, Count); end;
function TDynStream.Write(const Buffer; Count: Longint): Longint;           begin Result := FStream.Write(Buffer, Count); end;
{$ENDIF}

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

