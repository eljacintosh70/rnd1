type
  TDynOutAssignFn = function (var Dest; const src: dyn): Boolean;

  dynout = record
  public
    AsigFn: TDynOutAssignFn;
    DestVar: Pointer;
  public
    procedure Assign(const src: dyn);
    function CanAssign(const src: dyn): Boolean;
  public
    class operator Implicit(var A: dyn): dynout; inline;
    class operator Implicit(var A: IDyn): dynout; inline;
    class operator Implicit(var A: Boolean): dynout; inline;
    class operator Implicit(var A: Int64): dynout; inline;
    class operator Implicit(var A: Double): dynout; inline;
    class operator Implicit(var A: WideChar): dynout; inline;
    class operator Implicit(var A: AnsiChar): dynout; inline;
    class operator Implicit(var A: UnicodeString): dynout; inline;
  end;

function AsignToDyn(var Dest; const src: dyn): Boolean;
function AsignToIDyn(var Dest; const src: dyn): Boolean;
function AsignToBoolean(var Dest; const src: dyn): Boolean;
function AsignToInt64(var Dest; const src: dyn): Boolean;
function AsignToDouble(var Dest; const src: dyn): Boolean;
function AsignToWideChar(var Dest; const src: dyn): Boolean;
function AsignToAnsiChar(var Dest; const src: dyn): Boolean;
function AsignToString(var Dest; const src: dyn): Boolean;

function AsignIgnore(var Dest; const src: dyn): Boolean;
function AsignIsNil(var Dest; const src: dyn): Boolean;

function MatchCount(const src: dyn; const Required: array of dynout): Integer; overload;
function MatchCount(const src: dyn; const Required: array of dynout; Rest: dynout): Integer; overload;


const
  _: dynout = (AsigFn: AsignIgnore);
  _nil: dynout = (AsigFn: AsignIsNil);

