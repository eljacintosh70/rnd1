type
  TDynMatchFn = function (const src: dyn; var Dest): Boolean;

  dynout = record
  public
    MatchFn: TDynMatchFn;
    DestVar: Pointer;
  public
    procedure Assign(const src: dyn);
    function Match(const src: dyn): Boolean;
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

function MatchDyn(const src: dyn; var Dest): Boolean;
function MatchIDyn(const src: dyn; var Dest): Boolean;
function MatchBoolean(const src: dyn; var Dest): Boolean;
function MatchInt64(const src: dyn; var Dest): Boolean;
function MatchDouble(const src: dyn; var Dest): Boolean;
function MatchWideChar(const src: dyn; var Dest): Boolean;
function MatchAnsiChar(const src: dyn; var Dest): Boolean;
function MatchString(const src: dyn; var Dest): Boolean;

function IgnoreMatch(const src: dyn; var Dest): Boolean;
function MatchNil(const src: dyn; var Dest): Boolean;

function Match(const src: dyn; const Required: array of dynout): Boolean; overload;
function Match(const src: dyn; const Required: array of dynout; Rest: dynout): Boolean; overload;
function MatchCount(const src: dyn; const Required: array of dynout): Integer; overload;
function MatchCount(const src: dyn; const Required: array of dynout; Rest: dynout): Integer; overload;


const
  _: dynout = (MatchFn: IgnoreMatch);
  _nil: dynout = (MatchFn: MatchNil);

