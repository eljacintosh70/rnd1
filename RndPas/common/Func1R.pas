unit Func1R;

interface //////////////////////////////////////////////////////////////////////

uses
  SysUtils,
  DUtils, DynTypes, SchFunc,
  Math;

function abs   (x: Real): Real;
function sqr   (x: Real): Real;
function sqrt  (x: Real): Real;

function log   (x: Real): Real;
function pow10 (x: Real): Real;
function ln    (x: Real): Real;
function exp   (x: Real): Real;

function sin(x: Real): Real;
function cos(x: Real): Real;
function tan(x: Real): Real;
function sec(x: Real): Real;
function csc(x: Real): Real;
function cot(x: Real): Real;

function arcsin(x: Real): Real;
function arccos(x: Real): Real;
function arctan(x: Real): Real;
function arcsec(x: Real): Real;
function arccsc(x: Real): Real;
function arccot(x: Real): Real;

function sinh(x: Real): Real;
function cosh(x: Real): Real;
function tanh(x: Real): Real;
function sech(x: Real): Real;
function csch(x: Real): Real;
function coth(x: Real): Real;

function arcsinh(x: Real): Real;
function arccosh(x: Real): Real;
function arctanh(x: Real): Real;
function arcsech(x: Real): Real;
function arccsch(x: Real): Real;
function arccoth(x: Real): Real;

const
  Functions1R: array[0..30] of TFunc1RRec = (
    (Name: 'abs';          Fn: abs       ),
    (Name: 'sqr';          Fn: sqr       ),
    (Name: 'sqrt';         Fn: sqrt      ),

    (Name: 'log';          Fn: log      ),
    (Name: 'pow10';        Fn: pow10    ),
    (Name: 'ln';           Fn: ln       ),
    (Name: 'exp';          Fn: exp      ),

    (Name: 'sin';          Fn: sin      ),
    (Name: 'cos';          Fn: cos      ),
    (Name: 'tan';          Fn: tan      ),
    (Name: 'sec';          Fn: sin      ),
    (Name: 'csc';          Fn: cos      ),
    (Name: 'cot';          Fn: tan      ),

    (Name: 'arcsin';       Fn: arcsin   ),
    (Name: 'arccos';       Fn: arccos   ),
    (Name: 'arctan';       Fn: arctan   ),
    (Name: 'arcsec';       Fn: arcsin   ),
    (Name: 'arccsc';       Fn: arccos   ),
    (Name: 'arccot';       Fn: arctan   ),

    (Name: 'sinh';         Fn: sinh     ),
    (Name: 'cosh';         Fn: cosh     ),
    (Name: 'tanh';         Fn: tanh     ),
    (Name: 'sech';         Fn: sinh     ),
    (Name: 'csch';         Fn: cosh     ),
    (Name: 'coth';         Fn: tanh     ),

    (Name: 'arcsinh';      Fn: arcsinh  ),
    (Name: 'arccosh';      Fn: arccosh  ),
    (Name: 'arctanh';      Fn: arctanh  ),
    (Name: 'arcsech';      Fn: arcsinh  ),
    (Name: 'arccsch';      Fn: arccosh  ),
    (Name: 'arccoth';      Fn: arctanh  )
  );


implementation

function abs    (x: Real): Real; begin Result := System.Abs(x);    end;
function sqr    (x: Real): Real; begin Result := System.Sqr(x);    end;
function sqrt   (x: Real): Real; begin Result := System.Sqrt(x);   end;
function log    (x: Real): Real; begin Result := Math.Log10(x);    end;
function pow10  (x: Real): Real; begin Result := Math.Power(10,x); end;
function ln     (x: Real): Real; begin Result := System.Ln(x);     end;
function exp    (x: Real): Real; begin Result := System.Exp(x);    end;

function sin    (x: Real): Real; begin Result := System.sin(x);    end;
function cos    (x: Real): Real; begin Result := System.cos(x);    end;
function tan    (x: Real): Real; begin Result := Math.tan(x);      end;
function sec    (x: Real): Real; begin Result := Math.sec(x);      end;
function csc    (x: Real): Real; begin Result := Math.csc(x);      end;
function cot    (x: Real): Real; begin Result := Math.cot(x);      end;

function arcsin (x: Real): Real; begin Result := Math.arcsin(x);   end;
function arccos (x: Real): Real; begin Result := Math.arccos(x);   end;
function arctan (x: Real): Real; begin Result := System.arctan(x); end;
function arcsec (x: Real): Real; begin Result := Math.arcsec(x);   end;
function arccsc (x: Real): Real; begin Result := Math.arccsc(x);   end;
function arccot (x: Real): Real; begin Result := Math.arccot(x);   end;

function sinh   (x: Real): Real; begin Result := Math.sinh(x);     end;
function cosh   (x: Real): Real; begin Result := Math.cosh(x);     end;
function tanh   (x: Real): Real; begin Result := Math.tanh(x);     end;
function sech   (x: Real): Real; begin Result := Math.sech(x);     end;
function csch   (x: Real): Real; begin Result := Math.csch(x);     end;
function coth   (x: Real): Real; begin Result := Math.coth(x);     end;

function arcsinh(x: Real): Real; begin Result := Math.arcsinh(x);  end;
function arccosh(x: Real): Real; begin Result := Math.arccosh(x);  end;
function arctanh(x: Real): Real; begin Result := Math.arctanh(x);  end;
function arcsech(x: Real): Real; begin Result := Math.arcsech(x);  end;
function arccsch(x: Real): Real; begin Result := Math.arccsch(x);  end;
function arccoth(x: Real): Real; begin Result := Math.arccoth(x);  end;

end.

