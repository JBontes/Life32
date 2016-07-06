unit RotatBit;

interface

function Rol32(i: integer; RotateBy: integer): integer;
function Ror32(i: integer; RotateBy: integer): integer;
function Rol16(i: Smallint; RotateBy: Smallint): Smallint;
function Ror16(i: Smallint; RotateBy: Smallint): Smallint;
function Rol8(i: Byte; RotateBy: Byte): Byte;
function Ror8(i: Byte; RotateBy: Byte): Byte;

implementation

//I really dislike the fact that Delphi, does not know inline routines.
//I should have started with C.
//btw This can be solved with the {$I filename} compiler-directive
//but this gives real UGLY code and I hate UgLy CodE.

function Rol32(i: integer; RotateBy: integer): integer; register;
asm
  mov ECX, EDX  {# of bits to rotate in CL}
  rol EAX, CL   {Result In EAX}
end;

function Ror32(i: integer; RotateBy: integer): integer; register;
asm
  mov ECX, EDX  {# of bits to rotate in CL}
  ror EAX, CL   {Result In EAX}
end;

function Rol16(i: Smallint; RotateBy: Smallint): Smallint; register;
asm
  mov ECX, EDX   {# of bits to rotate in CL}
  rol AX,CL      {Result In EAX}
end;

function Ror16(i: Smallint; RotateBy: Smallint): Smallint; register;
asm
  mov ECX, EDX
  ror AX,CL
end;

function Rol8(i: Byte; RotateBy: Byte): Byte; register;
asm
  mov ECX, EDX
  rol AL,CL
end;

function Ror8(i: Byte; RotateBy: Byte): Byte; register;
asm
  mov ECX, EDX
  ror AL,CL
end;

initialization
end.
