// -----------------------------------------------------------------------------
// Project:         Drag and Drop Component Suite
// Module:          DragDrop.inc
// Description:     This include file contains common defines used by the
//                  library.
// Authors:         Angus Johnson,   ajohnson@rpi.net.au
//                  Anders Melander, anders@melander.dk
//                                   http://www.melander.dk
// Copyright        © 1997-99 Angus Johnson & Anders Melander
// -----------------------------------------------------------------------------


// -----------------------------------------------------------------------------
//			Detect compiler version
// -----------------------------------------------------------------------------

// Delphi 1.x
{$IFDEF VER80}
  {$DEFINE VER8_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 2.x
{$IFDEF VER90}
  {$DEFINE VER9x}
  {$DEFINE VER9_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// C++ Builder 1.x
{$IFDEF VER93}
  {$DEFINE VER9x}
  {$DEFINE VER93_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 3.x
{$IFDEF VER100}
  {$DEFINE VER10_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// C++ Builder 3.x
{$IFDEF VER110}
  {$DEFINE VER11_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 4.x
{$IFDEF VER120}
  {$DEFINE VER12_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// C++ Builder 4.x
{$IFDEF VER125}
  {$DEFINE VER125_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Delphi 5.x
{$IFDEF VER130}
  {$DEFINE VER13_PLUS}
  {$DEFINE VER_OK}
{$ENDIF}

// Unknown compiler version - assume D4 compatible
{$IFNDEF VER_OK}
  {$DEFINE VER12_PLUS}
{$ELSE}
  {$UNDEF VER_OK}
{$ENDIF}

{$IFDEF VER13_PLUS}
  {$DEFINE VER125_PLUS}
{$ENDIF}
{$IFDEF VER125_PLUS}
  {$DEFINE VER12_PLUS}
{$ENDIF}
{$IFDEF VER12_PLUS}
  {$DEFINE VER11_PLUS}
{$ENDIF}
{$IFDEF VER11_PLUS}
  {$DEFINE VER10_PLUS}
{$ENDIF}
{$IFDEF VER10_PLUS}
  {$DEFINE VER93_PLUS}
{$ENDIF}
{$IFDEF VER93_PLUS}
  {$DEFINE VER9_PLUS}
{$ENDIF}
{$IFDEF VER9_PLUS}
  {$DEFINE VER8_PLUS}
{$ENDIF}


// -----------------------------------------------------------------------------
//			Required compiler directives
// -----------------------------------------------------------------------------

{$TYPEDADDRESS OFF}	// @ operator returns untyped pointer
{$BOOLEVAL OFF}		// Short circuit boolean evaluation
{$EXTENDEDSYNTAX ON}	// Enable Delphi Pascal extensions

