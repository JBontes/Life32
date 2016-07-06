unit LifeLoad;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  WinTypes, WinProcs, Classes, SysUtils, Dialogs, Graphics, LifeGen,
  LifeUtil, LifeConst;

type
  TCharSet = set of ansichar;

  TStringTokenizer = class(TObject)
  private
    FString: string;
    FPos: integer;
    FSeparators: TCharSet;
  protected
    procedure SetString(Value: string);
  public
    constructor Create(AString: string);
    destructor Destroy; override;
    function NextToken: string;
    function NextNumber: integer;
    property Pos: integer read FPos write FPos;
    //properties are a Delphi thing.
    //On the outside (see implementation) they look just like a variable.
    //In fact "Pos" in fact is just that.
    property TokenString: string read FString write SetString;
    //This property will map to the variable FString when read,
    //But when written to, the procedure SetString will be called which
    //loads FString in turn, and does some other stuff too.
    property Separators: TCharSet write FSeparators;
  end;

  //returns "true" if succesfull, false if not.
  function ReadProLife(AFile: string; AUniverse: TUniverse): Boolean;
  function ReadBitmap(ABitmap: TBitmap; AUniverse: TUniverse): Boolean;
  function ReadLife(LifeLines: TStringList; AUniverse: TUniverse): Boolean;
  function WriteLife(LifeLines: TStringList; AUniverse: TUniverse;
                     FileFormat: integer; IncludeTorusData: boolean): Boolean;
  function WriteBitmap(ABitmap: TBitmap; AUniverse: TUniverse): Boolean;
  function GetDescription(LifeLines: TStringList): TStringList;
  function GetRules(LifeLines: TStringList): string;
  //the above functions are public, because they are listed outside an object
  //in the interface section.

var
  //some initialized variables.
  //In older versions of Pascal this would be a typed constant.
  //In C++ it would be a static variable.
  MaxPictureWidth: integer = 80;
  MaxPictureHeight: integer = 80;
  LastFileRules: string = DefaultRules;
  DarkTreshhold: integer = 127;

implementation

uses
  Forms, Unit1;

type
  //Define the enumeration TLifeType.
  TLifeType = (XLife, Life105, RLE, Bell, Life106, Bitmap, MCell);

const
  //Defined the LifeConst.pas unit. In fact DefaultRule = '23/3'
  DefaultRule = DefaultRules;
  //Maximum width of a RLE pattern text file when *writing*.
  RLEMAX = 70;

  //TRect is defined as
  //TRect = record
  //  Left, Top, Right, Bottom: Int16;
  //end;

function IsEmptyRect(ARect: TRect): boolean;
begin
  with ARect do Result:= (Left = Right) or (Top = Bottom);
end;

//We define the functions here, so we can use them, before actually coding
//them. Hence the "forward"
//Remember that Delphi is a one-pass compiler, so it needs these hints.
//This is why it can compile 30,000 lines a second on my 100 Mhz Pentium.
function GetLifeType(LifeLines: TStringList): TLifeType; forward;

procedure ReadTorusData(ALine: string; AUniverse: TUniverse); forward;
function ReadLife105(LifeLines: TStringList; AUniverse: TUniverse):Boolean; forward;
function ReadRLE(LifeLines: TStringList; AUniverse: TUniverse):Boolean; forward;
function ReadBell(LifeLines: TStringList; AUniverse: TUniverse):Boolean; forward;
function ReadXLife(LifeLines: TStringList; AUniverse: TUniverse): Boolean; forward;
function ReadMCell(LifeLines: TStringList; AUniverse: TUniverse):Boolean; forward;

procedure WriteTorusData(LifeLines: TStringList; AUniverse: TUniverse); forward;
function WriteRLE (LifeLines: TStringList; AUniverse: TUniverse;
                  BoundingRect: TRect; IncludeTorusData: boolean): Boolean; forward;
function WriteXLife(LifeLines: TStringList; AUniverse: TUniverse;
                      BoundingRect: TRect; IncludeTorusData: boolean): Boolean; forward;
function WriteLife105(LifeLines: TStringList; AUniverse: TUniverse;
                      BoundingRect: TRect; IncludeTorusData: boolean): Boolean; forward;
function WriteLife106(LifeLines: TStringList; AUniverse: TUniverse;
                      BoundingRect: TRect; IncludeTorusData: boolean): Boolean; forward;


const
  Digits = ['-','0','1','2','3','4','5','6','7','8','9'];
  DefaultSeparators = [' ',',','.',';',#10,#13];
  SpecialChars = ['-','='];
  CRLF = #13+#10;


//C++ should have a tokenizer build in. I'm not 100% sure it will
//work equivilant though.
constructor TStringTokenizer.Create(AString: string);
begin
  FString:= AString;
  FSeparators:= DefaultSeparators;
  FPos:= 1;
end;

destructor TStringTokenizer.Destroy;
begin
  inherited Destroy;
end;

function TStringTokenizer.NextNumber: integer;
var
  TokenStart: integer;
  TokenLength: integer;
begin
  //Result:= 0;
  try
    while CharInSet(FString[FPos],FSeparators) and (FPos < Length(FString)) do Inc(Fpos);
    while not CharInSet(FString[FPos],Digits) and (FPos < Length(FString)) do Inc(FPos);
    TokenStart:= FPos;
    if Length(FString) > FPos then begin
      Inc(FPos);
      while (Length(FString) >= FPos) and
            CharInSet(FString[FPos],Digits) do Inc(FPos);
      TokenLength:= FPos - TokenStart;
      Result:= StrToInt(Copy(FString,TokenStart,TokenLength));
    end
    else if CharInSet(FString[FPos],Numbers) then Result:= StrToInt(FString[FPos])
    //If something goes wrong, raise an exception that will be caught
    //in the caller. (This must never reach the user).
    //Some perfectly valid patterns cause this exception, but still
    //show a valid pattern.
    else raise EConvertError.Create('Error in NextNumber');

    //Do not catch the exception here, but pass it on to the caller.
    except on {EConvertError} exception do raise;
  end; {try}
end;

function TStringTokenizer.NextToken: string;
var
  TokenStart: integer;
  TokenLength: integer;
begin
  try
    while (FPos < Length(FString)) and CharInSet(FString[FPos],FSeparators) do Inc(FPos);
    TokenStart:= FPos;
    if Length(FString) > FPos then begin
      Inc(FPos);
      while (Length(FString) >= FPos) and
            not CharInSet(FString[FPos],FSeparators) and
            not CharInSet(FString[FPos],SpecialChars) and
            not CharInSet(FString[FPos],Digits)
            do Inc(FPos);
      TokenLength:= FPos - TokenStart;
      Result:= Copy(FString,TokenStart,TokenLength);
    end
    else Result:= FString[FPos];
    //if we read past the end of the string or whatever else goes wrong
    //just give nothing as the result.
    except on Exception do Result:= '';
  end; {try}
end;

procedure TStringTokenizer.SetString(Value: string);
begin
  FString:= Value;
  FPos:= 1;
end;


//Try to read the text file and get some clue as to what kind of file
//it is.
//BTW, notice that LifeLoad works with stringlists, not files.
//This way the exact same code can be used for reading files and
//reading stuff being dragged or pasted in.
//
//The stringlist is just a collection of strings. One string
//for each line in the file.
//In Delphi each string can be 2 gigabyte in size.
function GetLifeType(LifeLines: TStringList):TLifeType;
var
  i: integer;
  RLEfound: boolean;
  StartLine: string;
begin
  //Note there's no exception handler, I'm just really carefull
  //not to read past the end of the string, or the stringlist.
  Result:= XLife;
  i:= 0;
  StartLine:= '';
  while (i < LifeLines.count) and (StartLine = '') do begin
    StartLine:= Trim(LifeLines[i]);
    inc(i);
  end; {while}
  if Length(StartLine) > 0 then begin
    if CharInSet(StartLine[1],['X','x']) then Result:= RLE
    else if (Pos('LIFE 1.0',UpperCase(StartLine)) <> 0) then Result:= Life105
    else if (Pos('MCELL',UpperCase(StartLine)) <> 0) then Result:= MCell
    else if CharInSet(StartLine[1],['!']) or
            (Pos('@!',StartLine) <> 0) and not CharInSet(StartLine[1],['#']) then
      Result:= Bell

    else begin
      RLEfound:= false;
      i:= 0;
      while (i < LifeLines.Count) and not(RLEfound) do begin
        if (Length(LifeLines[i]) > 0) and (LifeLines[i][1] <> '#') then begin
          if ((Pos('B',UpperCase(LifeLines[i])) <> 0) and
              (Pos('O',UpperCase(LifeLines[i])) <> 0) and
              ((Pos('$',UpperCase(LifeLines[i])) <> 0)) or (Pos('!',UpperCase(LifeLines[i])) <> 0)) then RLEFound:= true;
        end;
        inc(i);
      end; {while}
      //for one line patterns (no '$'), not terminated by a '!'.
      //it still could be an RLE pattern, lets look for one more tellsign.
      i:= 0;
      while not(RLEFound) and (i < LifeLines.Count) do begin
        if (Length(LifeLines[i])>0) and (Upcase(LifeLines[i][1]) = 'X') then
          RLEFound:= true;
        inc(i);
      end; {while}
      if RLEfound then Result:= RLE
      else Result:= XLife;
    end; {else}
  end;
end;

function ReadLife(LifeLines: TStringList; AUniverse: TUniverse):Boolean;
begin
  Result:= true;
  try
    //do *not* Default to life-rules.
    //AUniverse.RuleString:= '';
    case GetLifeType(LifeLines) of
      RLE: Result:= ReadRLE(LifeLines,AUniverse);
      XLife: Result:= ReadXLife(LifeLines,AUniverse);
      Life105: Result:= ReadLife105(LifeLines,AUniverse);
      Bell: Result:= ReadBell(LifeLines,AUniverse);
      MCell: Result:= ReadMCell(LifeLines,AUniverse);
    end; {case}
    //This will happen very rarely. The loader takes just about anything.
    //Even random text will be accepted. The user is quite able to detect
    //from the result on the screen that an error occured, we don't need an
    //error box for that!
    except on E:exception do
      ShowMessage('Error reading file '+#10+#13+
                  'Picture may be damaged'+#10+#13+
                  'Error: '+E.message);
  end; {try}
end;

//This function filters out the description from a pattern.
//Usefull for the preview function.
//It also filters out the rule if there is any clue in the pattern
//file.
//This is stored in the unit variable "LastFileRules".
function GetDescription(LifeLines: TStringList): TStringList;
var
  i,a: integer;
  LifeType: TLifeType;
  DescHeader: string;
  PastRLE: boolean;
  ALine: string;
begin
  Result:= TStringList.Create;
  LastFileRules:= DefaultRules;
  try
    PastRLE:= false;
    LifeType:= GetLifeType(LifeLines);
    for i:=0 to LifeLines.Count-1 do begin
      ALine:= LifeLines[i]+'  ';
      DescHeader:= UpperCase(Copy(ALine,1,2));
      if (DescHeader[1] = '#') and not(PastRLE) then case DescHeader[2] of
        'D','C','O': Result.Add(Copy(ALine,3,Length(ALine)-4));
        'N': begin
          if Length(Trim(ALine)) > 2 then
            Result.Add(Copy(ALine,3,Length(ALine)-4))
          else LastFileRules:= DefaultRules;
        end; {'N'}
        'R': begin
          if (LifeType <> XLife) or (upcase(ALine[2]) = 'R') then LastFileRules:= Copy(ALine,3,Length(ALine)-4)
          else LastFileRules:= DefaultRules;
        end;
      end; {if case}

      if (LifeType = Bell) or (LifeType = RLE) then begin
        if (CharInSet(ALine[1],['!'])) then Result.Add(Copy(ALine,2,Length(ALine)-3))
        else if not ((LifeType = RLE) and (PastRLE)) then begin
          a:= Pos('RULE',Uppercase(ALine));
          if a <> 0 then LastFileRules:= Copy(ALine,a+4,Length(ALine)-(a+5));
        end; {else}
      end;

      if (LifeType = RLE) and (PastRLE) then begin
        DescHeader:= UpperCase(Copy(ALine,1,2));
        if (DescHeader[1] = '#') then case DescHeader[2] of
          'D','C','O': Result.Add(Copy(ALine,3,Length(ALine)-4));
          'N': if Length(Trim(LifeLines[1])) > 2 then
            Result.Add(Copy(ALine,3,Length(ALine)-4));
        end {if case}
        else Result.Add(Copy(ALine,1,Length(ALine)-2));
      end; {if}

      if (LifeType = RLE) and (Pos('!',ALine)<>0) and (DescHeader[1]<> '#') then begin
        PastRLE:= true;
      end; {if}
    end;    {for}
    except begin
      Result.Free;
      Result:= nil;
    end; {except}
  end; {try}
end;

//This function only works OK when GetDescription has been called first.
//On the pattern at hand.
function GetRules(LifeLines: TStringList): string;
var
  i: integer;
begin
  Result:= LastFileRules;
  i:= Pos('=',Result);
  if i <> 0 then Delete(Result,i,1);
  Result:= Trim(Result);
end;

//the LifeLoad unit enforces the rule, that it will accept anything
//you throw at it when reading.
//But it is very strict when writing files.

//Read a Run-length encoded description from a stringlist.
function ReadRLE(LifeLines: TStringList; AUniverse: TUniverse): Boolean;
var
  st: TStringTokenizer;
  AWidth, AHeight: integer;
  AVal: integer;
  i: integer;
  APos: integer;
  MinX,x,y: integer;
  ready, TorusActive: boolean;
begin
  TorusActive:= false;
  st:= TStringTokenizer.Create('');
  //default in case there is no leading line.
  //In reality the AHeight and AWidth lines are ignored.
  //I just use them to figure out what the middle of the pattern
  //is, so I can center the pattern at coordinate (0,0).
  //Hence the 2,2 (1 div 2 = 0, no good).
  AHeight:= 2;
  AWidth:= 2;
  try
    Result:= true;
    i:= 0;

    //Skip empty lines.
    while Length(TrimLeft(LifeLines[i])) = 0 do Inc(i);
    st.TokenString:= Trim(LifeLines[i]);

    //first get rid of all '#' lines.
    if st.TokenString[1] = '#' then begin
      while (st.TokenString[1] = '#') and (i < LifeLines.count) do begin
        if (Length(st.TokenString) > 2) and (st.TokenString[2] = 'T') then begin
          TorusActive:= true;
          ReadTorusData(st.TokenString, AUniverse);
        end;
        Inc(i);
        st.TokenString:= Trim(LifeLines[i]);
      end; {while}
      //Dec(i);
    end;

    //Every RLE file should begin with an "X".
    if (Upcase(st.NextToken[1]) = 'X') then begin
      if st.NextToken[1] <> '=' then AWidth:= 2
      else try
        AWidth:= st.NextNumber;
        if (AWidth <= 0) then AWidth:= 2;
        except AWidth:= 2;
      end; {try}

      if (Upcase(st.NextToken[1]) = 'Y') then begin
        if st.NextToken <> '=' then AHeight:= 2
        else try
          AHeight:= st.NextNumber;
          if (AHeight <= 0) then AHeight:= 2;
          except AHeight:= 2;
        end; {try}
      end;

      if st.NextToken = 'rule' then begin
        if st.NextToken = '=' then
          AUniverse.RuleString:= (Trim(Copy(st.Tokenstring,st.Pos,Length(st.TokenString)-st.Pos+1)));
      end; {if}
      Inc(i);
    end; {if}

    if not TorusActive then begin
      y:= 0 - (AHeight div 2);
      x:= 0 - (AWidth div 2);
    end
    else begin
      x:= AWidth;
      y:= AHeight;
    end;
    MinX:= x;
    Ready:= false;
    while (i < LifeLines.Count) and not(Ready) do begin
      st.TokenString:= LifeLines[i];
      APos:= 1;
      while (APos <= Length(LifeLines[i])) and not(Ready) do begin
        AVal:= 1;
        if CharInSet(st.TokenString[APos],Digits) then begin
          st.Pos:= APos;
          AVal:= st.NextNumber;
          APos:= st.Pos;
        end;
        case st.TokenString[APos] of
          'b','B':Inc(x,AVal);
          '$':begin
            Inc(y,AVal);
            x:= MinX;
          end;
          ' ',#9,#10,#13:; {do nothing}
          '!': Ready:= true;
          //Also accept other chars then "O" and "o" as on cells,
          //because some patterns also "X" and even more bizarre
          //chars for the on cell.
          else {'o','O': and others} while AVal > 0 do begin
            AUniverse.ChangeCel(x,y,true);
            Inc(x); Dec(AVal);
          end;
        end; {case}
        Inc(APos);
      end; {while}
      Inc(i);
    end; {while}
    //make sure st gets cleaned up. Even if an A-bomb hits our CPU.
    //see the "try" above.
    finally st.Free;
  end;
end;

procedure ReadTorusData(ALine: string; AUniverse: TUniverse);
var
  ARect: TRect;
  st: TStringTokenizer;
  Option: string;
begin
  st:= TStringTokenizer.Create(ALine);
  try
    if length(ALine) < 3 then exit;
    st.NextToken; {skip the #Tx part}
    case Upcase(ALine[3]) of
      'D': begin
        try

          with ARect do begin
            Left:= st.NextNumber; Top:= st.NextNumber;
            Right:= Left + st.NextNumber - 1;
            Bottom:= Top + st.NextNumber - 1;
            AUniverse.Limit:= ARect;
            AUniverse.IsLimited:= true;
          end;
        except {ignore}
        end;
      end; {D:}
      'O': begin
        try
          repeat
            option:= Uppercase(st.NextToken);
            if (option = 'WRAP') then begin
              if st.NextToken <> '=' then AUniverse.DeadEdges:= false
              else try
                AUniverse.DeadEdges:= not(boolean(st.NextNumber));
                except AUniverse.DeadEdges:= false;
              end; {try}
            end; {if WRAP}
            if (option = 'KIND') then begin
              if st.NextToken <> '=' then AUniverse.TorusKind:= tk_All
              else try
                AUniverse.TorusKind:= st.NextNumber;
                except AUniverse.TorusKind:= tk_All;
              end; {try}
            end; {if KIND}
          until Length(Option) < 4;
        except {ignore}
        end;
      end; {O:}
    end; {case}
  finally
    st.Free;
  end; {try}
end;

//This procedure will just try to interpret every line as best it can.
//In many ways it goes way beyond the Life 1.05 and 1.06 spec.
//This is really a copy of the ReadXLife routines, with some adjustments
//to facilitate the differences between XLife and Life 1.05.
function ReadLife105(LifeLines: TStringList; AUniverse: TUniverse):Boolean;
var
  Start: string;
  x1,y1: integer;
  a,b,i,j: integer;
  st: TStringTokenizer;
  c: Char;
begin
  st:= TStringTokenizer.Create('');
  try
	  y1:= 0; x1:= 0; //just to kill compiler warnings
	  Result:= true;
	  //Remove #Life 1.0x line.
    //We know for sure it will be there, otherwise we would never
    //have gotten here.
	  LifeLines.Delete(0);

	  for i:= 0 to LifeLines.Count -1 do begin
	    //first clean up the trash
	    LifeLines[i]:= Trim(LifeLines[i]);
	    a:= Length(LifeLines[i]);
	    if a > 0 then begin
	      if a=1 then Start:= LifeLines[i][1]+' '
	      else Start:= LifeLines[i][1] + LifeLines[i][2];
	      st.TokenString:= LifeLines[i];
	      if Start[1]='#' then begin
	        st.NextToken;
	        case Start[2] of
            'T','t': begin
              //TorusActive:= true;
              ReadTorusData(st.TokenString, AUniverse);
            end;
	          'D','d': begin {comments:ignore} end; {'D'}
	          'P','p': begin
	            try
	              x1:= st.NextNumber;
	              except on EConvertError do x1:= 0;
	            end; {try}
	            try
	              y1:= st.NextNumber;
	              except on EConvertError do y1:= 0;
	            end; {try}
	          end; {'P':}
	          'N','n': AUniverse.RuleString:= DefaultRules;
	          'R','r': try
              AUniverse.RuleString:= (Copy(st.TokenString,st.Pos,(Length(st.TokenString)-(st.Pos-1))));
	            except AUniverse.RuleString:= DefaultRules;
              LastFileRules:= AUniverse.RuleString;
            end; {'R'}
	          'S','s': begin end; {this is the playback speed, ignore for now}
            'C','c': begin end; {XLife style comments, ignore. }
	        end; {case}
	      end

	      //*,O,o = On, this does not comply with Xlife 3.5 (i think)
	      //Only '.' is off, this is also not the same as Xlife.

	      else if CharInSet(Start[1],['.','*','o','O','$']) then begin
          b:= 0;
	        for j:= 1 to Length(LifeLines[i]) do begin
	          c:= LifeLines[i][j];
	          if CharInSet(c,['*','o','O']) then AUniverse.ChangeCel(x1+j+b,y1,true)
            else if c = '$' then Inc(b,10);
	        end; {for j}
	        inc(y1);
	      end {else if}

	      else if not CharInSet(start[1],['!']) then begin
	      //let's try if these are normal coordinates, ignore otherwise.
	        try
	          x1:= st.NextNumber;
	          y1:= st.NextNumber;
	          AUniverse.ChangeCel(x1,y1,true);
	          except on EConvertError do begin {do nothing} end;
	        end; {try}
	      end; {if}
	    end; {if}
	  end; {for i}
    finally st.Free;
  end; {try}
end;


// In MCell an RLE format is used.
//every pattern line is preceded by a '#L' header
//and use '.' for off
//'A'..'X' for on
//$ and numbers as usual. no terminating '!' is used.
function ReadMCell(LifeLines: TStringList; AUniverse: TUniverse):Boolean;
type
  TGameType = (Life, Unsupported);
var
  MinX: integer;
  i,NoOfTokens: integer;
  p: integer;
  st: TStringTokenizer;
  ALine: string;
  APos: integer;
  AVal: integer;
  x,y: integer;
  GameType: TGameType;
  Speed: integer;
begin
  st:= TStringTokenizer.Create('');
  //st.Separators:= DefaultSeparators - ['.'];
  //Default to Life games.
  GameType:= Life;
  try
    //just to kill compiler warnings
	  Result:= true;
    //Find the #Game header line.
	  i:= 0;
    while (i < LifeLines.Count) and (Pos('#GAME',Uppercase(LifeLines[i])) = 0)
    do Inc(i);
    if i < LifeLines.Count then begin
      st.TokenString:= LifeLines[i];
      p:= Pos('GAME',Uppercase(LifeLines[i]));
      if p <> 0 then begin
        st.Pos:= p + Length('Game');
        if Uppercase(st.NextToken) = 'LIFE' then GameType:= Life
        else if Uppercase(Trim(LifeLines[i])) = '#GAME' then GameType:= Life
        else GameType:= Unsupported;
      end; {if}
    end; {if}
    if GameType = Life then begin
      //Find the #Rule header line.
      i:= 0;
      NoOfTokens:= 2; //search for 2 tokens.
      while (NoOfTokens > 0) do begin
        while (i < LifeLines.Count) and (
          (Pos('#RULE',Uppercase(LifeLines[i])) = 0) and
          (Pos('#SPEED',Uppercase(LifeLines[i])) = 0))
        do Inc(i);
        if i < LifeLines.Count then begin
          Dec(NoOfTokens);
          st.TokenString:= LifeLines[i];
          p:= Pos('RULE',Uppercase(LifeLines[i]));
          if p <> 0 then begin
            st.Pos:= p + Length('RULE');
            AUniverse.RuleString:= (Trim(Copy(st.Tokenstring,st.Pos,Length(st.TokenString)-st.Pos+1)));
          end; {if}
          p:= Pos('SPEED',Uppercase(LifeLines[i]));
          if p <> 0 then begin
            st.Pos:= p + Length('SPEED');
            Speed:= st.NextNumber;
            try
              TLife32MainForm(Application.MainForm).PlaySpeed:= Speed;
            except {ignore}
            end;
          end; {if}
          inc(i);
        end {if}
        else NoOfTokens:= 0;
      end;
      //Now read the Extended RLE lines.
      i:= 0;
      while (i < LifeLines.Count) and (Pos('#L',Uppercase(LifeLines[i])) = 0) do Inc(i);
      y:= 0;
      x:= 0;
      MinX:= x;
      while (i < LifeLines.Count) do begin
        if (Pos('#L',Uppercase(LifeLines[i])) <> 0) then begin
          //Cut of #L from line
          ALine:= Copy(LifeLines[i]+'  ',3,Length(LifeLines[i]));
          st.TokenString:= ALine;
          APos:= 1;
          while (APos <= Length(ALine)) do begin
            AVal:= 1;
            if CharInSet(st.TokenString[APos],Digits) then begin
              st.Pos:= APos;
              AVal:= st.NextNumber;
              APos:= st.Pos;
            end;
            case st.TokenString[APos] of
              '.':Inc(x,AVal);
              '$':begin
                Inc(y,AVal);
                x:= MinX;
              end;
              ' ',#9,#10,#13,'!':; {do nothing}
              //Also accept other chars then "A" and "a" as on cells,
              //because some patterns also "X" and even more bizarre
              //chars for the on cell.
              else {'A','a': and others} while AVal > 0 do begin
                AUniverse.ChangeCel(x,y,true);
                Inc(x); Dec(AVal);
              end;
            end; {case}
            Inc(APos);
          end; {while}
        end; {if '#L' in line}
        Inc(i);
      end; {while}
    end; {if FameType = Life}
    finally st.Free;
  end; {try}
end;

function ReadBell(LifeLines: TStringList; AUniverse: TUniverse):Boolean;
var
  MinX,x1,y1: integer;
  a,i,j,k: integer;
  p: integer;
  st: TStringTokenizer;
  Blanks: integer;
begin
  st:= TStringTokenizer.Create('');
  try
	  y1:= 0; x1:= 0; MinX:= 0; //just to kill compiler warnings
	  Result:= true;
	  i:= 0;
	  //Skip any leading empty or comment lines.
	  while (i < LifeLines.Count) and
	        (Pos('CELLS',Uppercase(LifeLines[i])) = 0) do inc(i);

	  if i < LifeLines.Count then begin
	    st.TokenString:= LifeLines[i];
	    p:= Pos('LENGTH',Uppercase(st.TokenString));
	    if p <> 0 then begin
	      st.Pos:= p + Length('Length');
	      try
	        y1:= -(st.NextNumber div 2);
	        except y1:= 0;
	      end; {try}
	    end; {if}
	    p:= Pos('WIDTH',Uppercase(st.TokenString));
	    if p <> 0 then begin
	      st.Pos:= p + Length('Width');
	      try
	        MinX:= -(st.NextNumber div 2);
	        except MinX:= 0;
	      end; {try}
	      x1:= MinX;
	    end; {if}
	  end; {if}


	  for i:= 0 to LifeLines.Count -1 do begin
	    //first clean up the trash
	    LifeLines[i]:= TrimLeft(LifeLines[i]);
	    a:= Length(LifeLines[i]);
	    if a > 0 then begin
	      if (LifeLines[i][1] <> '!') then begin
	        j:= 1;
	        while j <= Length(LifeLines[i]) do begin
	          case LifeLines[i][j] of
	            '0'..'9': begin
	              Blanks:= 0;
	              while (j < Length(LifeLines[i])) and CharInSet(LifeLines[i][j],Digits) do begin
	                Blanks:= Blanks * 10;
	                Blanks:= Blanks + byte(LifeLines[i][j]) - byte('0');
	                Inc(j);
	              end; {while}
	              case LifeLines[i][j] of
	                '0'..'9': begin  //This means line ends with digits, this many blank lines.
	                  Blanks:= Blanks * 10;
	                  Blanks:= Blanks + byte(LifeLines[i][j]) - byte('0');
	                  Inc(y1,Blanks-1);
	                end; {'0'..'9'}
	                ' ','.':Inc(x1,Blanks);
	                '*','O','o':for k:= 1 to Blanks do begin
	                  AUniverse.ChangeCel(x1,y1,true);
	                  Inc(x1);
	                end; {'*'..}
	                //ignore this stuff.
	                //'k','K':if (Pos('@!',LifeLines[i]) <> 0) then Inc(MinX,Blanks);
	                //'j','J':if (Pos('@!',LifeLines[i]) <> 0) then Dec(MinX,Blanks);
	                //'h','H':if (Pos('@!',LifeLines[i]) <> 0) then Dec(y1,Blanks);
	                //'l','L':if (Pos('@!',LifeLines[i]) <> 0) then Inc(y1,Blanks);
	              end; {case}
	            end; {Digits}
	            ' ','.':Inc(x1);
	            '*','o','O':begin
	              AUniverse.ChangeCel(x1,y1,true);
	              Inc(x1);
	            end; {'*'..}
	          end; {case}
	          Inc(j);
	        end; {while}
	        x1:= MinX;
	        inc(y1);
	      end {if}
	    end; {if}
	  end; {for i}
    finally st.Free;
  end; {try}
end;


type
  TLoadMode = (lm_Absolute, lm_Relative, lm_Picture);

//For now this will only read XLife 2.0 files (and stuff that looks a lot
//like it). Not #I XLife files.
//Note that Life32 supports this, but #I really is hard to code.
//If anyone can help me with this, please do.
function ReadXLife(LifeLines: TStringList; AUniverse: TUniverse):Boolean;
var
  Start: string;
  Temp: string;
  x1,y1,XOffset,YOffset: integer;
  a,i,j: integer;
  st: TStringTokenizer;
  LoadMode: TLoadMode;
  EmptyLine: boolean;
begin
  st:= TStringTokenizer.Create('');
  try
    XOffset:= 0; YOffset:= 0;
	  Result:= true;
	  LoadMode:= lm_Absolute; //Pick a default.
	  for i:= 0 to LifeLines.Count -1 do begin
	    //first clean up the trash
	    //LifeLines[i]:= Trim(LifeLines[i]);
	    st.TokenString:= LifeLines[i];
      //remove '>' in front of picture files, so pattern gets read correctly.
      Temp:= LifeLines[i] + '  ';
      repeat
        a:= Pos('>',Temp);
        if (a <> 0) and (Pos('#',Temp) = 0) then Delete(Temp,a,1);
      until a = 0;
      LifeLines[i]:= temp;
      //now start doing normal stuff.
      Temp:= trim(Temp);
	    a:= Length(Temp);
	    if a=0 then Start:= '  '
	    else if a=1 then Start:= Temp[1]+' '
	    else Start:= Temp[1] + Temp[2];
	    //'!' is used by dbLife (oh those non standard file formats blues).
      //'@' is used by Noam Elkies to highlight stuff in his emails <sigh>
      //'N' is used because Noam somethings sets his initials slam bam in the middle
      //of the pattern text, program reads letters NDE and blanks then, so pattern
      //runs correctly.
	    if CharInSet(Upcase(Start[1]),['.','*','o','O','!','$','@',' ','N']) then  LoadMode:= lm_Picture;

	    if Start[1] = '#' then begin
	      st.NextToken; //Get rid of #? 'cause that is already in Start.
	      case Start[2] of
          'T': begin
            //TorusActive:= true;
            ReadTorusData(st.TokenString, AUniverse);
          end;
	        'N':;//PatternName:= st.NextToken;
	        'P':begin
	          LoadMode:= lm_Picture;
	          try
	            XOffset:= st.NextNumber;
	            YOffset:= st.NextNumber;
	            except begin XOffset:= 0; YOffset:= 0; end;
	          end; {try}
	        end; {'P':}
	        'R':begin
            //Test to see if this really could be a rule line.
            //Uppercase matters here, but I'm using a workaround for that.
            //#r  ./.. is use rule "./.." and #R is loadmode relative, or the other
            //way around. Anyway, if #R is alone it's the Relative loadmode thing.
            //Otherwise something is listed, and then we'll use that something
            //for a rule. The clue here is the "/" char, which is only used in
            //a rule.
            //This avoids an incompatibility between XLife and Life 1.05.
            if Pos('/',st.TokenString) <> 0 then try
              AUniverse.RuleString:= (Copy(st.TokenString,st.Pos,(Length(st.TokenString)-(st.Pos-1))));
	            except AUniverse.RuleString:= DefaultRules;
	          end {if}
            else begin
              LoadMode:= lm_Relative;
              try
                XOffset:= st.NextNumber;
                YOffset:= st.NextNumber;
                except begin XOffset:= 0; YOffset:= 0; end;
              end; {try}
            end; {else}
          end; {'R':}
	        'A':LoadMode:= lm_Absolute;
	        'B':begin {start a pattern block} end;
	        'E':begin {end a pattern block} end;
          //This exception will make it to the user.
          //A dialog is shown with the text below.
	        'I':raise Exception.Create('Can''t read XLife #I format yet');
	        'r':try
	          AUniverse.RuleString:= (Copy(st.TokenString,st.Pos,(Length(st.TokenString)-(st.Pos-1))));
	          except AUniverse.RuleString:= DefaultRules;
	        end; {'r':}
	      end; {case}
	    end {if}
	    else if not CharInSet(Start[1],['!']) then case LoadMode of
	      lm_Absolute:try
	        x1:= st.NextNumber;
	        y1:= st.NextNumber;
	        AUniverse.changeCel(x1,y1,true);
	        except {ignore this line}
	      end; {lm_absolute}
	      lm_Relative:try
	        x1:= st.NextNumber;
	        y1:= st.NextNumber;
	        AUniverse.ChangeCel(XOffset+x1,YOffset+y1,true);
	        except {ignore this line}
	      end; {lm_Relative}
	      lm_Picture:begin
	        x1:= 0;
	        EmptyLine:= true;
	        for j:= 1 to Length(st.TokenString) do begin
	          case st.TokenString[j] of
              '$':begin //replace a "$" with 10 blanks.
                Inc(x1,10);
                EmptyLine:= false;
              end; {'$':}
              //Noam Elkies initials inside pattern description should be
              //treaded as blanks.
	            '.',' ','N','D','E':begin
	              Inc(x1);
	              EmptyLine:= false;
	            end; {'.':}
	            'O','o','*','@':begin //'@' used by NDE.
	              EmptyLine:= false;
	              AUniverse.ChangeCel(XOffset+x1,YOffset,true);
	              Inc(x1);
	            end; {'*':}
	          end; {case}
	        end; {for j}
	        if not EmptyLine then Inc(YOffset);
	      end; {lm_Picture}
	    end; {case}
	  end; {for i}
    finally st.Free;
  end; {try}
end;

//ProLife does not use a text file, but a binary one.
//So any binary file is send to this reader, which checks to see
//if it really is a valid ProLife file.
function ReadProLife(AFile: string; AUniverse: TUniverse): Boolean;
var
  BoundaryCondition: word;
  x,xMax,y,yMax: word;
  LifeField: array[0..19,0..159] of word;
  LifeFile: file of word;
  Mask: word;
  i: integer;
begin
  Result:= false;
  try
    AssignFile(LifeFile,AFile);
    Reset(LifeFile);
    //The above two statements open the file for READING.

    //do some checking to ensure we have a valid ProLife file.
    Read(LifeFile,BoundaryCondition);
    Read(LifeFile,xMax);
    if (XMax > 19) then Abort;
    Read(LifeFile,yMax);
    if (YMax > 159) then Abort;
    i:= FileSize(LifeFile) * SizeOf(Word);
    if i <> (((XMax+1) * (YMax+1) * 2)+ (2* 3)) then Abort;

    //All seems OK, so lets set the rules to conway's Default;
    //ProLife only supports Conway's default, so there is no doubt.
    AUniverse.RuleString:= DefaultRules;
    LastFileRules:= DefaultRules;

    //Read the stuff into a B I T map.
    for y:= 0 to yMax do begin
      for x:= 0 to xMax do begin
        Read(LifeFile,LifeField[x,y]);
      end; {for x}
    end; {for y}
    CloseFile(LifeFile);

    //Now we read the lifefield bit by bit and store it into the universe.
    for y:= 0 to yMax do begin
      for x:= 0 to xMax do begin
        Mask:= $8000;
        for i:= 0 to 15 do begin
          if (LifeField[x,y] and Mask) <> 0 then AUniverse.ChangeCel((x*16)+i,y,true);
          Mask:= Mask shr 1;
        end; {for i}
      end; {for x}
    end; {for y}
    Result:= true; //succes.
    //Catch any exceptions here.
    except CloseFile(LifeFile);
  end; {try}
end;

type
  TBGR = array[0..3] of byte;

function ReadBitmap(ABitmap: TBitmap; AUniverse: TUniverse): boolean;
var
  x,y: integer;
  Color: integer;
  Dark: boolean;
  BGR: TBGR;
begin
  //Light:= RGB(127,127,127);
  with ABitmap do begin
    TransparentMode:= tmFixed;
    //Monochrome:= true;
    for y:= 0 to Height -1 do for x:= 0 to Width -1 do begin
      Color:= Canvas.Pixels[x,y];
      BGR:= TBGR(ColorToRGB(Color));
      Dark:= BGR[0]+BGR[1]+BGR[2] <= (DarkTreshhold*3);
      AUniverse.ChangeCel(x,y,Dark);
    end; {for y,x}
  end; {with}
  Result:= true;
end;

function WriteBitmap(ABitmap: TBitmap; AUniverse: TUniverse): boolean;
var
  BoundingRect: TRect;
  x,y: integer;
begin
  ABitmap.Monochrome:= true;
  BoundingRect:= AUniverse.GetBoundingBox;
  with BoundingRect, ABitmap do begin
    Height:= Bottom - Top;
    Width:= Right - Left;
    for x:= 0 to Width -1 do for y:= 0 to Height -1 do begin
      case AUniverse.CelState(x+Left, y+Top) of
        true: Canvas.Pixels[x,y]:= clBlack;
        false: Canvas.Pixels[x,y]:= clWhite;
      end; {case}
    end; {for x,y}
  end; {with}
  Result:= true;
end;


function WriteLife(LifeLines: TStringList; AUniverse: TUniverse;
                   Fileformat: integer; IncludeTorusData: boolean): Boolean;
var
  BoundingRect: TRect;
begin
  Result:= true;
  BoundingRect:= AUniverse.ClipRect;
  if not(IsEmptyRect(BoundingRect)) then case FileFormat of
    //In default mode the program chooses between a picture file
    //and RLE based on the width and height of the pattern.
    smDefault: begin
      with BoundingRect do begin
      if ((Right - Left + 1) > MaxPictureWidth) or
         ((Bottom - Top + 1) > MaxPictureHeight) then
        Result:= WriteRLE(LifeLines, AUniverse, BoundingRect, IncludeTorusData)
      else Result:= WriteXLife(LifeLines, AUniverse, BoundingRect, IncludeTorusData);
    end
    end; {smDefault:}
    smLife105: Result:= WriteLife105(LifeLines, AUniverse, BoundingRect, IncludeTorusData);
    smRLE,smDBLife: Result:= WriteRLE(LifeLines, AUniverse, BoundingRect, IncludeTorusData);
    smXLife20: Result:= WriteXLife(LifeLines, AUniverse, BoundingRect, IncludeTorusData);
    smLife106: Result:= WriteLife106(LifeLines, AUniverse, BoundingRect, IncludeTorusData);
  end {case}
  else Result:= false;
end;

//This function takes 80x80 snapshots of the universe and uses the internal
//procedure AppendPicturePart to add this snapshot to the stringlist.
function AppendPicture(LifeLines: TStringList; AUniverse: TUniverse; BoundingRect: TRect): Boolean;

  //I'll need to delete any trailing dots from this in a later state.
  procedure AppendPicturePart(PartBox: TRect);
  var
    x,y: integer;
    ALine: string;
  begin
    //This will give a rectangle where any blank lines on the outside are
    //discarded.
    AUniverse.ShrinkSelRect(PartBox);
    with PartBox do begin
      LifeLines.Add('#P '+IntToStr(Left)+' '+IntToStr(Top));
      for y:= Top to Bottom-1 do begin
        ALine:= '';
        for x:= Left to Right-1 do begin
          if AUniverse.CelState(x,y) then ALine:= ALine + '*'
          else ALine:= ALine + '.';
        end; {for x}
        LifeLines.Add(ALine);
      end; {for y}
    end; {with}
  end; {function AppendPicturePart}

var
  PartBox: TRect;
begin
  Result:= true;
  try
    with BoundingRect do begin
      PartBox.Top:= Top;
      repeat
        //Let the user know about the progress of the save operation.
        //The write routines lets the universe know.
        //The universe lets it's owner know (the LifeBox in Life32).
        //And the LifeBox lets the main Window know.
        //The main window in turn shows the progress to the user, and checks
        //to see if the user pressed <esc>. If so it will ask this routine
        //to cancel. (if the user presses esc. SaveProgress will return
        //true (maybe false would be more approriate, ah hell :-), and
        //the procedure will "exit"
        if AUniverse.SaveProgress(PartBox.Top-Top) then exit; //give the user the chance to cancel.
        PartBox.Left:= Left;
        PartBox.Right:= Min(Right, PartBox.Left + MaxPictureWidth);
        PartBox.Bottom:= Min(Bottom, PartBox.Top + MaxPictureWidth);
        AppendPicturePart(PartBox);

        while PartBox.Right < Right do begin
          PartBox.Left:= PartBox.Right;
          PartBox.Right:= Min(Right, PartBox.Left + MaxPictureWidth);
          AppendPicturePart(PartBox);
        end; {while}
        PartBox.Top:= PartBox.Bottom;
      until (PartBox.Bottom = Bottom);
      //were all done now, do show 100% though :-)
      AUniverse.SaveProgress(PartBox.Bottom-Top);
    end; {with}
    except Result:= false;
  end; {try}
end;

//Both the routines below use the append picture routine.
//They only save different headers.
function WriteLife105(LifeLines: TStringList; AUniverse: TUniverse;
                      BoundingRect: TRect; IncludeTorusData: boolean): Boolean;
var
  i: integer;
begin
  Result:= true;
  LifeLines.Clear;
  LifeLines.Add(cstrLife105Header);
  //If torusdata, then start with that.
  if IncludeTorusData then WriteTorusData(LifeLines,AUniverse);
  with AUniverse.Description do begin
    i:= 0;
    while (i < count) and (i < MaxDescLines) do begin
      LifeLines.Add('#D '+strings[i]);
      Inc(i);
    end; {while}
  end; {with}
  if AUniverse.Rulestring <> DefaultRule then
    LifeLines.Add('#R '+Uppercase(StripPrefix(AUniverse.Rulestring)))
  else LifeLines.Add('#N');
  Result:= Result and AppendPicture(LifeLines, AUniverse, BoundingRect);
end;

function WriteXLife(LifeLines: TStringList; AUniverse: TUniverse;
                    BoundingRect: TRect; IncludeTorusData: boolean): Boolean;
var
  i: integer;
begin
  Result:= true;
  LifeLines.Clear;
  //If torusdata, then start with that.
  if IncludeTorusData then WriteTorusData(LifeLines,AUniverse);
  if AUniverse.RuleString <> DefaultRule then
    LifeLines.Add('#r '+Uppercase(StripPrefix(AUniverse.RuleString)));
  Result:= Result and AppendPicture(LifeLines, AUniverse, BoundingRect);
  i:= AUniverse.Description.Count;
  while i > 0 do begin
    LifeLines.Insert(0,'#C'+AUniverse.Description[i-1]);
    Dec(i);
  end; {while}
end;

function WriteLife106(LifeLines: TStringList; AUniverse: TUniverse;
                      BoundingRect: TRect; IncludeTorusData: boolean): Boolean;
var
  i: integer;
  x,y: integer;
  //width, Height: integer;
  ProgressGrid: integer;
begin
  Result:= true;
  LifeLines.Clear;
  LifeLines.Add(cstrLife106Header);
  //If torusdata, then start with that.
  if IncludeTorusData then WriteTorusData(LifeLines,AUniverse);
  with AUniverse.Description do begin
    i:= 0;
    while (i < count) and (i < MaxDescLines) do begin
      LifeLines.Add('#D '+strings[i]);
      Inc(i);
    end; {while}
  end; {with}
  if AUniverse.Rulestring <> DefaultRule then
    LifeLines.Add('#R '+Uppercase(StripPrefix(AUniverse.Rulestring)))
  else LifeLines.Add('#N');
  with BoundingRect do begin
    //do some math to make sure the progressbar (see AppendPicture for details)
    //is updated every 5% of the operation.
    ProgressGrid:= Max((Bottom-Top) div 20,1);
    Height:= (Bottom - Top);
    Width:= (Right - Left);
    for y:= Top to Bottom-1 do begin
      if ((y-top) mod ProgressGrid) = 0 then begin
        if AUniverse.SaveProgress(y-Top) then Exit; //if user gets tired of waiting, he can cancel the operation.
        //*** program exit point here.
      end; {if}
      for x:= Left to Right-1 do begin
        if AUniverse.CelState(x,y) then
          LifeLines.Add(IntToStr((x - Left) - (width div 2))+' '+IntToStr((y - Top)-(Height div 2)));
      end; {for x}
    end; {for y}
  end; {with}
end;


//RLE is short for Run Length Encoded.
//The save algorithm uses these runs. Every time a run ends, WriteRLEItem
//is called. This routine also keeps track of the length of each line,
//so no line exceeds 70 chars.
procedure WriteRLEItem(LifeLines: TStringList; Number: integer; Action: char);
var
  buf: string;
begin
  buf:= '';
  if Number <> 0 then begin
    if Number = 1 then buf:= Action
    else if Number = 2 then buf:= action + action
    else buf:= IntToStr(Number) + Action;
  end; {if}
  with LifeLines do begin
    if (Count = 1) or ((Length(Strings[Count-1]) + Length(Buf)) > RLEMAX) then Add(buf)
    else Strings[Count-1]:= Strings[Count-1] + Buf;
  end; {with}
end;    //LifeLines[1]


function WriteRLE(LifeLines: TStringList; AUniverse: TUniverse;
                  BoundingRect: TRect; IncludeTorusData: boolean): Boolean;
var
  ALine: string;
  x,y,x1,y1: integer;
  OldState: boolean;
  Action: char;
  i: integer;
  ProgressGrid: integer;
begin
  Result:= true; //assume succes
  with BoundingRect do begin
    //report progress per 5%
    ProgressGrid:= Max((Bottom-Top) div 20,1);
    Inc(Right,1);    //I don't know why, but without this there are errors.
    LifeLines.Clear;
    //If torusdata, then start with that.
    if IncludeTorusData then begin
      WriteTorusData(LifeLines,AUniverse);
      //Absolute x and y
      ALine:= 'x = '+IntToStr(Left)+', y = '+IntToStr(Top);
    end
    else begin
      //Width, Height in starting line.
      ALine:= 'x = '+IntToStr(Right-Left-1)+', y = '+IntToStr(Bottom-Top);
    end;
    if AUniverse.RuleString <> DefaultRule then
      ALine:= ALine + ', rule = '+Uppercase(StripPrefix(AUniverse.RuleString));
    LifeLines.Add(ALine);
    //Start a new line for the RLE data.
    LifeLines.Add('');

    //do the actual writing of the RLE pattern, calling WriteRLEItem
    //every time a run ends.
    with AUniverse do begin
      OldState:= CelState(Left,Top); //start with good state in mind.
      y1:= Top;
      for y:= Top to Bottom do begin
        x1:= Left;
        if ((y-top) mod ProgressGrid) = 0 then begin
          if AUniverse.SaveProgress(y-Top) then Exit; //if user gets tired of waiting, he can cancel the operation.
          //*** program exit point here.
        end;
        for x:= Left to Right do begin
          //are we at the end of our run yet.
          if CelState(x,y) <> OldState then begin
            //have we started a new line? Quick! Better write a "$" first.
            if (y1 <> y) then begin
              WriteRLEItem(LifeLines,(y-y1),'$');
              y1:= y;
            end; {if}
            if (x1 <> x) then begin
              if OldState then Action:= 'o' else Action:= 'b';
              WriteRLEItem(LifeLines,(x-x1),Action);
              x1:= x;
            end; {if}
            OldState:= not(OldState) //update OldState
          end; {if}
        end; {for x}
      end; {for y}
    end; {with}
    //The RLE section ends with a "!"
    WriteRLEItem(LifeLines,1,'!');
  end; {with}
  //Anything, that means anything at all *after* the "!" is a
  //comment.
  i:= 0;
  while i < AUniverse.Description.Count do begin
    LifeLines.Add(AUniverse.Description[i]);
    Inc(i);
  end; {while}
end;

procedure WriteTorusData(LifeLines: TStringList; AUniverse: TUniverse);
//var
//  ALine: string;
begin
  //not implemented yet !!!
  (*with AUniverse.Limit do begin
    ALine:= '#TD '+IntToStr(Left)+', '+IntToStr(Top)+', '
                  +IntToStr(Right - Left + 1) +', '+ IntToStr(Bottom - Top + 1);
    LifeLines.Add(ALine);
    ALine:= '#TO '+'wrap = '+IntToStr(integer(AUniverse.DeadEdges))+', '
                  +'kind = '+IntToStr(AUniverse.TorusKind);
    LifeLines.Add(ALine);
  end;(**)
end;

//That's all folks.
end.
