unit LifeRules;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

//Unit is based on:
(*************************************************************
 LifeRules.java

 This class exists to convert a rule string (i.e. "23/3") into
 an array of 512 booleans representing the 3x3 neighborhood.
 The bits, in order of 123456789, are laid out in order:

 123
 456
 789

 so that, for example, the hex number 0x1ca would represent

 ***
 ..*
 .*.

 (c) Alan Hensel, Aug 2000. All Rights Reserved.
**************************************************************)

uses
  LifeConst;

type
  TNonTotArray = array [false..true,0..8,0..13] of boolean;

type
  TLifeRules = class(TObject)
  private
    FRuleArray: TRuleArray;
    FRuleString:string;
    FNeighborhood: integer;
    FMaxNeighbors: integer;
    //FOnRuleChange: TRuleChangeEvent;
    function FlipBits(x:integer):integer;
    function RotateBits90Clockwise(x:integer):integer;
    procedure SetSymmetricalNeighborhood(num: integer; survival:boolean;
        c:char; b:boolean);
    procedure SetMopNeighborhood(num: integer; survival: boolean;c: char;
    b: boolean);
    procedure SetSymmetricalNeighborhood2(x:integer; b:boolean);

    procedure SetTotalisticNeighborhood(num:integer; survival,b:boolean);
    function StripOffNeighborhood(ARule: string): string;
    function TryMinusPart(Part: string; number: integer): string;
    procedure ShortenRule;
    function GetStringFromRuleArray: string;
  protected
    procedure SetRules(rules: string; survival: boolean);
    procedure ConvertRules(rule:string);
    procedure SetNeighborhood(N: integer);
    function GetRules: string;
  public
    constructor Create;
    function IsValidRule(TestRule: string): boolean;
    function SetNTState(AState: string; Add: boolean):string;
    function GetNonTot: TNonTotArray;
    property Rulestring: string read GetRules write ConvertRules;
    property Neighborhood: integer read FNeighborhood write SetNeighborhood;
    property RuleArray: TRuleArray read FRuleArray;
    //property OnRuleChange: TRuleChangeEvent read FOnRuleChange write FOnRuleChange;
  end;

implementation

uses
  sysutils, WinTypes, LifeUtil;

constructor TLifeRules.Create;
begin
  inherited Create;
  FMaxNeighbors:= DefaultMaxNeighbors;
  FNeighborhood:= nbDefault;
end;

procedure TLifeRules.SetNeighborhood(N: integer);
begin
  FNeighborhood:= N;
  //force recalc of rulestring;
  ConvertRules(FRuleString);
end;

function TLifeRules.GetRules: string;
var
  i: integer;
  RuleString: string;
begin
  Rulestring:= lowercase(FRuleString);
  if (Pos('/',FRuleString) = 0) then RuleString:= lowercase(DefaultRules);
  Result:= RuleString;
  //strip the prefix.
  i:= pos(':',result);
  if i <> 0 then delete(result,1,i);
  //Add an S before the first part and an B before the part after the '/'.
  if not CharInSet(Result[1],['/','s','b','S','B']) then Result:= 's'+Result;
  if (pos('b',Result) = 0) and (pos('B',Result) = 0) then Insert('b',Result,Pos('/',Result)+1);
  //Insert the prefix again
  Result:= NeighborhoodToStr(Neighborhood)+result;
end;


(* The main event of this class: get a rule array laid out
 * according to the human-readable string.
 *)
procedure TLifeRules.ConvertRules(rule:string);
var
  i: integer;
  SlashIndex: integer;
  LeftRules, RightRules: string;
  bs: boolean;
begin
  if rule = '' then rule:= 'M:23/3';
  for i:= 0 to 511 do FRuleArray[i]:= false;

  Rule:= Lowercase(Rule);
  Rule:= StripOffNeighborhood(Rule);
  slashIndex:= Pos('/',Rule);

  //if (slashIndex < 0) return null;  // no slash!
  leftRules:= Copy(rule,1,slashIndex-1);
  rightRules:= Copy(rule,slashIndex+1,Length(Rule)-(SlashIndex));

  // determine which set of digits is actually survival, birth
  bs:= (Pos('b',leftRules) > 0) or (Pos('s',rightRules) > 0);

  setRules(leftRules, not(bs));
  setRules(rightRules, bs);

  FRuleString:= NeighborhoodtoStr(Neighborhood)+rule;
end;

(** Set half of the rules.
 * survival = true for survival, false for birth.
 *)
procedure TLifeRules.setRules(rules:string; survival:boolean);
var
  i: integer;
  num: integer;
  PeekAhead: char;
  b: boolean;
begin
  rules:= rules +' ';  // for safety (1-character peekaheads occurring)

  i:= 1;
  while i < Length(rules) do begin
    num:= integer(rules[i]) - $30;
    if (((num > 0) and (num <= 9)) or ((num = 0) and survival)) then begin
      peekAhead:= rules[i+1];

      if (peekAhead < 'a') or CharInSet(peekAhead,['b','s']) then setTotalisticNeighborhood(num, survival,true);

      b:= true;
      if (peekAhead = '-') then begin
        b:= false; // invert meaning of chars that follow.
        Inc(i);
        peekAhead:= rules[i+1];
      end; {if}

      if CharInSet(PeekAhead,['m','o','p']) and (Neighborhood = nbHex) then begin
        setMOPNeighborhood(num, survival, peekAhead, b);
      end
      else while (peekAhead >= 'a') and (peekAhead <= 'z') do begin
        setSymmetricalNeighborhood(num, survival, peekAhead, b);
        Inc(i);
        peekAhead:= rules[i+1];
      end; {while}
    end; {if}
    Inc(i);
  end; {while}
end;


function TLifeRules.flipBits(x:integer):integer;
begin
  Result:= ((x and $07) shl 6) or  ((x and $1c0) shr 6) or (x and $38);
  //return ((x & 0x07)<<6) | ((x & 0x1c0)>>>6) | (x & 0x38);
end;

function TLifeRules.rotateBits90Clockwise(x: integer): integer;
begin
  Result:=                     //  return
        ((x and $04) shl 6)    //     ((x & 0x04) << 6)
     or ((x and $20) shl 2)    //     | ((x & 0x20) << 2)
     or ((x and $100) shr 2)   //     | ((x &0x100) >>> 2)
     or ((x and $02) shl 4)    //     | ((x & 0x02) << 4)
     or  (x and $10)           //     |  (x & 0x10)
     or ((x and $80) shr 4)    //     | ((x & 0x80) >>> 4)
     or ((x and $01) shl 2)    //     | ((x & 0x01) << 2)
     or ((x and $08) shr 2)    //     | ((x & 0x08) >>> 2)
     or ((x and $40) shr 6);   //     | ((x & 0x40) >>> 6);
end;

// non-totalistic rule extensions
const
  rule_letters: array[0..3] of string =
  ('ce',
   'cekaiv',
   'cekaivyqjr',
   'cekaivyqjrtwz');



const
  rule_neighborhoods: array[0..3,0..12] of integer =

  (($01, $02, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000, 000),  //hex numbers
   ($05, $0a, $21, $03, $28, $44, 000, 000, 000, 000, 000, 000, 000),
   ($45, $2a, $62, $0b, $07, $0d, $29, $46, $0e, $61, 000, 000, 000),
  ($145, $aa, $63, $0f, $2d, $47, $2b, $66, $6a, $65, $69, $4e, $6c));

  mop_letters = 'mop';

  mop_neighborhoods: array [0..2,0..5] of integer =
  (($21,$102,$a0,$108,$81,$0a),
   ($03,$22,$120,$180,$88,$09),
   ($101,$82,$28,$101,$82,$28));


(* set Rule array by neighborhood type (of 51 possibilities) specified
 * by number of neighbors (num) and a letter of the alphabet (c).
 * survival:= true if survival rule, false if birth.
 * b:= true if adding, false if suppressing survival or birth. *)
procedure TLifeRules.setSymmetricalNeighborhood(num: integer; survival: boolean;c: char;
    b: boolean);
var
  xorbit: integer;
  nIndex: integer;
  LetterIndex: integer;
  x: integer;
begin
  if (num = 0) or (num = 8) then begin   // duh, no combos for homogeneous bits
    setTotalisticNeighborhood(num, survival,true);
    exit;
  end;

  xorbit:= 0;
  nIndex:= num-1;
  if (nIndex > 3) then begin
    nIndex:= 6-nIndex;
    xorbit:= $1ef;
  end;

  letterIndex:= Pos(c,rule_letters[nIndex])-1;
  if (letterIndex = -1) then exit;

  x:= rule_neighborhoods[nIndex][letterIndex] xor xorbit;
  if (survival) then x:= x or $10;

  setSymmetricalNeighborhood2(x, b);
end;

(* set Rule array by 9-bit neighborhood x.
 * b:= true if adding, false if suppressing survival or birth.
 *)
procedure TLifeRules.setSymmetricalNeighborhood2(x: integer; b: boolean);
var
  y,i: integer;
begin
  y:= x;

  for i:= 0 to 3 do begin
    FRuleArray[y]:= b;
    y:= rotateBits90Clockwise(y);
  end;

  y:= flipBits(y);
  for i:=0 to 3 do begin
    FRuleArray[y]:= b;
    y:= rotateBits90Clockwise(y);
  end;
end;


procedure TLifeRules.setMopNeighborhood(num: integer; survival: boolean;c: char;
    b: boolean);
var
  i: integer;
begin
  setTotalisticNeighborhood(num, survival,true);
  if (num <> 2) then exit;

  //letterIndex:= Pos(c,mop_letters)-1;
  //if (letterIndex = -1) then exit;

  for i:= 0 to 511 do begin
    if (survival) and Bool(i and bit4) and (CountNeighbors(i,Neighborhood)=2) then begin
      //kill everything NOT 'protected' by MOP.
      if (HexRule2MOP(i,Neighborhood) <> c) then FRuleArray[i]:= false;
    end;
    if not(survival) and not(Bool(i and bit4)) and (CountNeighbors(i,Neighborhood)=2) then begin
      //kill everything NOT 'protected' by MOP.
      if (HexRule2MOP(i,Neighborhood) <> c) then FRuleArray[i]:= false;
    end;
  end; {for i}
end;



(** set Rule array by number of neighbors.
 * survival:= true for survival rule, false for birth rule.
 *)
procedure TLifeRules.setTotalisticNeighborhood(num: integer; survival,b: boolean);
var
  mask: integer;
  i2,i,j: integer;
  neighbors, nborhood: integer;
begin
  if survival then mask:= $10 else mask:= 0;

  for i2:= 0 to 15 do begin            // 012
    i:= i2*32;                         // 3#.
    for j:= 0 to 15 do begin                      //.#5
      nborhood:= i+j;                             //678
      Neighbors:= CountNeighbors(nborhood,Neighborhood);
      if (num = neighbors) then FRuleArray[i+j+mask]:= b;
    end; {for}
  end; {for}
end;

function TLifeRules.StripOffNeighborhood(ARule: string): string;
var
  i: integer;
  Prefix: string;
  NewNeighborhood: integer;
begin
  //strip off prefix
  i:= Pos(':',ARule);
  if i = 0 then Prefix:= '' //default is Moore neighborhood.
  else begin
    Prefix:= Copy(ARule,1,i);
    Delete(Arule,1,i);
  end; {else}
  //if rule contains 'm', 'o' or 'p' then Hex neighborhood is assumed.
  if {Prefix = ''} true then begin
    if (Pos('m',ARule) + Pos('o',ARule) + Pos('p',ARule)) > 0 then
    Prefix:= 'h:';
  end;

  NewNeighborhood:= StrToNeighborhood(Prefix);
  if NewNeighborhood = nbUnchanged then NewNeighborhood:= Neighborhood;
  FMaxNeighbors:= MaxNeighborsInNeighborhood(NewNeighborhood);
  FNeighborhood:= NewNeighborhood; //don't use property to avoid endless loop.
  Result:= ARule;
end;

function TLifeRules.GetNonTot: TNonTotArray;
var
  i,x,j: integer;
  b: boolean;
  nIndex: integer;
  MaxJ: integer;
  xorbit: integer;
  AllTrueB,AllTrueS: boolean;
  MaxN: integer;
begin
  MaxN:= MaxNeighborsInNeighborhood(Neighborhood);
  for b:= false to true do for i:= 0 to 8 do for j:= 0 to 13 do
  Result[b,i,j]:= false;
  //Simple test for non-moore neighborhoods.
  if Neighborhood <> nbMoore then begin
    j:= 0;
    while (j < 512) do begin
      x:= (j and Neighborhood);
      //test birth
      if FRuleArray[x] then
        Result[true,CountNeighbors(x,Neighborhood),0]:= true;
      //test survival
      x:= x or $10;
      if FRuleArray[x] then
        Result[false,CountNeighbors(x,Neighborhood),0]:= true;
      Inc(j);
    end; {while}
  end {if not nbMoore}
  else begin

    //sample the rulearray at non-tot hotspots.
    //travel all the 8 states.
    for i:= 1 to MaxN-1 do begin
      xorbit:= 0;
      nIndex:= i-1;
      if (nIndex > 3) then begin
        nIndex:= 6-nIndex;
        xorbit:= $1ef;
      end;
      //sample all the states in the rulearray.
      MaxJ:= Length(Rule_letters[nIndex]);
      //if all state given a number are ON, no letters are needed.
      //note this in the NONtot rule array.
      AllTrueB:= true; AllTRueS:= true;
      for j:= 1 to MaxJ do begin
        //sample birth
        x:= rule_neighborhoods[nIndex][j-1] xor xorbit;
        Result[true,i,j]:= FRuleArray[x];
        AlltrueB:= AlltrueB and FRuleArray[x];
        //sample survival
        x:= x or $10;
        Result[false,i,j]:= FRuleArray[x];
        AlltrueS:= AlltrueS and FRuleArray[x];
      end; {for j}
      Result[true,i,0]:= AlltrueB;
      Result[false,i,0]:= AlltrueS;
    end; {for i}
    //sample 0.
    Result[true,0,0]:= FRuleArray[$00];
    Result[false,0,0]:= FRuleArray[$10];
    //Sample MaxN
    Result[true,MaxN,0]:= FRuleArray[$1ef];
    Result[false,MaxN,0]:= FRuleArray[$1ff];
  end; {else begin}
end;

function TLifeRules.GetStringFromRuleArray: string;
var
  NonTot: TNonTotArray;
  Sub, Whole: string;
  RuleSave: string;
  i,j,x: integer;
  b: boolean;
  MaxN: integer;
begin
  MaxN:= MaxNeighborsInNeighborhood(Neighborhood);
  Whole:= NeighborhoodtoStr(Neighborhood)+'S';
  NonTot:= GetNonTot;
  for b:= false to true do begin
    for i:= 0 to MaxN do begin
      Sub:= IntToStr(i);
      x:= i - 1;
      if x > 3 then x:= 6 - x;
      if NonTot[b,i,0] then begin
        Whole:= Whole + Sub;
        Sub:= '';
      end
      else if (i > 0) and (i < MaxN) then for j:= 1 to length(Rule_letters[x]) do begin
        if NonTot[b,i,j] then begin
          Sub:= Sub + rule_Letters[x][j];
          Whole:= Whole + Sub;
          Sub:= '';
        end; {if}
      end; {for j}
    end; {for i}
    if (pos('/B',Whole) = 0) then Whole:= Whole + '/B';
  end; {for b}
  //now shorten the rule.
  RuleSave:= FRuleString;
  FRuleString:= whole;
  ShortenRule;
  whole:= FRuleString;
  FRuleString:= RuleSave;
  Result:= whole;
end;

//adds state to non-tot rule string.
//returns the new rulestring.
//format of AState 3char string is 1:[SB] 2:[SBceaikvjqrytwz] 3:[12345678]
function TLifeRules.SetNTState(AState: string; Add: boolean):string;
var
  Sub: string;
  x,Number: integer;
  c: char;
  Survival: boolean;
begin
  Result:= '';
  if (Upcase(AState[1]) = 'S') then Survival:= true
  else if (Upcase(AState[1]) = 'B') then Survival:= false
  else exit;
  Sub:= AState[3];
  try Number:= StrToInt(Sub); except exit; end;
  if number > 8 then exit;
  c:= (Lowercase(AState)[2]);
  if (Number >= 1) and (Number <= 7) then begin
    x:= Number-1;
    if x > 3 then x:= 6 - x;
    if pos(c,rule_letters[x]) <> 0 then begin
      Self.setSymmetricalNeighborhood(Number, Survival, c, Add);
    end
    else if not CharInSet(c,['s','b']) then exit;
  end
  else if not CharInSet(c,['s','b']) then exit;
  if CharInSet(c,['s','b']) then begin
    Self.setTotalisticNeighborhood(Number,survival,add)
  end; {else}
  FRuleString:= GetStringFromRuleArray;
  Result:= FRuleString;
end;


function TLifeRules.TryMinusPart(Part: string; number: integer): string;
var
  whole: string;
  i,a: integer;
begin
  result:= part;
  case Number of
    1,7: exit; //only 2 options no need for "-" here.
    2,6: whole:= rule_letters[1];
    3,5: whole:= rule_letters[2];
    4: whole:= rule_letters[3];
    else exit;
  end; {case}
  for a:= 1 to length(part) do begin
    i:= 1;
    while i <= length(whole) do begin
      if Part[a] = whole[i] then begin
        delete(whole,i,1);
        i:= length(whole);
      end
      else Inc(i)
    end; {while}
  end; {for}
  if (length(whole) < Length(part)-1) then result:= '-' + whole;
end;

procedure TLifeRules.ShortenRule;
var
  i,Start,Count: integer;
  Number: integer;
  OldPart, NewPart: string;
begin
  i:= Pos(':',FRuleString)+1;
  Number:= 0;
  while (i < Length(FRuleString)) do begin
    //look for a number.
    while (i < Length(FRuleString)) and (not CharInSet(FRuleString[i],SpecialRules)) do begin
      if CharInSet(FRuleString[i],Numbers) then Number:= StrToInt(FRuleString[i]);
      Inc(i);
    end; {while}
    Start:= i;
    while (i <= Length(FRuleString)) and (not CharInSet(FRuleString[i],NormalRules)) do Inc(i);
    Count:= i-Start;
    OldPart:= copy(FRuleString,Start,Count);
    NewPart:= TryMinusPart(OldPart,Number);
    if NewPart <> OldPart then begin
      Delete(FRuleString,Start,Count);
      Insert(NewPart,FRuleString,Start);
      i:= i - length(OldPart);
      i:= i + length(NewPart);
    end; {if}
  end; {while}
end;


function TLifeRules.IsValidRule(TestRule: string): boolean;
var
  PosColon, PosSlash, PosS, PosB, PosNul: integer;
  AllowedShort, AllowedOct, NumbersAllowed, MopAllowed,ExtensionAllowed: set of ansichar;
  ARule, APart: string;
  i: integer;
  LastNumber: char;
  SFirst: boolean;
  NB: integer;
  loop: integer;
  NulRight, BirthRight: boolean;
begin
  TestRule:= Lowercase(TestRule);
  //Rule is always in lowercase.
  Result:= false;
  PosColon:= Pos(':',TestRule);
  PosSlash:= Pos('/',TestRule);
  if (PosSlash = 0) then PosSlash:= Pos('\',TestRule);
  PosS:= Pos('s',TestRule);
  //check the position of the colon.
  if (PosColon > PosSlash) then exit;
  if (PosColon > PosS) and (PosS > 0) then exit;
  if (PosColon = 1) then exit;
  //check the slash
  if (PosSlash = 0) then exit;
  //we will remove the frontpart, so adjust the slashpos to reflect this.
  PosSlash:= PosSlash - PosColon;

  //Now walk though the rule and see if it holds up.
  ARule:= trim(TestRule);
  i:= 1;
  NB:= nbMoore; //the default.
  //First check the prefix.
  if (PosColon > 0) then begin
    APart:= Copy(ARule,1,PosColon);
    Delete(ARule,1,PosColon);
    //strip off the ":".
    Delete(APart,Length(APart),1);
    AllowedShort:= ['m','h','v','x'];
    if CharInSet(APart[i],AllowedShort) then begin
      if Length(APart) > 1 then exit;
    end {if}
    else begin
      if (Length(APart) > 4) then exit;
      AllowedOct:= ['0'..'7'];
      while (i <= length(APart)) do begin
        if not CharInSet(APart[i],AllowedOct) then exit;
        Inc(i);
      end; {while}
    end; {else}
    //we are here, so the neighborhood must be OK.
    NB:= StrToNeighborhood(Copy(TestRule,1,PosColon));
    if (NB and $10) <> 0 then exit;
  end; {if}
  //Check to see if the is no '0' in the birth part.
  PosS:= Pos('s',ARule);
  PosB:= Pos('b',ARule);
  PosNul:= Pos('0',ARule);
  if PosNul <> 0 then begin
    NulRight:= (PosNul > PosSlash);
    if PosB <> 0 then BirthRight:= (PosB > PosSlash)
    else if PosS <> 0 then BirthRight:= (PosS < PosSlash)
    else BirthRight:= true;
    if NulRight = BirthRight then exit;
  end;
  //now check the rule proper.
  //strip off the part before the slash
  APart:= Copy(ARule,1,PosSlash-1);
  Delete(ARule,1,PosSlash);
  //first check the part before the slash, then after.
  SFirst:= (PosS < PosSlash);
  for loop:= 1 to 2 do begin
    i:= 1;
    LastNumber:= ' ';
    NumbersAllowed:= ['s','b','0'..Char(MaxNeighborsInNeighborhood(Neighborhood)+ord('0'))];
    MopAllowed:= ['m','o','p'];
    ExtensionAllowed:= [];
    while i <= Length(APart) do begin
      if CharInSet(APart[i],NumbersAllowed) then begin
        //S and B are each only allowed once, and not on the same side of the "/".
        if CharInSet(APart[i],['s','b']) then begin
          if loop = 1 then SFirst:= (APart[i] = 's')
          else begin
            if SFirst and (APart[i] = 's') then exit;
            if not(SFirst) and (APart[i] = 'b') then exit;
          end;
          NumbersAllowed:= NumbersAllowed - ['s','b'];
        end
        else begin //must be a number
          LastNumber:= APart[i];
          NumbersAllowed:= NumbersAllowed - [LastNumber];
          case LastNumber of
            '1','7': ExtensionAllowed:= ['-','c','e'];
            '2','6': ExtensionAllowed:= ['-','c','e','k','a','i','v'];
            '3','5': ExtensionAllowed:= ['-','c','e','k','a','i','v','y','q','j','r'];
            '4': ExtensionAllowed:= ['-','c','e','k','a','i','v','y','q','j','r','t','w','z'];
            else ExtensionAllowed:= [];
          end;
        end;
      end
      //not in numbers allowed, Hex allowed for MOP after a '2'.
      else if (NB = nbHex) and (LastNumber = '2') then begin
        if not CharInSet(APart[i],MopAllowed) then exit
        else MopAllowed:= MopAllowed - [APart[i]];
      end
      //not in numbers allowed, see if Moore extensions apply
      else if (NB = nbMoore) and (LastNumber <> ' ') then begin
        if not CharInSet(APart[i],ExtensionAllowed) then exit
        else ExtensionAllowed:= ExtensionAllowed - [APart[i]];
      end
      else exit;
      Inc(i);
    end; {while}
    APart:= ARule;
  end; {for loop}
  Result:= true;
end;

end.


