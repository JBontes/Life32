(**************************************************************
based on:
 LifeHash.java
 This class keeps a fast-lookup table of LifeCell blocks.
  (c) Alan Hensel, Apr 1996. All Rights Reserved.
**************************************************************)
unit LifeHash;

(* This Source Code Form is subject to the terms of the Mozilla Public
 * License, v. 2.0. If a copy of the MPL was not distributed with this
 * file, You can obtain one at http://mozilla.org/MPL/2.0/. *)

interface

uses
  Classes, LifeCel;

const
  HashSize = 6;
  FMax = 63;
  Mask = FMax and FMax shl 16;

type
  TLifeHash = class(TObject)
  private
    HashTable: array[0..FMax,0..FMax] of TLifeCel; //64x64x4 = 16k.
    keyX, keyY: integer;
    Cache: TLifeCel;
    NextCache: TLifeCel;
    NextX, NextY: integer;
    //procedure MakeKeys(x,y: integer);
    function RetrieveFast(x,y:smallint): TLifeCel; //coordinates are div 16.
  public
    procedure Clear;
    procedure Store(c: TLifeCel);
    function Retrieve(x, y: integer):TLifeCel;
    function RetrieveCached(x,y: integer): TLifeCel;
    procedure Delete(c: TLifeCel);
    function First: TLifeCel;
    function Next: TLifeCel;
  end;

implementation

(*procedure TLifeHash.MakeKeys(x, y: integer);
begin
  keyX:= (x and FMax);
  keyY:= (y and FMax);
end; (**)

procedure TLifeHash.Store(c: TLifeCel);
var
  x,y,xp,yp,xm,ym: integer;
begin
  x:= c.coor.x;
  y:= c.coor.y;
  xp:=(x+1); xm:=(x-1);  //pin
  yp:=(y+1); ym:=(y-1);  //pin
  KeyX:= (x and FMax);
  KeyY:= (y and FMax);
  c.Down:= HashTable[keyX,keyY];
  HashTable[keyX,keyY]:= c;

  //x:= x * 16;
  //y:= y * 16;
  //xp:=(x+16); xm:=(x-16);  //pin
  //yp:=(y+16); ym:=(y-16);  //pin

  c.SE:= RetrieveFast(xp,yp);
  if Assigned(c.SE) then begin
    c.SE.NW:= c;
    c.S:= c.SE.W;
    if Assigned(c.S) then c.S.N:= c;
    c.E:= c.SE.N;
    if Assigned(c.E) then c.E.W:= c;
  end {if}
  else begin
    c.S:= RetrieveFast(x, yp);
    if Assigned(c.S) then c.S.N:= c;
    c.E:= RetrieveFast(xp,y);
    if Assigned(c.E) then c.E.W:= c;
  end; {else}
  c.NW:= RetrieveFast(xm,ym);
  if Assigned(c.NW) then begin
    c.NW.SE:= c;
    c.N:= c.NW.E;
    if Assigned(c.N) then c.N.S:= c;
    c.W:= c.NW.S;
    if Assigned(c.W) then c.W.E:= c;
  end {if}
  else begin
    c.N:= RetrieveFast(x, ym);
    if Assigned(c.N) then c.N.S:= c;
    c.W:= RetrieveFast(xm,y);
    if Assigned(c.W) then c.W.E:= c;
  end; {else}
end;

function TLifeHash.retrieve(x, y: integer):TLifeCel;
var
  s: TCoor;
begin
  s.x:= x shr 4;
  s.y:= y shr 4;

  KeyX:= (s.x and FMax);
  KeyY:= (s.y and FMax);
  Result:= HashTable[keyX,keyY];
  while (Assigned(Result) and (Result.coor.xy <> s.xy)) do
    Result:= Result.Down;
end;

function TLifeHash.RetrieveFast(x,y: smallint): TLifeCel;
begin
  KeyX:= (x and FMax);
  KeyY:= (y and FMax);
  Result:= HashTable[KeyX,KeyY];
  while (Assigned(Result) and ((Result.coor.x <> x) or (Result.coor.y <> y))) do
    Result:= Result.Down;
end;

function TLifeHash.RetrieveCached(x,y: integer): TLifeCel;
var
  s: TCoor;
begin
  s.x:= x shr 4;
  s.y:= y shr 4;
  if Assigned(Cache) and (Cache.coor.xy = s.xy) then Result:= Cache
  else begin
    Result:= RetrieveFast(s.x,s.y);
    Cache:= Result;
  end; {else}
end;


procedure TLifeHash.Delete(c: TLifeCel);
var
  cur: TLifeCel;
  prev: TLifeCel;
begin
  //Clear the cache, just to be sure.
  Cache:= nil;
  KeyX:= (c.coor.x and FMax);
  KeyY:= (c.coor.y and FMax);
  cur:= HashTable[keyX,keyY];

  prev:= nil;
  while (cur.coor.xy <> c.coor.xy) do begin
    prev:=cur;
    cur:=cur.Down;
  end; {while}

  if Assigned(prev) then prev.Down:=cur.Down
  else HashTable[keyX,keyY]:=cur.Down;
  cur.Down:=nil;
end;

procedure TLifeHash.Clear;
var
  x,y: integer;
  c,cN: TLifeCel;
begin
  //Clear the cache.
  Cache:= nil;
  for x:= 0 to FMax do for y:= 0 to FMax do begin
    c:= HashTable[x,y];
    HashTable[x,y]:= nil;
    while Assigned(c) do begin
      cN:= c.Down;
      c.Down:= nil;
      c:= cN;
    end; {while}
  end; {for}
end;

function TLifeHash.First: TLifeCel;
var
  x,y: integer;
begin
  //Search hastable, left to right, then top to bottom for cells
  //topleft = 0,0 bottomright =63,63.
  x:= 0;y:= 0; Result:= nil;
  while (y < 64) and (Result = nil) do begin
    while (x < 64) and (Result = nil) do begin
      Result:=  HashTable[x,y];
      if Result <> nil then begin
        NextCache:= Result;
        Nextx:= x;
        Nexty:= y;
      end;
      Inc(x);
    end; {while}
    x:= 0;
    Inc(y);
  end; {while}
end;

function TLifeHash.Next: TLifeCel;
var
  x,y: integer;
begin
  x:= Nextx; y:= Nexty;
  Result:= nil;
  if NextCache = nil then exit;

  Result:= NextCache.Down;
  Inc(x);
  while (y < 64) and (Result = nil) do begin
    while (x < 64) and (Result = nil) do begin
      Result:= HashTable[x,y];
      if Result <> nil then begin
        Nextx:= x;
        Nexty:= y;
      end;
      Inc(x);
    end; {while}
    x:= 0;
    Inc(y);
  end; {while}
  NextCache:= Result;
end;

end.


