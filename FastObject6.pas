unit FastObject6;

interface

uses Classes, LifeConst;

type

  TFastObject = class(TPersistent)
  public
    class procedure InitStorageFacility(AClass: TClass);
    class procedure FreeStorageFacility;
    class function NewInstance: TObject; override;
    procedure FreeInstance; override;
    class function Create: pointer;
  end;

var
  BlockSize: integer = DefaultMemPerBlock;

implementation  //##########################

uses SysUtils, Windows, LifeCel, Prefs;

const
  CacheSize = 100;

type
  //used to directly manipulate the properties of a list
  //@@@@ Big hack alert.
  THackList = class(TObject)
  private
    FList: PPointerList;
    FCount: Integer;
  end;

  PPointer = ^Pointer;
  PCardinal = ^Cardinal;

  PBrick = ^TBrick;
  TBrick = record
    NextBrick: PBrick;
  end;

  PBlock = ^TBlock;
  TBlock = record
    NextBrick: PBrick;
    FBricksInUse: Cardinal;
    Next: PBlock;
    Prev: PBlock;
    CacheNext: PBlock;
    CacheCount: integer;
  end;


  TStorageFacility = class(TObject)
  private
    //FUsedBlocks: TList; //list of TStorageBlocks in use
    FFreeBlocks: TList; //List of available Blocks in virtual memory.
    DataBuffer: PBlock;
    BlockList: TBlock;
    VirtualSize: Cardinal;

  public
    constructor Create(ObjectSize, ObjectCount: Cardinal);
    destructor Destroy; override;
    function NewBlock: PBlock;
    function CreateBrick: TFastObject;
    procedure FreeBrick(ABrick: TFastObject);
  end;




(********************** TStorageFacility **************)

var
  BrickSize: Cardinal; //StorageFacility only works with same sized blocks!
                          //BrickSize holds the InstanceSize of the Objects
                          //that will be stored in the block.
  BricksPerBlock: Cardinal;

constructor TStorageFacility.Create(ObjectSize, ObjectCount: Cardinal);
var
  i,b: cardinal;
  MemStatus: TMemoryStatus;
begin
  inherited Create;
  BrickSize:= ObjectSize;
  BricksPerBlock:= BlockSize div BrickSize;
  //Reserve space in virtual memory, 2x physical mem, so the comp. will
  //start trashing horrible before running out of mem.
  //Instead of giving an error.
  GlobalMemoryStatus(MemStatus);
  VirtualSize:= MemStatus.dwTotalPhys * 2;
  repeat
    DataBuffer:=
      PBlock(VirtualAlloc(nil,VirtualSize,MEM_RESERVE or MEM_COMMIT {or MEM_TOP_DOWN}, PAGE_READWRITE));
    //Actual physical pages are not allocated unless/until the virtual addresses are actually accessed.
    if (DataBuffer = nil) then VirtualSize:= VirtualSize div 2;
  until (DataBuffer <> nil);

  i:= VirtualSize div BlockSize;
  //init the list of used blocks.
  BlockList.Next:= @BlockList;
  BlockList.Prev:= @BlockList;
  //BlockList.CacheNext:= nil;

  //Init the list of free blocks.
  FFreeBlocks:= TList.Create;
  FFreeBlocks.Capacity:= i;
  while i > 0 do begin
    Dec(i);
    b:= cardinal(DataBuffer) + (i * BlockSize);
    FFreeBlocks.Add(pointer(b));
  end;
  //Create one block to start with.
  NewBlock;
end;

destructor TStorageFacility.Destroy;
begin
  VirtualFree(DataBuffer,VirtualSize,MEM_DECOMMIT);
  VirtualFree(DataBuffer,0,MEM_RELEASE);
  //The data pointed to by blocklist is already freed.
  BlockList.Next:= @BlockList;
  BlockList.Prev:= @BlockList;
  FFreeBlocks.Free;
  inherited;
end;

function TStorageFacility.NewBlock: PBlock;
var
  i, last: cardinal;
  adres: cardinal;
  ABrick: PBrick;
begin
  //Are there any blocks in the cache?
  if (BlockList.CacheCount > 0) then begin
    Result:= BlockList.CacheNext;
    BlockList.CacheNext:= Result^.CacheNext;
    Dec(BlockList.CacheCount)
  end
  //no, create a new block
  else begin
    //Get a adres from the list of free blocks.
    Last:= FFreeBlocks.count - 1;
    pointer(Adres):= FFreeBlocks[Last];
    Dec(THackList(FFreeBlocks).FCount);   //FFreeBlocks.Delete(Last);
    //get a ZERO-initialized block from our vitual storage.
    //Result:= PBlock(VirtualAlloc(pointer(Adres),BlockSize,MEM_COMMIT,PAGE_READWRITE));
    Result:= pointer(adres);
  end; {else}

  //(re)Initialize the block structure.
  //Use counter is already 0.
  //This speeds UP the action by improving the cache-characteristics of
  //the Block/brick structure.
  ABrick:= PBrick(Result);
  Adres:= integer(ABrick);
  for i:= 1 to BricksPerBlock - 1 do begin
    adres:= adres + BrickSize;
    ABrick^.NextBrick:= PBrick(adres);
    cardinal(ABrick):= adres;
  end;
  ABrick^.NextBrick:= nil;

  //Add the block at the end the list of used blocks.
  //FUsedBlocks.Add(Result);
  Result^.Next:= @BlockList;
  Result^.Prev:= BlockList.Prev;
  BlockList.Prev:= Result;
  Result^.Prev^.Next:= Result;
end;



function TStorageFacility.CreateBrick: TFastObject;
var
  ABlock: PBlock;
begin
  //Get a block from the used list.
  ABlock:= BlockList.Prev;
  //used list is empty, create a new block.
  if (ABlock = @BlockList) then ABlock:= NewBlock;

  Result:= TFastObject(ABlock^.NextBrick);
  //update the internal bookkeeping.
  ABlock^.NextBrick:= ABlock^.NextBrick^.NextBrick;
  Inc(ABlock^.FBricksInUse);

  //if Block is full then remove it from the usedlist.
  if ABlock^.NextBrick = nil then begin
    ABlock^.Prev^.Next:= ABlock^.Next;
    ABlock^.Next^.Prev:= ABlock^.Prev;
  end;
end;



procedure TStorageFacility.FreeBrick(ABrick: TFastObject);
var
  ABlock: PBlock;
const
  Empty = 0;
begin
  //How do we know which Block this object belongs to?
  //Easy, it points towards its storageblock.
  //each block starts on a 8k boundary.
  //Just strip off the bottom bits, and we've got the starting address.
  Cardinal(ABlock):= Cardinal(ABrick) and (-BlockSize);
  Dec(ABlock^.FBricksInUse);

  //Is the block empty?
  if (ABlock^.FBricksInUse = Empty) then begin
    //if so, remove the block from the Used list.
    ABlock^.Prev^.Next:= ABlock^.Next;
    ABlock^.Next^.Prev:= ABlock^.Prev;

    //Can we add it to the cache?
    if (BlockList.CacheCount < CacheSize) then begin
      ABlock^.CacheNext:= BlockList.CacheNext;
      BlockList.CacheNext:= ABlock;
      Inc(BlockList.CacheCount);
      //Add the brick to the list of free bricks.
      PBrick(ABrick)^.NextBrick:= ABlock^.NextBrick;
      ABlock^.NextBrick:= PBrick(ABrick);
    end
    else begin //no, free the block.
      //update bookkeeping of freeblocks.
      VirtualFree(ABlock,BlockSize,MEM_DECOMMIT);
      THackList(FFreeBlocks).FList^[THackList(FFreeBlocks).FCount]:= ABlock;
      Inc(THackList(FFreeBlocks).FCount); //FFreeBlocks.Add(ABlock);
    end;
  end {if}

  //Not empty, lets go on.
  else begin
    //WAS the block fully used?, if so it need to be added,
    //to the START of the used list.
    if ABlock^.NextBrick = nil then begin
      ABlock^.Prev:= @BlockList;
      ABlock^.Next:= BlockList.Next;
      BlockList.Next:= ABlock;
      ABlock^.Next^.Prev:= ABlock;
    end;
    //Add the brick to the list of free bricks.
    PBrick(ABrick)^.NextBrick:= ABlock^.NextBrick;
    ABlock^.NextBrick:= PBrick(ABrick);
  end;
end;

(********************** TStorageBlock *****************)


{ TFastTinyObject }

const
  FStorage: TStorageFacility = nil;

class procedure TFastObject.InitStorageFacility(AClass: TClass);
begin
  FStorage:= TStorageFacility.Create(AClass.InstanceSize, BricksPerBlock);
end;

class procedure TFastObject.FreeStorageFacility;
begin
  FStorage.Free;
  FStorage:= nil;
end;

class function TFastObject.NewInstance: TObject;
type
  PClass = ^TClass;
begin
  Result:= FStorage.CreateBrick;
  PClass(Result)^:= Self;  // Init VMT, don't init fields.
end;

class function TFastObject.Create: pointer;
begin
  Result:= NewInstance;
end;

procedure TFastObject.FreeInstance;
begin
  FStorage.FreeBrick(Self);
end;

initialization
  BlockSize:= GetMemPerBlock;
  TFastObject.InitStorageFacility(TLifeCel);
finalization
  TFastObject.FreeStorageFacility;
end.
