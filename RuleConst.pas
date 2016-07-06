unit RuleConst;

interface

uses
  Classes, Registry, Menus, IniFiles,
  LifeUtil, LifeConst;

type
  TRuleList = class(TList)
    constructor Create;
    destructor Destroy; override;
    procedure SaveToRegistry;
    procedure SaveToMenu(AMenu: TPopupMenu; AnEvent: TNotifyEvent);
  end;

  TRuleItem = class(TObject)
  private
    FRule: string;
    FName: string;
    FShortInfo: string;
    FLongInfo: string;
  public
    constructor Create(ARule,AName,AShortInfo,ALongInfo: string);
    property Rule: string read FRule write FRule;
    property Name: string read FName write FName;
    property ShortInfo: string read FShortInfo write FShortInfo;
    property LongInfo: string read FLongInfo write FLongInfo;
  end;

const
  idRule = 0;
  idShortInfo = 1;
  idName = 2;
  idLongInfo =3;

  StandardRuleList: array[0..16,0..3] of string =
    (('M:/2','(exploding) phoenix, minimal','','(Exploding) Every living cell dies every generation, but most pa' +
              'tterns explode anyway. It'#39's a challenge to build new patterns th' +
              'at don'#39't explode. Arguably the simplest challenging rule.'),
     ('M:/234','(exploding) phoenix, lacey patterns','','(Exploding) Like /2, every living cell dies every generation. Th' +
              'is rule is picked for the exceptional fabric-like beauty of the ' +
              'patterns that it produces.'),
     ('M:12345/3','(exploding) maze-like designs','','(Exploding) An "a-maze-ing" universe - crystalizes into maze-lik' +
              'e patterns. Interesting variations: try removing 5 from the "No ' +
              'change" list. To produce mice running in the maze, add 7 to the ' +
              '"Births" list.'),
     ('M:125/36','(chaotic) Life-like 2x2 block rule','','(Chaotic) Similar in character to Conway'#39's Life, but completely ' +
              'different patterns. Many different oscillators occur at random, ' +
              'and a rare glider. This rule is also a 2x2 block universe. This ' +
              'means that patterns consisting entirely of 2x2 blocks, all align' +
              'ed, will continue to consist of 2x2 blocks.'),
     ('M:1357/1357','(exploding) everything is a replicator','','(Exploding) In this remarkable universe every pattern is a repli' +
              'cator. I particularly like drawing my kid brother in gen=0 and t' +
              'hen skipping +1024 gens.'),
     ('M:1358/357','(chaotic) a balanced amoeba rule','','(Chaotic) An "amoeba" universe - forms large random areas that r' +
              'esemble amoebas. Internal to a random area is chaos. The edge va' +
              'scillates wildly, and patterns tend to grow more than shrink. Th' +
              'e more they grow, the more certain their survival. This is a fai' +
              'rly well-balanced rule.'),
     ('M:23/3','(chaotic) "Conway'#39's Life" (default)','Conway'#39's Life (default)',
              '(Chaotic) The default rule. This is the most famous cellular aut' +
              'omata ever invented. People have been discovering patterns for t' +
              'his rule since around 1970. Large collections are available on t' +
              'he Internet.'),
     ('M:23/36','(chaotic) "HighLife" (has replicator)','HighLife',
              '(Chaotic) This rule is very similar to Conway'#39's Life, but is has' +
              ' a surprise replicator pattern. There is no known replicator in ' +
              'Conway'#39's Life.'),
     ('M:235678/3678','(stable) ink blot, quick drying','','(Stable) Most close variations of these rules expand forever, bu' +
              't this one curiously does not. Why?'),
     ('M:235678/378','(exploding) coagulations in chaos','','(Exploding) Creates gooey coagulations as it expands forever. Be' +
              'st viewed at zoom=1. Notice that this is a close variation of th' +
              'e previous rule, 235678/3678, except that there is one less cond' +
              'ition for a dead cell to come to life on the next generation. In' +
              ' general this should make a universe less active, but this is an' +
              ' exception.'),
     ('M:238/357','(chaotic) broken Life','','(Chaotic) In this close variation of Conway'#39's Life, the chaos is' +
              ' remarkably similar, but almost none of the engineered patterns ' +
              'work.'),
     ('M:245/368','(stable) death plus puffers and ships','','(Stable) A very calm universe, which nonetheless has a very comm' +
              'only occuring slow spaceship and a slow puffer.'),
     ('M:34/34','(exploding) "34 Life"','34 Life',
              '(Exploding) One of the first explored alternatives to Conway'#39's L' +
              'ife, back in the early 1970'#39's. Computing power was so low back t' +
              'hen, it was months before anyone noticed that this is an explodi' +
              'ng universe. What makes this universe interesting is the variety' +
              ' of small oscillators and the period-3 orthogonal spaceship.'),
     ('M:34678/3678','(stable) "Day & Night"','Day & Night',
              '(Stable) So named because dead cells in fields of live cells act' +
              ' by the same rules as live cells in fields of dead cells. There ' +
              'are obviously other rules which have this symmetrical property, ' +
              'but this rule was chosen because it has some interesting high pe' +
              'riod spaceships and oscillators.'),
     ('M:45678/3','(exploding) slow coral growth','','(Exploding) This rule produces patterns with a surprisingly slow' +
              ' rate of expansion and an interesting coral-like texture.'),
     ('M:5/345','(stable) "Long life"','Long life',
              '(Stable) This rule is called "Long life" because of the extremel' +
              'y high period patterns that can be produced in this universe.'),
     ('M:5678/35678','(chaotic) diamonds & catastrophes','Diamoeba',
              '(Chaotic) Creates solid diamond-shaped "amoeba" patterns that ar' +
              'e surprisingly unpredictable. It is not known whether any diamon' +
              'ds expand forever, or if the tendency toward the catastrophic de' +
              'struction of corners is too strong. '));

implementation

uses
  Windows, SysUtils;

constructor TRuleList.Create;
const
  CantCreate = false;
var
  //MyReg: TRegistry;
  MyReg: TCustomInifile;
  KeyRules: TStringList;

  procedure GetDefaultRules;
  var
    i: integer;
    NewItem: TRuleItem;
  begin
    for i:= Low(StandardRuleList) to High(StandardRuleList) do begin
      NewItem:= TRuleItem.Create(StandardRuleList[i,idRule],
                                 StandardRuleList[i,idName],
                                 StandardRuleList[i,idShortInfo],
                                 StandardRuleList[i,idLongInfo]);
      Add(NewItem);
    end; {for i}
  end;

var
  i: integer;
  NewItem: TRuleItem;
  AName, AShortInfo, ALongInfo: string;
begin
  inherited Create;
  //MyReg:= TRegistry.Create;
  //MyReg:= TIniFile.Create('Life32Rules.ini');
  MyReg:= TRegistryIniFile.Create('Software\'+'Life32Rules');
  //MyReg.Rootkey:= HKEY_CURRENT_USER;
    //if not(MyReg.OpenKey('SOFTWARE',DontCreate)) then
    //raise Exception.Create('Registry may be damaged');
    //if not(MyReg.OpenKey('JBontes\Life32',MakeIt)) then
    //raise Exception.Create('Can''t save settings to registry');
  try
    //if not(MyReg.OpenKey('Rules',CantCreate)) then GetDefaultRules
    {else} begin
      KeyRules:= TStringList.Create;
      //MyReg.GetKeyNames(KeyRules);
      MyReg.ReadSections(KeyRules);
      if KeyRules.Count = 0 then GetDefaultRules
      else with KeyRules do begin
        i:= 0;
        while i < Count do begin
          //OldKey:= MyReg.CurrentPath;
          //MyReg.OpenKey(KeyRules[i],CantCreate);
          AName:= MyReg.ReadString(KeyRules[i],'Name','');
          AShortInfo:= MyReg.ReadString(KeyRules[i],'ShortInfo','');
          ALongInfo:= MyReg.ReadString(KeyRules[i],'LongInfo','');
          //MyReg.CloseKey;
          //MyReg.OpenKey(OldKey,CantCreate);
          NewItem:= TRuleItem.Create(KeyRules[i],AName,AShortInfo,ALongInfo);
          Self.Add(NewItem);
          Inc(i);
        end; {while}
      end; {with}
    end; {else}
    finally
  end;
end;

destructor TRuleList.Destroy;
begin
  while (Count > 0) do begin
    TRuleItem(Items[0]).Free;
    Delete(0);
  end; {while}
  inherited Destroy;
end;

procedure TRuleList.SaveToRegistry;
const
  MakeIt = true;
  CantCreate = false;
var
  i: integer;
  Oldstrings: TStringList;
  Section: string;
  //Rulename: string;
begin
  try
  //with TRegistry.Create do try
  //with TIniFile.Create('Life32Rules.ini') do try
  with TRegistryIniFile.Create('Software\'+'Life32Rules') do try
    //Rootkey:= HKEY_CURRENT_USER;
    //if not(OpenKey('SOFTWARE',DontCreate)) then
    //raise Exception.Create('Registry may be damaged');
    //if not(OpenKey('JBontes\Life32',MakeIt)) then
    //raise Exception.Create('Can''t save settings to registry');
    //OpenKey('Rules',MakeIt);
    //delete any non-prefixed rules if they exist.
    OldStrings:= TStringList.Create;
    ReadSections(OldStrings);
    i:= 0;
    while i < OldStrings.Count do begin
      try
        if Pos(':',OldStrings[i])=0 then begin
          EraseSection(Oldstrings[i]);
        end;
      except {ignore}
      end; {try}
      Inc(i);
    end; {while}

    //Now write the current keys to the reg, except keys without a prefix.
    i:= 0;
    while i < count do begin
      //OldKey:= CurrentPath;
      //Rulename:= Items[i];
      if pos(':',TRuleItem(Items[i]).Rule) <> 0 then begin
        //OpenKey(TRuleItem(Items[i]).Rule,MakeIt);
        Section:= TRuleItem(Items[i]).Rule;
        WriteString(Section,'Name',TRuleItem(Items[i]).Name);
        WriteString(Section,'ShortInfo',TRuleItem(Items[i]).ShortInfo);
        WriteString(Section,'LongInfo',TRuleItem(Items[i]).LongInfo);
        //go back to the old path.
        //CloseKey;
      end; {if}
      //OpenKey(OldKey,CantCreate);
      Inc(i);
    end; {while}
    finally Free;
  end; {with}
  except
    {ignore}
  end;
end;

procedure TRuleList.SaveToMenu(AMenu: TPopupMenu; AnEvent: TNotifyEvent);
var
  i: integer;
  AnItem: TMenuItem;
begin
  i:= FixedItemsInRuleMenu;
  while (i+1) < AMenu.Items.Count do begin
    AMenu.Items.Remove(AMenu.Items[i]);
  end;
  i:= 0;
  while i < Count do begin
    AnItem:= TMenuItem.Create(AMenu);
    AnItem.Caption:= TRuleItem(Items[i]).Rule + #9 +
                     TRuleItem(Items[i]).ShortInfo;
    AnItem.OnClick:= AnEvent;
    AMenu.Items.Add(AnItem);
    Inc(i);
  end;
end;

constructor TRuleItem.Create(ARule,AName,AShortInfo,ALongInfo: string);
var
  APrefix: string;
begin
  inherited Create;
  APrefix:= GetPrefix(ARule);
  if ((APrefix = '') or (APrefix = ':')) then ARule:= InsertPrefix(ARule,NeighBorhoodToStr(nbDefault));
  FRule:= ARule;
  FName:= AName;
  FShortInfo:= AShortInfo;
  FLongInfo:= ALongInfo;
end;

end.
