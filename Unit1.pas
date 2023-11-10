unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, CastleShapes, CastleCameras, X3DNodes,X3DLoad,
  CastleTransform, CastleBoxes,CastleSceneCore,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.CastleControl, CastleUIControls,
  Math, CastleComponentSerialize, CastleKeysMouse, CastleLog,
  CastleVectors, CastleGLUtils, CastleColors, CastleScene, Vcl.StdCtrls,
  Vcl.ExtCtrls, CastleViewport;

type
  TCastleApp = class(TCastleView)

    procedure Render; override; // TCastleUserInterface
    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean);
  private
  end;

  TForm1 = class(TForm)
    CastleControl: TCastleControl;
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    ListBox1: TListBox;
    FailureDection: TTimer;
    Button4: TButton;
    StaticText1: TStaticText;
    StaticText2: TStaticText;
    StaticText3: TStaticText;

    procedure FormCreate(Sender: TObject);
    procedure FailureDetectionTimer(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure StartStopAnimation(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxPlayAnimation(Sender: TObject);
  private
    { Private declarations }
    GLWin: TCastleControl;
    GLView: TCastleApp;
    Viewport1: TCastleViewport;
    function FindCastleControls(o: TObject): TArray<TCastleControl>;
    procedure SetupCamera;
    function GetSceneToFocusOn: TCastleScene;
    procedure InitVars;
    procedure PlayStatusLight;
  public
    { Public declarations }
  end;

var
  Form1: TForm1;
  IsCastleControlOnForm: Boolean;
  MainCamera: TCastleCamera;
  ModelScene: TCastleScene;
  OrbitRadius, OrbitSpeed, Angle: Double;
  failureDetected: boolean;
  Mat: TPhysicalMaterialNode;
  AppearanceAsAbstractNode: TAppearanceNode;
  IsRed: boolean;


implementation

{$R *.dfm}

uses System.Generics.Collections;

procedure TCastleApp.Update(const SecondsPassed: Single;
  var HandleInput: Boolean);
begin
  inherited Update(SecondsPassed, HandleInput);

  if Container.Pressed[Key7] then
    ModelScene.Exists := false;

end;

procedure TForm1.FormCreate(Sender: TObject);
var
  c: TArray<TCastleControl>;
  Scene: TCastleScene;
  Light: TAbstractLightNode;
begin
  IsCastleControlOnForm := True;
  c := FindCastleControls(Self);
  if Length(c) = 0 then
  begin
    IsCastleControlOnForm := false;
    GLWin := TCastleControl.Create(Self)
  end
  else

    GLWin := c[0];
  GLWin.Parent := Form1;
  GLWin.Align := alClient;
  GLView := TCastleApp.Create(GLWin);
  GLWin.Container.View := GLView;

  CastleControl.Container.DesignUrl :=
    'castle-data:/test_3d.castle-user-interface';
  ModelScene := CastleControl.Container.DesignedComponent('ModelScene')
    as TCastleScene;
  MainCamera := CastleControl.Container.DesignedComponent('MainCamera')
    as TCastleCamera;

  InitVars;
  SetupCamera;




end;

procedure TForm1.InitVars;
var
I: Integer;
begin
     // puts all animations in a list but gives error
 for I := 0 to ModelScene.AnimationsList.Count - 1 do
  ListBox1.AddItem(ModelScene.AnimationsList[I], nil);


  failureDetected:= false;

  // search for statuslight object
     AppearanceAsAbstractNode := ModelScene.Node('StatusLight') as TAppearanceNode;
     Mat := AppearanceAsAbstractNode.Material as TPhysicalMaterialNode;
     IsRed:= false;
end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

    if Key = VK_ESCAPE then
  begin
    ModelScene.Exists := not ModelScene.Exists;
  end;
end;

procedure TForm1.SetupCamera;
var
    Nav: TCastleNavigation;
begin
// TODO: Create smooth camera navtigation.
// And position camera based on size of model.


end;

procedure TForm1.FailureDetectionTimer(Sender: TObject);

begin
  // todo: Fix the camera Rotation. Overwrite the default camera navigation first.
    //while loop create blinking light animiation.

     if failureDetected then
      begin
           if IsRed then
           begin
             Mat.BaseColor := Vector3(0.9, 0.9, 0.9); // almost white
             IsRed:= false
           end
           else
           begin
              Mat.BaseColor := Vector3(0.9, 0.1, 0.1); // reddish
              IsRed:= true;
           end;
      end
      else
       Mat.BaseColor := Vector3(0, 1, 0); // green

end;

procedure TCastleApp.Render;
var
  Points: array [0 .. 3] of TVector2;
begin
  inherited;
  Points[0] := Vector2(0, Container.UnscaledHeight / 2);
  Points[1] := Vector2(Container.UnscaledWidth, Container.UnscaledHeight / 2);
  Points[2] := Vector2(Container.UnscaledWidth / 2, 0);
  Points[3] := Vector2(Container.UnscaledWidth / 2, Container.UnscaledHeight);

  if IsCastleControlOnForm then
    DrawPrimitive2D(pmLines, Points, Purple)
  else
    DrawPrimitive2D(pmLines, Points, Red);
end;



procedure TForm1.Button1Click(Sender: TObject);

begin
    // todo; randomize color of selected node




end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  // todo: change position of selected node:
end;

procedure TForm1.Button3Click(Sender: TObject);
var
  Box: TBox3D;
  AInitialPosition, AInitialDirection, AInitialUp, AGravityUp: TVector3;
begin
    // todo; Start selected animation.
      if failureDetected then
      failureDetected := false
      else
      failureDetected := true;
end;

// Creates a new tiny window with a new camera
procedure TForm1.StartStopAnimation(Sender: TObject);
var
  NewViewport: TCastleViewport;
  SceneToFocus: TCastleScene;
  AnimationActive: boolean;
begin
   ModelScene.StopAnimation(false);


  // todo; figure out how to call the last animation and stop/start it again on command.



end;

function TForm1.GetSceneToFocusOn: TCastleScene;
begin
  // todo; Return node/mesh where the error occurs. So the camera can zoom into its box.

    Result := ModelScene;
end;

procedure TForm1.ListBoxPlayAnimation(Sender: TObject);
var
  ListBox: TListBox;
  ItemIndex: Integer;
  ItemValue: string;
begin
  ListBox := Sender as TListBox;
  ItemIndex := ListBox.ItemIndex;  // Gets the index of the selected item
  if ItemIndex <> -1 then  // -1 means no item is selected
  begin
    ItemValue := ListBox.Items[ItemIndex];  // Gets the value of the selected item
    ModelScene.PlayAnimation(ItemValue,true);
  end;
end;

function TForm1.FindCastleControls(o: TObject): TArray<TCastleControl>;
var
  I: Integer;
begin
  Result := TArray<TCastleControl>.Create();
  for I := 0 to ComponentCount - 1 do
  begin
    if Components[I] is TCastleControl then
    begin
      SetLength(Result, Length(Result) + 1);
      Result[Length(Result) - 1] := TCastleControl(Components[I]);
    end;
  end;
end;



procedure TForm1.PlayStatusLight;
begin


  







end;


end.
