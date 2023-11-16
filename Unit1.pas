unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, CastleShapes, CastleCameras, X3DNodes, X3DLoad,
  CastleTransform, CastleBoxes, CastleSceneCore, X3DLoadInternalUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.CastleControl, CastleUIControls,
  Math, CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleColors, CastleScene, Vcl.StdCtrls,
  Vcl.ExtCtrls, CastleViewport;

type
  TCastleApp = class(TCastleView)

    procedure Update(const SecondsPassed: Single; var HandleInput: Boolean);
  private
  end;

  TForm1 = class(TForm)
    CastleControl: TCastleControl;
    SetErrorButton: TButton;
    ListBox1: TListBox;
    FailureDection: TTimer;
    Button4: TButton;
    StaticText1: TStaticText;
    StaticText3: TStaticText;

    procedure FormCreate(Sender: TObject);
    procedure FailureDetectionTimer(Sender: TObject);
    procedure SetErrorButtonClick(Sender: TObject);
    procedure StartStopAnimation(Sender: TObject);
    procedure Button1Click(Sender: TObject);
    procedure NewCamera(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxPlayAnimation(Sender: TObject);
  private
    { Private declarations }
    GLWin: TCastleControl;
    GLView: TCastleApp;

    function FindCastleControls(o: TObject): TArray<TCastleControl>;
    procedure SetupCamera;
    procedure InitVars;
    procedure InitEditorComponents;
    procedure BlinkRed(NodeName: string);
    procedure SetFailedObject(NodeName: string);

  public
    { Public declarations }
    procedure ExitFailDetectionWindow(Sender: TObject);
    procedure FullscreenFailDetectionWindow(Sender: TObject);
  end;

var
  Form1: TForm1;
  IsCastleControlOnForm: Boolean;
  MainCamera: TCastleCamera;
  ModelScene: TCastleScene;
  Viewport1: TCastleViewport;
  Viewport2: TCastleViewport;
  MakeBiggerButton: TCastleButton;
  ExitButton: TCastleButton;
  Items: TCastleRootTransform;
  ErrorCamera: TCastleCamera;

  OrbitRadius, OrbitSpeed, Angle: Double;
  failureDetected: Boolean;
  Mat: TPhysicalMaterialNode;
  Mat2: TPhysicalMaterialNode;
  AppearanceAsAbstractNode: TAppearanceNode;
  PowerSwitchnode: TAppearanceNode;
  Hdmi: TCastleTransform;
  IsRed: Boolean;

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
  NewCamera: TCastleCamera;
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

  InitVars;
  SetupCamera; // todo;

end;

procedure TForm1.InitEditorComponents;
begin
  CastleControl.Container.DesignUrl :=
    'castle-data:/test_3d.castle-user-interface';

  ModelScene := CastleControl.Container.DesignedComponent('ModelScene')
    as TCastleScene;
  MainCamera := CastleControl.Container.DesignedComponent('MainCamera')
    as TCastleCamera;
  Viewport1 := CastleControl.Container.DesignedComponent('Viewport1')
    as TCastleViewport;
  Viewport2 := CastleControl.Container.DesignedComponent('Viewport2')
    as TCastleViewport;
  ErrorCamera := CastleControl.Container.DesignedComponent('ErrorCamera')
    as TCastleCamera;
  MakeBiggerButton := CastleControl.Container.DesignedComponent('MakeBigger')
    as TCastleButton;
  MakeBiggerButton.OnClick := FullscreenFailDetectionWindow;

  ExitButton := CastleControl.Container.DesignedComponent('ExitButton')
    as TCastleButton;
  ExitButton.OnClick := ExitFailDetectionWindow;

end;

procedure TForm1.ExitFailDetectionWindow(Sender: TObject);
begin
  Viewport2.Exists := false;
end;

procedure TForm1.FullscreenFailDetectionWindow(Sender: TObject);
begin
  Viewport2.FullSize := True;
end;

procedure TForm1.InitVars;
var
  I: Integer;

begin
  InitEditorComponents;

  // puts all animations in a list
  for I := 0 to ModelScene.AnimationsList.Count - 1 do
    ListBox1.AddItem(ModelScene.AnimationsList[I], nil);

  // failure should be false on default.
  failureDetected := false;

  // sets extra ErrorCamera in the world.
  Viewport2.Items := Viewport1.Items;
  Viewport2.Camera := ErrorCamera;
  Viewport2.Exists := False;

  // search for statuslight object
  AppearanceAsAbstractNode := ModelScene.Node('StatusLight') as TAppearanceNode;
  Mat := AppearanceAsAbstractNode.Material as TPhysicalMaterialNode;
  IsRed := false;
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
  // while loop create blinking light animiation.

  if failureDetected then
  begin
    if IsRed then
    begin
      Mat.BaseColor := Vector3(0.9, 0.9, 0.9); // almost white
      Mat2.BaseColor := Vector3(0.9, 0.9, 0.9);
      IsRed := false
    end
    else
    begin
      Mat.BaseColor := Vector3(0.9, 0.1, 0.1); // reddish
      Mat2.BaseColor := Vector3(0.9, 0.1, 0.1);
      IsRed := True;
    end;
  end
  else
    Mat.BaseColor := Vector3(0, 1, 0); // green
end;

procedure TForm1.Button1Click(Sender: TObject);
begin
  // todo; randomize color of selected node
   // turns the powerswitch button RED.
   PowerSwitchnode := ModelScene.Node(DecodeX3DName('MPowerCord'))
   as TAppearanceNode; // works
   Mat2 := PowerSwitchnode.Material as TPhysicalMaterialNode;
   Mat2.BaseColor := Vector3(0.9, 0.1, 0.1); // reddish
end;

procedure TForm1.SetFailedObject(NodeName: string);
begin
  var
  ObjectFailing := ModelScene.Node(NodeName) as TAppearanceNode;
  Mat2 := ObjectFailing.Material as TPhysicalMaterialNode;

end;

procedure TForm1.NewCamera(Sender: TObject);
var
  FailingPart: TTransformNode;
  CameraPosition, CameraDirection: TVector3;
  Box: TBox3D;
  APos, ADir, AUp: TVector3;
  IntersectionDistance: Single;
begin
  Viewport2.FullSize := false;
  // Now find the TTransformNode node by name
  FailingPart := ModelScene.RootNode.FindNode('PowerCord') as TTransformNode;

  if Assigned(FailingPart) then
  begin
    if not Viewport2.Exists then
      Viewport2.Exists := True;

    CameraPosition := FailingPart.Translation + Vector3(0, 0, 8);
    CameraDirection := FailingPart.Translation - CameraPosition;
    Viewport2.Camera.AnimateTo(CameraPosition, CameraDirection,
      Vector3(0, 2, 0), 1.5);

  end
  else
  begin
    if Viewport2.Exists then
      Viewport2.Exists := false;
  end;
end;

procedure TForm1.SetErrorButtonClick(Sender: TObject);
begin
  // todo; Start selected animation.


  if failureDetected then
    failureDetected := false
  else
  begin
    failureDetected := True;
    SetFailedObject('MPowerCord');
    NewCamera(Sender);
  end;

end;

procedure TForm1.BlinkRed(NodeName: string);
begin
  // todo; Start selected animation.
  var
  a := ModelScene.Node(NodeName) as TAppearanceNode;
  // turn red/white animation. But Figure out what the companys library is called

end;

procedure TForm1.StartStopAnimation(Sender: TObject);
var
  NewViewport: TCastleViewport;
  SceneToFocus: TCastleScene;
  AnimationActive: Boolean;
begin
  ModelScene.StopAnimation(false);
  // todo; figure out how to call the last animation and stop/start it again on command.

end;


procedure TForm1.ListBoxPlayAnimation(Sender: TObject);
var
  ListBox: TListBox;
  ItemIndex: Integer;
  ItemValue: string;
begin
  ListBox := Sender as TListBox;
  ItemIndex := ListBox.ItemIndex; // Gets the index of the selected item
  if ItemIndex <> -1 then // -1 means no item is selected
  begin
    ItemValue := ListBox.Items[ItemIndex];
    // Gets the value of the selected item
    ModelScene.PlayAnimation(ItemValue, True);
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

end.
