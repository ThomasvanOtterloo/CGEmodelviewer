unit Unit1;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, CastleShapes, CastleCameras, X3DNodes, X3DLoad,
  CastleTransform, CastleBoxes, CastleSceneCore, X3DLoadInternalUtils,
  Vcl.Controls, Vcl.Forms, Vcl.Dialogs, Vcl.CastleControl, CastleUIControls,
  Math, CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleUtils, CastleTriangles, CastleRectangles,
  CastleColors, CastleScene, Vcl.StdCtrls,
  Vcl.ExtCtrls, CastleViewport;

type
  TCastleApp = class(TCastleView)
  published // items added from editor

  private
    AngleX, AngleY: Single;
    TargetPosition: TVector3;
    RotationSpeedX, RotationSpeedY: Single;
  public
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Motion(const Event: TInputMotion): boolean; override;
  end;

  TForm1 = class(TForm)
    CastleControl: TCastleControl;
    SetErrorButton: TButton;
    ListBox1: TListBox;
    FailureDection: TTimer;
    Button4: TButton;
    StaticText1: TStaticText;
    OrbitModelTimer: TTimer;

    procedure FormCreate(Sender: TObject);
    procedure FailureDetectionTimer(Sender: TObject);
    procedure SetErrorButtonClick(Sender: TObject);
    procedure StartStopAnimation(Sender: TObject);
    procedure NewCamera(Error: string);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure ListBoxPlayAnimation(Sender: TObject);

  private
    { Private declarations }
    GLWin: TCastleControl;
    GLView: TCastleApp;

    function FindCastleControls(o: TObject): TArray<TCastleControl>;
    procedure InitVars;
    procedure InitEditorComponents;
    procedure BlinkRed(NodeName: string);
    procedure SetFailedObject(NodeName: string);
    procedure StartErrorDetectionNav(Sender: TObject);

  public
    { Public declarations }
    procedure ExitFailDetectionWindow(Sender: TObject);
    procedure FullscreenFailDetectionWindow(Sender: TObject);
  end;

var
  Form1: TForm1;
  IsCastleControlOnForm: boolean;
  MainCamera: TCastleCamera;
  ModelScene: TCastleScene;
  Viewport1: TCastleViewport;
  Viewport2: TCastleViewport;
  MakeBiggerButton: TCastleButton;
  ExitButton: TCastleButton;
  NavigateButton: TCastleButton;
  Items: TCastleRootTransform;
  ErrorCamera: TCastleCamera;
  MainNavIsActive: boolean;
  LabelFPS: TCastleLabel;

  OrbitRadius, OrbitSpeed, Angle: Double;

  ErrorRootNode: TTransformNode; // TTransformNode RootNode;
  ErrorgroupNode: TGroupNode; // FIrst child
  ErrorShapeNode: TShapeNode; // Second child

  // Mat2OriginalColor: TVector3;

  failureDetected: boolean;
  Mat: TPhysicalMaterialNode;
  ErrorMaterialNode: TPhysicalMaterialNode;
  ErrorNodeOriginalColor: TVector3;
  StatusLightAppNode: TAppearanceNode;
  ErrorAppearanceNode: TAppearanceNode;
  Hdmi: TCastleTransform;
  LightIsRed: boolean;


  LookTarget, LookDir: TVector3;  //Motion vars for navigation
  DesiredUp: TVector3 = (Data: (0, 1, 0));

  TotalTimePassed: single = 0;

implementation

{$R *.dfm}

uses System.Generics.Collections;

function TCastleApp.Motion(const Event: TInputMotion): boolean;
var
  AngleRotate: Single;
begin
  Result := inherited;
  if Result then
    Exit;
  if (buttonLeft in Event.Pressed) and (MainNavIsActive) then
  begin
    AngleRotate := -0.01 * (Event.Position.X - Event.OldPosition.X);
    LookTarget := Viewport1.Items.BoundingBox.Center;
    LookDir := LookTarget - Viewport1.Camera.Translation;
    LookDir := RotatePointAroundAxis(Vector4(DesiredUp, AngleRotate), LookDir);

    Viewport1.Camera.SetView(LookTarget - LookDir,
      // new camera Translation (aka position)
      LookDir, // new camera Direction (the length of LookDir given here is ignored, ok)
      DesiredUp);
  end;
end;

procedure TCastleApp.Update(const SecondsPassed: Single; var HandleInput: boolean);
var
  CPos, ModelCenter, LookAtDirection: TVector3;
  DistanceToModel, OrbitSpeed, AngleU: Single;
begin
  inherited;

  ModelCenter := Viewport1.Items.BoundingBox.Center; // Center of the model
  DistanceToModel := 650; // Orbit radius
  OrbitSpeed := 10; // Degrees per second

  // Update total time passed
  TotalTimePassed := TotalTimePassed + SecondsPassed;

  // Convert orbit speed to radians and calculate angle
  AngleU := (OrbitSpeed * TotalTimePassed) * (Pi / 180); // Convert to radians

  // Calculate new camera position for circular orbit
  CPos.X := ModelCenter.X + DistanceToModel * Cos(AngleU);
  CPos.Z := ModelCenter.Z + DistanceToModel * Sin(AngleU);
  CPos.Y := ModelCenter.Y + 20; // Vertical position

  // Adjust camera look at direction
  LookAtDirection := ModelCenter - CPos;

  // Set the new camera view
  Viewport1.Camera.SetView(CPos, LookAtDirection, DesiredUp);
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

  LabelFPS := CastleControl.Container.DesignedComponent('LabelFPS')
    as TCastleLabel;
  NavigateButton := CastleControl.Container.DesignedComponent('NavigateButton')
    as TCastleButton;
  NavigateButton.OnClick := StartErrorDetectionNav;
  Viewport2.Navigation.Exists := false;
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
  FailureDection.Enabled := false;
  MainNavIsActive := true;
  InitEditorComponents;

  // puts all animations in a list
  for I := 0 to ModelScene.AnimationsList.Count - 1 do
    ListBox1.AddItem(ModelScene.AnimationsList[I], nil);

  // failure should be false on default.
  failureDetected := false;

  // sets extra ErrorCamera in the world.
  Viewport2.Items := Viewport1.Items;
  Viewport2.Camera := ErrorCamera;
  Viewport2.Exists := True;

  // search for statuslight object

  try
    StatusLightAppNode := ModelScene.Node('M_StatusLight') as TAppearanceNode;
    Mat := StatusLightAppNode.Material as TPhysicalMaterialNode;

  except
    showmessage('StatusLight not found.');
  end;

  LightIsRed := false;
end;

procedure TForm1.StartErrorDetectionNav(Sender: TObject);
begin

  if MainNavIsActive then
  begin
    MainNavIsActive := false;
    Viewport2.Navigation.Exists := True;
  end
  else
  begin
    MainNavIsActive := True;
    Viewport2.Navigation.Exists := false;
  end;

end;

procedure TForm1.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin

  if Key = VK_ESCAPE then
  begin
    ModelScene.Exists := not ModelScene.Exists;
  end;
end;

procedure TForm1.FailureDetectionTimer(Sender: TObject);
begin
  if failureDetected then
  begin
    if LightIsRed then
    begin
      Mat.BaseColor := Vector3(0.9, 0.9, 0.9); // almost white
      ErrorMaterialNode.BaseColor := ErrorNodeOriginalColor;
      LightIsRed := false
    end
    else
    begin
      Mat.BaseColor := Vector3(0.9, 0.1, 0.1); // red-ish
      ErrorMaterialNode.BaseColor := Vector3(0.9, 0.1, 0.1);
      LightIsRed := True;
    end; // above here works fine :0
  end
end;

procedure TForm1.SetFailedObject(NodeName: string);
var
  EncodedString: string;
begin
  // see X3D file

  ErrorRootNode := ModelScene.RootNode.FindNode(NodeName) as TTransformNode;
  // Root

  ErrorRootNode := ModelScene.RootNode.FindNode(NodeName) as TTransformNode;
  // Root
  ErrorgroupNode := ErrorRootNode.FdChildren[0] as TGroupNode; // first child
  ErrorShapeNode := ErrorgroupNode.FdChildren[0] as TShapeNode; // Second child
  ErrorAppearanceNode := ErrorShapeNode.Appearance as TAppearanceNode;
  // Third child
  ErrorMaterialNode := ErrorAppearanceNode.Material as TPhysicalMaterialNode;
  ErrorNodeOriginalColor := ErrorMaterialNode.BaseColor;

end;

procedure TForm1.NewCamera(Error: string);
var

  CameraPosition, CameraDirection: TVector3;
  Box: TBox3D;
  APos, ADir, AUp: TVector3;
  IntersectionDistance: Single;
  Shape: TShapeNode;
  Group: TGroupNode;
  trash: TVector3;
begin
  Viewport2.FullSize := false;

  if Assigned(ErrorRootNode) then
  begin
    if not Viewport2.Exists then
      Viewport2.Exists := True;

    Viewport1.Camera.GetWorldView(trash, ADir, AUp);
    Box := Box3DAroundPoint(Box.Center, Box.Size.Max);

    // if not Box.TryRayClosestIntersection(IntersectionDistance, Box.Center, -ADir) then
    // begin
    // { TryRayClosestIntersection may return false for box with size zero
    // (though not observed in practice),
    // only then ray from Box.Center may not hit one of box walls. }
    // IntersectionDistance := 1;
    // WritelnWarning('Ray from box center didn''t hit any of box walls');
    // end;
    APos := Box.Center - ADir * IntersectionDistance * 2;

    //
    // var textPosCenter := Shape.Scene.Center+ Vector3(0,0,8);
    // var testPos := FailingPart.Translation + Vector3(0,0,10);
    var
    textDir := Vector3(0, 0, -1);
    var
    testUp := Vector3(0, 1, 0);

    Viewport2.Camera.AnimateTo(Vector3(0, 1, 7), textDir, testUp, 1.5);
  end
  else
  begin
    if Viewport2.Exists then
      Viewport2.Exists := false;
  end;
end;

procedure TForm1.SetErrorButtonClick(Sender: TObject);
var
  Error, PartAfter, SubString: string;
  Position: Integer;
begin
  Error := 'Solid4'; // errordata containts location of error

  try
    if failureDetected then
    begin // back to default
      failureDetected := false;
      FailureDection.Enabled := false;
      // reset colors to default
      // Mat2.BaseColor := Mat2OriginalColor;
      Mat.BaseColor := Vector3(0, 1, 0);
      ErrorMaterialNode.BaseColor := ErrorNodeOriginalColor;
      Viewport2.Exists := false;
    end
    else
    begin // start error display
      SetFailedObject(Error);
      failureDetected := True; // This enables the animation in TTimer.
      FailureDection.Enabled := True;
      NewCamera(Error);
    end;

  except
    showmessage('Error Naming location not found');

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
  AnimationActive: boolean;
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
