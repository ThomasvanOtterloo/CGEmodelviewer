unit MainForm;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls,

  CastleShapes, CastleCameras, X3DNodes, X3DLoad, CastleTransform,
  CastleBoxes, CastleSceneCore, X3DLoadInternalUtils, CastleUIControls,
  CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleUtils, CastleTriangles,
  CastleRectangles, CastleColors, CastleScene, CastleViewport,
  Vcl.CastleControl,

  Math, ErrorManager,ModelProcessing, ShapeNodeColor;

type
  TCastleApp = class(TCastleView)
  published // items added from editor

  private const
    OrbitMultiplier = 1.75;
    VerticalCameraOffset = 20;
    RadiansPerDegree = Pi / 180;

  public
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure UpdateMainCameraPosition(const ModelCenter: TVector3;
      const DistanceToModel, AngleU: Single);
  end;

  TForm1 = class(TForm)
    CastleControl: TCastleControl;
    SetErrorButton: TButton;
    ListBox1: TListBox;
    FailureDection: TTimer;
    Button4: TButton;
    StaticText3: TStaticText;
    procedure FormCreate(Sender: TObject);
    procedure FailureDetectionTimer(Sender: TObject);
    procedure SetErrorButtonClick(Sender: TObject);
    procedure StartStopAnimation(Sender: TObject);
    procedure NewCamera(Error: string);
    procedure ListBoxPlayAnimation(Sender: TObject);

  private
    { Private declarations }
    GLWin: TCastleControl;
    GLView: TCastleApp;

      ErrorManager: TErrorManager;
      ModelProcessing: TModelProcessing;

    procedure ExitFailDetectionViewport(Sender: TObject);
    procedure FullscreenFailDetectionWindow(Sender: TObject);
    function FindCastleControls(o: TObject): TArray<TCastleControl>;
    procedure InitVars;
    procedure InitEditorComponents;
    procedure FindFailedNode(NodeName: string);
    procedure StartErrorDetectionNav(Sender: TObject);
    procedure InitializeGLWin;
    procedure ForwardEditorComponentsToMethods;
    procedure SetFailedDetection;

  public
    { Public declarations }


  end;

const
  DefaultOrbitSpeed = 10; // Degrees per second
  VerticalPositionOffset = 20;
  AlmostWhiteColor: TVector3 = (Data: (0.9, 0.9, 0.9));
  RedishColor: TVector3 = (Data: (0.9, 0.1, 0.1));

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

  FailureDetected: boolean;
  MatStatusLight: TPhysicalMaterialNode;
  StatusLightAppNode: TAppearanceNode;
  Hdmi: TCastleTransform;
  LightIsRed: boolean;
  CamOrbitIsActive: boolean;

  LookTarget, LookDir: TVector3; // Motion vars for navigation
  DesiredUp: TVector3 = (Data: (0, 1, 0));

  TotalTimePassed: Single = 0;

  PShapeNodeColors: array of TShapeNodeColor; // Pointer

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
    CamOrbitIsActive := false;
    AngleRotate := -0.01 * (Event.Position.X - Event.OldPosition.X);
    LookTarget := Viewport1.Items.BoundingBox.Center;
    LookDir := LookTarget - Viewport1.Camera.Translation;
    LookDir := RotatePointAroundAxis(Vector4(DesiredUp, AngleRotate), LookDir);
    Viewport1.Camera.SetView(LookTarget - LookDir, LookDir, DesiredUp);
  end;
end;

procedure TCastleApp.UpdateMainCameraPosition(const ModelCenter: TVector3;
  const DistanceToModel, AngleU: Single);
var
  CPos: TVector3;
  LookAtDirection: TVector3;
begin
  CPos.X := ModelCenter.X + DistanceToModel * Cos(AngleU);
  CPos.Z := ModelCenter.Z + DistanceToModel * Sin(AngleU);
  CPos.Y := ModelCenter.Y + VerticalCameraOffset; // Vertical position
  LookAtDirection := ModelCenter - CPos;
  Viewport1.Camera.SetView(CPos, LookAtDirection, DesiredUp);
end;

procedure TCastleApp.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  ModelCenter: TVector3;
  DistanceToModel, AngleU: Single;
begin
  inherited;

  if CamOrbitIsActive then
  begin
    ModelCenter := Viewport1.Items.BoundingBox.Center;
    DistanceToModel := Viewport1.Items.BoundingBox.AverageSize *
      OrbitMultiplier;
    TotalTimePassed := TotalTimePassed + SecondsPassed;
    AngleU := (DefaultOrbitSpeed * TotalTimePassed) * RadiansPerDegree;
    UpdateMainCameraPosition(ModelCenter, DistanceToModel, AngleU);
  end;
end;

procedure TForm1.InitializeGLWin;
var
  CastleControls: TArray<TCastleControl>;
begin
  CastleControls := FindCastleControls(Self);
  IsCastleControlOnForm := Length(CastleControls) > 0;

  if IsCastleControlOnForm then
    GLWin := CastleControls[0]
  else
    GLWin := TCastleControl.Create(Self);

  GLWin.Parent := Self;
  GLWin.Align := alClient;
end;

procedure TForm1.FormCreate(Sender: TObject);
begin
  InitializeGLWin;

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
  ExitButton := CastleControl.Container.DesignedComponent('ExitButton')
    as TCastleButton;
  LabelFPS := CastleControl.Container.DesignedComponent('LabelFPS')
    as TCastleLabel;
  NavigateButton := CastleControl.Container.DesignedComponent('NavigateButton')
    as TCastleButton;

end;

procedure TForm1.ExitFailDetectionViewport(Sender: TObject);
begin
  Viewport2.Exists := false;
  MainNavIsActive := True;
  Viewport2.Navigation.Exists := false;
  CamOrbitIsActive := True;
end;

procedure TForm1.FullscreenFailDetectionWindow(Sender: TObject);
begin
  Viewport2.FullSize := True;
end;

procedure TForm1.ForwardEditorComponentsToMethods;
begin
  MakeBiggerButton.OnClick := FullscreenFailDetectionWindow;
  NavigateButton.OnClick := StartErrorDetectionNav;
  ExitButton.OnClick := ExitFailDetectionViewport;
end;

procedure TForm1.InitVars;
var
  I: Integer;
begin
  InitEditorComponents;
  ForwardEditorComponentsToMethods;

  ErrorManager := TErrorManager.Create;
  ModelProcessing := TModelProcessing.Create(ModelScene);


  FailureDection.Enabled := false;
  Viewport2.Navigation.Exists := false;
  MainNavIsActive := True;
  CamOrbitIsActive := True;

  // puts all animations in a list
  for I := 0 to ModelScene.AnimationsList.Count - 1 do
    ListBox1.AddItem(ModelScene.AnimationsList[I], nil);

  FailureDetected := false;

  // sets extra ErrorCamera in the world.
  Viewport2.Items := Viewport1.Items;
  Viewport2.Camera := ErrorCamera;
  Viewport2.Exists := false;

  // search for statuslight object
  try
    StatusLightAppNode := ModelScene.Node('M_StatusLight') as TAppearanceNode;
    MatStatusLight := StatusLightAppNode.Material as TPhysicalMaterialNode;

  except
    ErrorManager.HandleError('StatusLight not found.');
  end;

  LightIsRed := false;
end;

procedure TForm1.StartErrorDetectionNav(Sender: TObject);
begin

  if MainNavIsActive then
  begin
    MainNavIsActive := false;
    Viewport2.Navigation.Exists := True;
    CamOrbitIsActive := True;
  end
  else
  begin
    MainNavIsActive := True;
    Viewport2.Navigation.Exists := false;
    CamOrbitIsActive := True;
  end;
end;

procedure TForm1.FailureDetectionTimer(Sender: TObject);
var
  I: Integer;
begin
  if FailureDetected then
  begin
    if LightIsRed then
    begin
      if MatStatusLight <> nil then
        MatStatusLight.BaseColor := AlmostWhiteColor;
      LightIsRed := false;
      for I := 0 to Length(PShapeNodeColors) - 1 do
        PShapeNodeColors[I].PhysicalMatNode.BaseColor := PShapeNodeColors[I]
          .OriginalColor;
    end
    else
    begin
      if MatStatusLight <> nil then
        MatStatusLight.BaseColor := RedishColor;
      LightIsRed := True;
      for I := 0 to Length(PShapeNodeColors) - 1 do
        PShapeNodeColors[I].PhysicalMatNode.BaseColor := Vector3(0.9, 0.1, 0.1);

    end;
  end
end;

procedure TForm1.FindFailedNode(NodeName: string);
var
  I: Integer;
begin
var NewArray := ModelProcessing.SetFailedObject(NodeName);
     for I := Low(NewArray) to High(NewArray) do
    begin
        SetLength(PShapeNodeColors, I+1);
        PShapeNodeColors[I] := NewArray[I];
    end;

end;

procedure TForm1.NewCamera(Error: string);
var
  APos: TVector3;
  BboxSize: TBox3D;
  LookAtFailedTarget, FacingDirection: TVector3;
  FDistanceToModel: Single;

begin
  if FailureDetected then
  begin
    if not Viewport2.Exists then
    Viewport2.Exists := True;
    BboxSize := ModelProcessing.CalculateSumBbox(ModelScene.Shapes.TraverseList(True, True, True));
    FacingDirection := BboxSize.Center - Viewport2.Camera.Translation;
    FDistanceToModel := BboxSize.AverageSize * 2.5;
    APos := BboxSize.Center - (Normalized(FacingDirection) * FDistanceToModel);
    Viewport2.Camera.AnimateTo(APos, FacingDirection, DesiredUp, 1.5);
  end
  else
  begin
    if Viewport2.Exists then
      Viewport2.Exists := false;
  end;
end;

procedure TForm1.SetErrorButtonClick(Sender: TObject);
var
  Error: string;
  I: Integer;
begin
  Error := 'EthernetPort';
  // As TransformNode - Look at X3D file.
  // errordata containts location of error

  try
    if FailureDetected then
    begin // back to default
      SetFailedDetection;
      MatStatusLight.BaseColor := Vector3(0, 1, 0);
      for I := 0 to Length(PShapeNodeColors) - 1 do
        PShapeNodeColors[I].PhysicalMatNode.BaseColor := PShapeNodeColors[I]
          .OriginalColor; // set error material node color to original color
      SetLength(PShapeNodeColors, 0)
    end
    else
    begin // start error display
      SetFailedDetection;
      FindFailedNode(Error);
      NewCamera(Error);
    end;
  except
    ErrorManager.HandleError('BEEEP Object: "' + Error +
      '" location not found');
  end;
end;

procedure TForm1.SetFailedDetection();
begin
  if FailureDetected then
  begin // back to default
    FailureDetected := false;
    FailureDection.Enabled := false;
    Viewport2.Exists := false;
  end
  else
  begin // start error display
    FailureDetected := True; // This enables the animation in TTimer.
    FailureDection.Enabled := True;
    Viewport2.FullSize := false;
  end;
end;

procedure TForm1.StartStopAnimation(Sender: TObject);
begin
  ModelScene.StopAnimation(false);
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
