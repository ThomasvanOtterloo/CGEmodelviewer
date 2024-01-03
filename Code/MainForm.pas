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

  Math, ErrorManager, ModelProcessing, ShapeNodeColor, T3DViewManager,
  CastleControlManager;

type
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
    procedure NewCamera();
    procedure ListBoxPlayAnimation(Sender: TObject);

  private
    { Private declarations }
    GLView: TCastleApp;
    GLWin: TCastleControl;


    ErrorManager: TErrorManager;
    ModelProcessing: TModelProcessing;

    procedure ExitFailDetectionViewport(Sender: TObject);
    procedure FullscreenFailDetectionWindow(Sender: TObject);
    procedure InitVars;
    procedure InitEditorComponents;
    procedure FindFailedNode(NodeName: string);
    procedure StartErrorDetectionNav(Sender: TObject);
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

  MainCamera: TCastleCamera;
  ModelScene: TCastleScene;
  Viewport1: TCastleViewport;
  Viewport2: TCastleViewport;
  MakeBiggerButton: TCastleButton;
  ExitButton: TCastleButton;
  NavigateButton: TCastleButton;
  Items: TCastleRootTransform;
  ErrorCamera: TCastleCamera;

  LabelFPS: TCastleLabel;

  OrbitRadius, OrbitSpeed, Angle: Double;

  ErrorRootNode: TTransformNode; // TTransformNode RootNode;
  ErrorgroupNode: TGroupNode; // FIrst child

  FailureDetected: boolean;
  MatStatusLight: TPhysicalMaterialNode;
  StatusLightAppNode: TAppearanceNode;
  Hdmi: TCastleTransform;
  LightIsRed: boolean;

  PShapeNodeColors: array of TShapeNodeColor; // Pointer

implementation

{$R *.dfm}

uses System.Generics.Collections;

procedure TForm1.FormCreate(Sender: TObject);
begin
  var a := CastleControlManager.InitializeGLWin(self);
  GLView := TCastleApp.Create(self,a);
  CastleControlManager.SetGLWinView(GLView);
  InitVars;
end;





procedure TForm1.InitEditorComponents;
begin
  CastleControl := CastleControlManager.GetCastleControl;

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
  GLView.SetBoolMainNav(true);
  Viewport2.Navigation.Exists := false;
  GLView.SetBoolCamOrbitIsActive(true);
end;

procedure TForm1.FullscreenFailDetectionWindow(Sender: TObject);
begin
  Viewport2.FullSize := true;
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


  GLView.SetBoolMainNav(true);
  GLView.SetBoolCamOrbitIsActive(true);

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

  if GLView.GetBoolMainNav then
  begin
    GLView.SetBoolMainNav(false);
    Viewport2.Navigation.Exists := true;
    GLView.SetBoolCamOrbitIsActive(true);
  end
  else
  begin
    GLView.SetBoolMainNav(true);
    Viewport2.Navigation.Exists := false;
    GLView.SetBoolCamOrbitIsActive(true);
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
      LightIsRed := true;
      for I := 0 to Length(PShapeNodeColors) - 1 do
        PShapeNodeColors[I].PhysicalMatNode.BaseColor := Vector3(0.9, 0.1, 0.1);

    end;
  end
end;

procedure TForm1.FindFailedNode(NodeName: string);
var
  I: Integer;
begin
  var
  NewArray := ModelProcessing.SetFailedObject(NodeName);
  for I := Low(NewArray) to High(NewArray) do
  begin
    SetLength(PShapeNodeColors, I + 1);
    PShapeNodeColors[I] := NewArray[I];
  end;

end;

procedure TForm1.NewCamera;   //todo; to new class. Camera class? or existing?
var
  APos: TVector3;
  BboxSize: TBox3D;
  FacingDirection: TVector3;
  FDistanceToModel: Single;

const
  DesiredUp: TVector3 = (Data: (0, 1, 0));

begin
  if FailureDetected then
  begin
    if not Viewport2.Exists then
      Viewport2.Exists := true;
      GLView.CalculateNewCameraPos(ModelScene);
  end
  else
  begin
    if Viewport2.Exists then
      Viewport2.Exists := false;
  end;
end;

procedure TForm1.SetErrorButtonClick(Sender: TObject);   //+
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
      NewCamera;
    end;
  except
     on E: Exception do
      ErrorManager.HandleError('Error matching X3DNames CalculatingSumBox: ' + E.Message);
  end;
end;




procedure TForm1.SetFailedDetection(); //-
begin
  if FailureDetected then
  begin // back to default
    FailureDetected := false;
    FailureDection.Enabled := false;
    Viewport2.Exists := false;
  end
  else
  begin // start error display
    FailureDetected := true; // This enables the animation in TTimer.
    FailureDection.Enabled := true;
    Viewport2.FullSize := false;
  end;
end;



//animations

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
    ModelScene.PlayAnimation(ItemValue, true);
  end;
end;

end.