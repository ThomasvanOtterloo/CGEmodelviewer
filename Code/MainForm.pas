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
    FailureDetectionTTimer: TTimer;
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
    MainCamera: TCastleCamera;
    ModelScene: TCastleScene;
    DefaultViewport: TCastleViewport;
    ErrorScopeViewport: TCastleViewport;
    MakeBiggerButton: TCastleButton;
    ExitButton: TCastleButton;
    NavigateButton: TCastleButton;
    Items: TCastleRootTransform;
    ErrorCamera: TCastleCamera;

    LabelFPS: TCastleLabel;

    OrbitRadius, OrbitSpeed, Angle: Double;

    ErrorRootNode: TTransformNode; // TTransformNode RootNode;
    ErrorgroupNode: TGroupNode; // FIrst child

    FailureDetectedIsActive: boolean;

    LightIsRed: boolean;

    // PShapeNodeColors: array of TShapeNodeColor; // Pointer

    ErrorManager: TErrorManager;
    ModelProcessing: TModelProcessing;

    procedure ExitFailDetectionViewport(Sender: TObject);
    procedure FullscreenFailDetectionWindow(Sender: TObject);
    procedure InitVars;
    procedure InitEditorComponents;
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

implementation

{$R *.dfm}

uses System.Generics.Collections;

procedure TForm1.FormCreate(Sender: TObject);
begin
  var
  a := CastleControlManager.InitializeGLWin(self);
  GLView := TCastleApp.Create(self, a);
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

  DefaultViewport := CastleControl.Container.DesignedComponent('Viewport1')
    as TCastleViewport;

  ErrorScopeViewport := CastleControl.Container.DesignedComponent('Viewport2')
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
  ErrorScopeViewport.Exists := false;
  GLView.SetBoolMainNav(true);
  ErrorScopeViewport.Navigation.Exists := false;
  GLView.SetBoolCamOrbitIsActive(true);
end;

procedure TForm1.FullscreenFailDetectionWindow(Sender: TObject);
begin
  ErrorScopeViewport.FullSize := true;
end;

procedure TForm1.ForwardEditorComponentsToMethods;
begin
  MakeBiggerButton.OnClick := FullscreenFailDetectionWindow;
  NavigateButton.OnClick := GLView.ErrorNavSetup;
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

  FailureDetectionTTimer.Enabled := false;
  ErrorScopeViewport.Navigation.Exists := false;

  GLView.SetBoolMainNav(true);
  GLView.SetBoolCamOrbitIsActive(true);

  // puts all animations in a list
  for I := 0 to ModelScene.AnimationsList.Count - 1 do
    ListBox1.AddItem(ModelScene.AnimationsList[I], nil);

  FailureDetectedIsActive := false;

  // sets extra ErrorCamera in the world.
  ErrorScopeViewport.Items := DefaultViewport.Items;
  ErrorScopeViewport.Camera := ErrorCamera;
  ErrorScopeViewport.Exists := false;
end;



procedure TForm1.FailureDetectionTimer(Sender: TObject);
var
  I: Integer;
begin
  if FailureDetectedIsActive then
  begin
    ModelProcessing.AnimateStatusLight;
  end
end;



procedure TForm1.NewCamera;
begin
  if FailureDetectedIsActive then
  begin
    if not ErrorScopeViewport.Exists then
      ErrorScopeViewport.Exists := true;
    GLView.CalculateNewCameraPos(ModelProcessing, ModelScene);
  end
  else
  begin
    if ErrorScopeViewport.Exists then
      ErrorScopeViewport.Exists := false;
  end;
end;

procedure TForm1.SetErrorButtonClick(Sender: TObject); // +
var
  Error: string;
  I: Integer;
begin
  Error := 'EthernetPort';
  // As TransformNode - Look at X3D file.
  // errordata containts location of error
  if FailureDetectedIsActive then
  begin
     FailureDetectedIsActive := false;
  end
  else
  begin
     FailureDetectedIsActive := true;
  end;

  try
    if FailureDetectedIsActive then
    begin // start error display
      SetFailedDetection;
      ModelProcessing.SetFailedObject(Error);
      NewCamera;

    end
    else
    begin // back to default
      SetFailedDetection;
      ModelProcessing.BackToOriginal;
    end;
  except
    on E: Exception do
      ErrorManager.HandleError('Error matching X3DNames CalculatingSumBox: ' +
        E.Message);
  end;
end;

procedure TForm1.SetFailedDetection(); // -
begin
  if FailureDetectedIsActive then
  begin // start error display
    FailureDetectionTTimer.Enabled := true;
    ErrorScopeViewport.FullSize := false;
  end
  else
  begin // back to default
    FailureDetectionTTimer.Enabled := false;
    ErrorScopeViewport.Exists := false;
  end
end;



// animations

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
