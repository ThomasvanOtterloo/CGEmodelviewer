unit T3DViewManager;

interface

uses
  Winapi.Windows, Winapi.Messages, System.SysUtils, System.Variants,
  System.Classes, Vcl.Graphics, Vcl.Controls, Vcl.Forms, Vcl.Dialogs,
  Vcl.StdCtrls, Vcl.ExtCtrls, ModelProcessing,

  CastleShapes, CastleCameras, X3DNodes, X3DLoad, CastleTransform,
  CastleBoxes, CastleSceneCore, X3DLoadInternalUtils, CastleUIControls,
  CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleUtils, CastleTriangles,
  CastleRectangles, CastleColors, CastleScene, CastleViewport,
  Vcl.CastleControl;

type
  TCastleApp = class(TCastleView)
  published // items added from editor

  private
    LookDir: TVector3; // Motion vars for navigation
    LookTarget: TVector3;
    TotalTimePassed: Single;
    CamOrbitIsActive: boolean;
    CastleControl: TCastleControl;
    DefaultViewport: TCastleViewport;
    ErrorScopeViewport: TCastleViewport;
    MainNavIsActive: boolean;
    ModelProcessing: TModelProcessing;
    ModelScene: TCastleScene;
    ModelCenter: TVector3;
    DistanceToModel, AngleU: Single;
    ExamineNavigation: TCastleExamineNavigation;
    procedure ErrorNavController;

  const
    OrbitMultiplier = 1.75;
    VerticalCameraOffset = 20;
    RadiansPerDegree = Pi / 180;
    DesiredUp: TVector3 = (Data: (0, 1, 0));
    DefaultOrbitSpeed = 10; // Degrees per second

  public
    // getters / setters
    procedure SetBoolCamOrbitIsActive(boolInput: boolean);
    procedure SetBoolMainNav(boolInput: boolean);
    function GetBoolMainNav: boolean;
    procedure CalculateNewCameraPos(ModelProcessing: TModelProcessing;
      ModelScene: TCastleScene);

    // methods
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure UpdateMainCameraPosition(const ModelCenter: TVector3;
      const DistanceToModel, AngleU: Single);
    constructor Create(AOwner: TForm; CastleControl: TCastleControl);
    procedure ErrorNavSetup(Sender: TObject);
  end;

implementation

constructor TCastleApp.Create(AOwner: TForm; CastleControl: TCastleControl);
begin
  inherited Create(AOwner);
  TotalTimePassed := 0;
  CamOrbitIsActive := true;
  MainNavIsActive := true;

  DefaultViewport := CastleControl.Container.DesignedComponent('Viewport1')
    as TCastleViewport;
  ModelCenter := DefaultViewport.Items.BoundingBox.Center;
  DistanceToModel := DefaultViewport.Items.BoundingBox.AverageSize *
    OrbitMultiplier;

  ErrorScopeViewport := CastleControl.Container.DesignedComponent('Viewport2')
    as TCastleViewport;

  ExamineNavigation := CastleControl.Container.DesignedComponent
    ('ExamineNavigation1') as TCastleExamineNavigation;

end;

procedure TCastleApp.SetBoolCamOrbitIsActive(boolInput: boolean);
begin
  CamOrbitIsActive := boolInput;
end;

procedure TCastleApp.SetBoolMainNav(boolInput: boolean);
begin
  MainNavIsActive := boolInput;
end;

procedure TCastleApp.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  AngleU: Single;
begin
  inherited;

  if CamOrbitIsActive then
  begin
    TotalTimePassed := TotalTimePassed + SecondsPassed;
    AngleU := (DefaultOrbitSpeed * TotalTimePassed) * RadiansPerDegree;
    UpdateMainCameraPosition(ModelCenter, DistanceToModel, AngleU);
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
  DefaultViewport.Camera.SetView(CPos, LookAtDirection, DesiredUp);
end;

function TCastleApp.GetBoolMainNav: boolean;
begin
  result := MainNavIsActive

end;

function TCastleApp.Motion(const Event: TInputMotion): boolean;
var
  AngleRotate: Single;
begin
  result := inherited;
  if result then
    Exit;
  if (buttonLeft in Event.Pressed) and (MainNavIsActive) then
  begin
    CamOrbitIsActive := false;
    AngleRotate := -0.01 * (Event.Position.X - Event.OldPosition.X);
    LookTarget := ModelCenter;
    LookDir := LookTarget - DefaultViewport.Camera.Translation;
    LookDir := RotatePointAroundAxis(Vector4(DesiredUp, AngleRotate), LookDir);
    DefaultViewport.Camera.SetView(LookTarget - LookDir, LookDir, DesiredUp);
  end;
end;

procedure TCastleApp.CalculateNewCameraPos(ModelProcessing: TModelProcessing;
  ModelScene: TCastleScene);
var
  APos: TVector3;
  BboxSize: TBox3D;
  FacingDirection: TVector3;
  FDistanceToModel: Single;

const
  DesiredUp: TVector3 = (Data: (0, 1, 0));

begin
  BboxSize := ModelProcessing.CalculateSumBbox
    (ModelScene.Shapes.TraverseList(true, true, true));
  FacingDirection := BboxSize.Center - ErrorScopeViewport.Camera.Translation;
  FDistanceToModel := BboxSize.AverageSize * 2.5;
  APos := BboxSize.Center - (Normalized(FacingDirection) * FDistanceToModel);
  ErrorScopeViewport.Camera.AnimateTo(APos, FacingDirection, DesiredUp, 1.5);

  // trying to fix the navigation here?
   ExamineNavigation.AutoCenterOfRotation := false;
  ExamineNavigation.CenterOfRotation := BboxSize.Center;


end;

procedure TCastleApp.ErrorNavSetup(Sender: TObject);
begin
  if GetBoolMainNav then
  begin
    SetBoolMainNav(false);
    ErrorScopeViewport.Navigation.Exists := true;
    SetBoolCamOrbitIsActive(true);
  end
  else
  begin
    SetBoolMainNav(true);
    ErrorScopeViewport.Navigation.Exists := false;
    SetBoolCamOrbitIsActive(true);
  end;

end;

procedure TCastleApp.ErrorNavController();
begin
  ErrorScopeViewport.Navigation.Radius := 0.05;



end;

end.
