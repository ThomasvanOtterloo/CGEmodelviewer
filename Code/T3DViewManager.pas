unit T3DViewManager;

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
  Vcl.CastleControl, CastleControlManager;

type
  TCastleApp = class(TCastleView)
  published // items added from editor

  private
    LookDir: TVector3; // Motion vars for navigation
    LookTarget: TVector3;
    TotalTimePassed: Single;
    CamOrbitIsActive: boolean;
    CastleControl: TCastleControl;

    Viewport1: TCastleViewport;
    MainNavIsActive: boolean;

  const
    OrbitMultiplier = 1.75;
    VerticalCameraOffset = 20;
    RadiansPerDegree = Pi / 180;
    DesiredUp: TVector3 = (Data: (0, 1, 0));
    DefaultOrbitSpeed = 10; // Degrees per second

  public
    // getters / setters
    procedure SetBoolCamOrbitIsActive(bool: boolean);
    procedure SetBoolMainNav(bool: boolean);
    function GetBoolMainNav: boolean;

    // methods
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean); override;
    function Motion(const Event: TInputMotion): boolean; override;
    procedure UpdateMainCameraPosition(const ModelCenter: TVector3;
      const DistanceToModel, AngleU: Single);
    constructor Create(AOwner: TComponent);

  end;

implementation

constructor TCastleApp.Create(AOwner: TComponent);
begin
  TotalTimePassed := 0;
  CamOrbitIsActive := true;
  CastleControl := CastleControlManager.GetCastleControl;

  Viewport1 := CastleControl.Container.DesignedComponent('Viewport1')
    as TCastleViewport;
end;

procedure TCastleApp.SetBoolCamOrbitIsActive(bool: boolean);
begin
  CamOrbitIsActive := bool;
end;

procedure TCastleApp.SetBoolMainNav(bool: boolean);
begin
  MainNavIsActive := bool;
end;

procedure TCastleApp.Update(const SecondsPassed: Single;
  var HandleInput: boolean);
var
  ModelCenter: TVector3;
  DistanceToModel, AngleU: Single;
begin
  inherited;

  if CamOrbitIsActive then // private boolean in TForm. smart way to access it?
  begin
    ModelCenter := Viewport1.Items.BoundingBox.Center;
    // private boolean in TForm
    DistanceToModel := Viewport1.Items.BoundingBox.AverageSize *
      OrbitMultiplier;
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
  Viewport1.Camera.SetView(CPos, LookAtDirection, DesiredUp);
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
    LookTarget := Viewport1.Items.BoundingBox.Center;
    LookDir := LookTarget - Viewport1.Camera.Translation;
    LookDir := RotatePointAroundAxis(Vector4(DesiredUp, AngleRotate), LookDir);
    Viewport1.Camera.SetView(LookTarget - LookDir, LookDir, DesiredUp);
  end;
end;

end.
