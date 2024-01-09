unit I_T3DViewManager;
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
R_T3DViewManager = interface

    constructor Create(AOwner: TForm; CastleControl: TCastleControl);


    procedure SetBoolCamOrbitIsActive(boolInput: boolean);
    procedure SetBoolMainNav(boolInput: boolean);
    function GetBoolMainNav: boolean;
    procedure CalculateNewCameraPos(ModelProcessing: TModelProcessing;
      ModelScene: TCastleScene);

    // methods
    procedure Update(const SecondsPassed: Single;
      var HandleInput: boolean);
    function Motion(const Event: TInputMotion): boolean;
    procedure UpdateMainCameraPosition(const ModelCenter: TVector3;
      const DistanceToModel, AngleU: Single);
    procedure ErrorNavSetup(Sender: TObject);


end;

implementation

end.
