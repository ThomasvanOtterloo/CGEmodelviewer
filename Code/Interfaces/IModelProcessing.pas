unit IModelProcessing;
interface

uses
   System.SysUtils,

  CastleShapes, CastleCameras, X3DNodes, X3DLoad, CastleTransform,
  CastleBoxes, CastleSceneCore, X3DLoadInternalUtils, CastleUIControls,
  CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleUtils, CastleTriangles,
  CastleRectangles, CastleColors, CastleScene, CastleViewport,
  Vcl.CastleControl,

  ErrorManager, ShapeNodeColor;

type
R_ModelProcessing = interface

    function SetFailedObject(NodeName: string): TArray<TShapeNodeColor>;
    function CalculateSumBbox(ShapeList: TShapeList): TBox3D;
    procedure AnimateStatusLight;
    procedure BackToOriginal;

end;

implementation

end.
