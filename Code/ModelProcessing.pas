unit ModelProcessing;

interface

uses
  System.SysUtils,

  CastleShapes, CastleCameras, X3DNodes, X3DLoad, CastleTransform,
  CastleBoxes, CastleSceneCore, X3DLoadInternalUtils, CastleUIControls,
  CastleComponentSerialize, CastleKeysMouse, CastleLog, CastleControls,
  CastleVectors, CastleGLUtils, CastleUtils, CastleTriangles,
  CastleRectangles, CastleColors, CastleScene, CastleViewport,
  Vcl.CastleControl,

   ErrorManager,ShapeNodeColor;

type
  TModelProcessing = class
  private

    ErrorRootNode: TTransformNode; // TTransformNode RootNode;
    ErrorgroupNode: TGroupNode; // FIrst child
    ModelScene: TCastleScene;
    ErrorManager: TErrorManager;
    PShapeNodeColors: array of TShapeNodeColor;
    procedure FindRootNodeAndBreakdown(var ErrorRootNode: TTransformNode; NodeName: string);


  public
    constructor Create(ModelScene: TCastleScene);
    destructor Destroy; override;
    function SetFailedObject(NodeName: string):TArray<TShapeNodeColor>;
     function CalculateSumBbox(ShapeList: TShapeList): TBox3D;
  published

  end;

implementation

constructor TModelProcessing.Create(ModelScene: TCastleScene);
begin

  self.ModelScene := ModelScene;
  ErrorManager := TErrorManager.Create;


end;

destructor TModelProcessing.Destroy;
begin
  inherited;
end;

function TModelProcessing.SetFailedObject(NodeName: string): TArray<TShapeNodeColor>;
var
  I: Integer;
  newNodeColor: TShapeNodeColor;

  ErrorShapeNode: TShapeNode;
  ErrorAppearanceNode: TAppearanceNode;
  ErrorMaterialNode: TPhysicalMaterialNode;
begin
  FindRootNodeAndBreakdown(ErrorRootNode, NodeName);

  try
    ErrorgroupNode := ErrorRootNode.FdChildren[0] as TGroupNode; // first child
    for I := 0 to ErrorgroupNode.FdChildren.Count - 1 do
    begin
      // break down untill you have the Physical Material Node to retrieve the materials
      ErrorShapeNode := ErrorgroupNode.FdChildren[I] as TShapeNode;
      ErrorAppearanceNode := ErrorShapeNode.Appearance as TAppearanceNode;
      ErrorMaterialNode := ErrorAppearanceNode.Material as
        TPhysicalMaterialNode;

      // Add values in list
      newNodeColor.PhysicalMatNode :=
        ErrorMaterialNode as TPhysicalMaterialNode;
      newNodeColor.OriginalColor := ErrorMaterialNode.BaseColor;

      SetLength(PShapeNodeColors, I + 1);
      PShapeNodeColors[I] := newNodeColor;


    end;
     // Set the length of the result array to match PShapeNodeColors
    SetLength(Result, Length(PShapeNodeColors));

    // Copy each element from PShapeNodeColors to Result
    for I := Low(PShapeNodeColors) to High(PShapeNodeColors) do
    begin
      Result[I] := PShapeNodeColors[I];
    end;
  except
    ErrorManager.HandleError
      ('Something went wrong creating the array of shapes. (children of TransformNode)');
  end;
end;



 function TModelProcessing.CalculateSumBbox(ShapeList: TShapeList): TBox3D;
var
  I: Integer;
  SumBox: TBox3D;

begin
  try
    for I := 0 to ShapeList.Count - 1 do
    begin
      if ShapeList[I].GeometryGrandParentNode.X3DName = ErrorgroupNode.X3DName
      then
      begin
        SumBox := SumBox + ShapeList[I].BoundingBox;
      end;
    end;
    Result := SumBox.Transform(ModelScene.WorldTransform);
  except
    on E: Exception do
      ErrorManager.HandleError('Error: ' + E.Message);

  end;
end;







procedure TModelProcessing.FindRootNodeAndBreakdown(var ErrorRootNode: TTransformNode; NodeName: string);
begin
  try
    ErrorRootNode := ModelScene.RootNode.FindNode(NodeName) as TTransformNode;
    // Root
    while (ErrorRootNode <> nil) and (ErrorRootNode.FdChildren.Count > 0) and (ErrorRootNode.FdChildren[0] is TTransformNode) do
    begin
      ErrorRootNode := ErrorRootNode.FdChildren[0] as TTransformNode;
    end;
  except
    // recursive pattern
    ErrorManager.HandleError('Given part name is not found in X3D file.');
  end;
end;




end.

