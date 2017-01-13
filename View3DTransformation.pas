unit View3DTransformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, GR32, GR32_Transforms;

type

  TFloatMatrix4D = array[0..3, 0..3] of TFloat;
  TFixedMatrix4D = array[0..3, 0..3] of TFixed;

  TVector4f = array[0..3] of TFloat;

const

  IdentityMatrix4D: TFloatMatrix4D = (
    (1, 0, 0, 0),
    (0, 1, 0, 0),
    (0, 0, 1, 0),
    (0, 0, 0, 1));

type

  { TView3DTransformation }

  TView3DTransformation = class(TTransformation)
  protected
    FOriginX: single;
    FOriginY: single;
    FOriginZ: single;
    FScaleX: single;
    FScaleY: single;
    FScaleZ: single;
    FRotationX: single;  // radians
    FRotationY: single;  // radians
    FRotationZ: single;  // radians
    FCameraZ: single;
    FTranslationX: single;
    FTranslationY: single;
    FTranslationZ: single;
    FPointZ: single;
    FFloatMatrix: TFloatMatrix4D;
    FFixedMatrix: TFixedMatrix4D;

    procedure TransformFloat(SrcX, SrcY: TFloat; out DstX, DstY: TFloat); override;
    procedure TransformFixed(SrcX, SrcY: TFixed; out DstX, DstY: TFixed); override;
    procedure PrepareTransform; override;
    procedure ReverseTransformFloat(DstX, DstY: TFloat; out SrcX, SrcY: TFloat);
      override;

    procedure RotateX;
    procedure RotateY;
    procedure RotateZ;
    procedure Translate(const TX, TY, TZ: single);
    procedure Scale;
    procedure ApplyPerspective;
  public
    constructor Create;
    function GetTransformedBounds(const ASrcRect: TFloatRect): TFloatRect; override;
    procedure SetOrigin(const OX, OY, OZ: single);
    procedure SetScale(const SX, SY, SZ: single);
    procedure SetRotationX(Deg: single);
    procedure SetRotationY(Deg: single);
    procedure SetRotationZ(Deg: single);
    procedure SetCameraZ(V: single);
    procedure SetTranslation(const TX, TY, TZ: single);
    procedure SetPointZ(V: single);
    procedure Transform(const X, Y, Z: single; out P: TFixedPoint); overload;
    procedure Transform(const X, Y, Z: single; out P: TPoint); overload;
  end;

implementation

uses
  Math, GR32_Math;

function Mult(const M1, M2: TFloatMatrix4D): TFloatMatrix4D;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] :=
        M1[0, j] * M2[i, 0] + M1[1, j] * M2[i, 1] + M1[2, j] *
        M2[i, 2] + M1[3, j] * M2[i, 3];
end;

function Mult(const M1, M2: TFixedMatrix4D): TFixedMatrix4D;
var
  i, j: integer;
begin
  for i := 0 to 3 do
    for j := 0 to 3 do
      Result[i, j] :=
        M1[0, j] * M2[i, 0] + M1[1, j] * M2[i, 1] + M1[2, j] *
        M2[i, 2] + M1[3, j] * M2[i, 3];
end;

function FixedMatrix(const M: TFloatMatrix4D): TFixedMatrix4D;
begin
  Result[0, 0] := Round(M[0, 0] * FixedOne);
  Result[0, 1] := Round(M[0, 1] * FixedOne);
  Result[0, 2] := Round(M[0, 2] * FixedOne);
  Result[0, 3] := Round(M[0, 3] * FixedOne);
  Result[1, 0] := Round(M[1, 0] * FixedOne);
  Result[1, 1] := Round(M[1, 1] * FixedOne);
  Result[1, 2] := Round(M[1, 2] * FixedOne);
  Result[1, 3] := Round(M[1, 3] * FixedOne);
  Result[2, 0] := Round(M[2, 0] * FixedOne);
  Result[2, 1] := Round(M[2, 1] * FixedOne);
  Result[2, 2] := Round(M[2, 2] * FixedOne);
  Result[2, 3] := Round(M[2, 3] * FixedOne);
  Result[3, 0] := Round(M[3, 0] * FixedOne);
  Result[3, 1] := Round(M[3, 1] * FixedOne);
  Result[3, 2] := Round(M[3, 2] * FixedOne);
  Result[3, 3] := Round(M[3, 3] * FixedOne);
end;

function VectorTransform(const M: TFloatMatrix4D; const V: TVector4f): TVector4f;
begin
  Result[0] := M[0, 0] * V[0] + M[1, 0] * V[1] + M[2, 0] * V[2] + M[3, 0] * V[3];
  Result[1] := M[0, 1] * V[0] + M[1, 1] * V[1] + M[2, 1] * V[2] + M[3, 1] * V[3];
  Result[2] := M[0, 2] * V[0] + M[1, 2] * V[1] + M[2, 2] * V[2] + M[3, 2] * V[3];
  Result[3] := M[0, 3] * V[0] + M[1, 3] * V[1] + M[2, 3] * V[2] + M[3, 3] * V[3];
end;

{ TView3DTransformation }

constructor TView3DTransformation.Create;
begin
  FScaleX := 1;
  FScaleY := 1;
  FScaleZ := 1;
  Changed;
end;

function TView3DTransformation.GetTransformedBounds(
  const ASrcRect: TFloatRect): TFloatRect;
var
  V1, V2, V3, V4: TVector4f;
begin
  V1[0] := ASrcRect.Left;
  V1[1] := ASrcRect.Top;
  V1[2] := 1;
  V1[3] := 1;
  V2[0] := ASrcRect.Right;
  V2[1] := V1[1];
  V2[2] := 1;
  V2[3] := 1;
  V3[0] := V1[0];
  V3[1] := ASrcRect.Bottom;
  V3[2] := 1;
  V3[3] := 1;
  V4[0] := V2[0];
  V4[1] := V3[1];
  V4[2] := 1;
  V4[3] := 1;
  V1 := VectorTransform(FFloatMatrix, V1);
  V2 := VectorTransform(FFloatMatrix, V2);
  V3 := VectorTransform(FFloatMatrix, V3);
  V4 := VectorTransform(FFloatMatrix, V4);
  Result.Left := Min(Min(V1[0], V2[0]), Min(V3[0], V4[0]));
  Result.Right := Max(Max(V1[0], V2[0]), Max(V3[0], V4[0]));
  Result.Top := Min(Min(V1[1], V2[1]), Min(V3[1], V4[1]));
  Result.Bottom := Max(Max(V1[1], V2[1]), Max(V3[1], V4[1]));
end;

procedure TView3DTransformation.TransformFloat(SrcX, SrcY: TFloat;
  out DstX, DstY: TFloat);
var
  W: TFloat;
begin
  DstX := FFloatMatrix[0, 0] * SrcX + FFloatMatrix[1, 0] * SrcY +
    FFloatMatrix[2, 0] * FPointZ + FFloatMatrix[3, 0];
  DstY := FFloatMatrix[0, 1] * SrcX + FFloatMatrix[1, 1] * SrcY +
    FFloatMatrix[2, 1] * FPointZ + FFloatMatrix[3, 1];
  W := FFloatMatrix[0, 3] * SrcX + FFloatMatrix[1, 3] * SrcY +
    FFloatMatrix[2, 3] * FPointZ + FFloatMatrix[3, 3];

  DstX := DstX / W;
  DstY := DstY / W;
end;

procedure TView3DTransformation.TransformFixed(SrcX, SrcY: TFixed;
  out DstX, DstY: TFixed);
var
  SrcZ, W: TFixed;
begin
  SrcZ := Fixed(FPointZ);

  DstX := FixedMul(SrcX, FFixedMatrix[0, 0]) + FixedMul(SrcY, FFixedMatrix[1, 0]) +
    FixedMul(SrcZ, FFixedMatrix[2, 0]) + FFixedMatrix[3, 0];
  DstY := FixedMul(SrcX, FFixedMatrix[0, 1]) + FixedMul(SrcY, FFixedMatrix[1, 1]) +
    FixedMul(SrcZ, FFixedMatrix[2, 1]) + FFixedMatrix[3, 1];
  W := FixedMul(SrcX, FFixedMatrix[0, 3]) + FixedMul(SrcY, FFixedMatrix[1, 3]) +
    FixedMul(SrcZ, FFixedMatrix[2, 3]) + FFixedMatrix[3, 3];

  DstX := FixedDiv(DstX, W);
  DstY := FixedDiv(DstY, W);
end;

procedure TView3DTransformation.PrepareTransform;
begin
  FFloatMatrix := IdentityMatrix4D;

  // Note that order is important here.
  Translate(FOriginX, FOriginY, FOriginZ);
  Scale;
  RotateY;
  ApplyPerspective;
  Translate(FTranslationX, FTranslationY, FTranslationZ);

  FFixedMatrix := FixedMatrix(FFloatMatrix);
  TransformValid := True;
end;

procedure TView3DTransformation.ReverseTransformFloat(DstX, DstY: TFloat;
  out SrcX, SrcY: TFloat);
begin
  //TODO:
end;

procedure TView3DTransformation.RotateX;
var
  M: TFloatMatrix4D;
  S, C: TFloat;
begin
  SinCos(FRotationX, S, C);
  M[0, 0] := 1;
  M[1, 0] := 0;
  M[2, 0] := 0;
  M[3, 0] := 0;
  M[0, 1] := 0;
  M[1, 1] := C;
  M[2, 1] := -S;
  M[3, 1] := 0;
  M[0, 2] := 0;
  M[1, 2] := S;
  M[2, 2] := C;
  M[3, 2] := 0;
  M[0, 3] := 0;
  M[1, 3] := 0;
  M[2, 3] := 0;
  M[3, 3] := 1;
  FFloatMatrix := Mult(M, FFloatMatrix);
end;

procedure TView3DTransformation.RotateY;
var
  M: TFloatMatrix4D;
  S, C: TFloat;
begin
  SinCos(FRotationY, S, C);
  M[0, 0] := C;
  M[1, 0] := 0;
  M[2, 0] := S;
  M[3, 0] := 0;
  M[0, 1] := 0;
  M[1, 1] := 1;
  M[2, 1] := 0;
  M[3, 1] := 0;
  M[0, 2] := -S;
  M[1, 2] := 0;
  M[2, 2] := C;
  M[3, 2] := 0;
  M[0, 3] := 0;
  M[1, 3] := 0;
  M[2, 3] := 0;
  M[3, 3] := 1;
  FFloatMatrix := Mult(M, FFloatMatrix);
end;

procedure TView3DTransformation.RotateZ;
var
  M: TFloatMatrix4D;
  S, C: TFloat;
begin
  SinCos(FRotationZ, S, C);
  M[0, 0] := C;
  M[1, 0] := -S;
  M[2, 0] := 0;
  M[3, 0] := 0;
  M[0, 1] := S;
  M[1, 1] := C;
  M[2, 1] := 0;
  M[3, 1] := 0;
  M[0, 2] := 0;
  M[1, 2] := 0;
  M[2, 2] := 1;
  M[3, 2] := 0;
  M[0, 3] := 0;
  M[1, 3] := 0;
  M[2, 3] := 0;
  M[3, 3] := 1;
  FFloatMatrix := Mult(M, FFloatMatrix);
end;

procedure TView3DTransformation.Translate(const TX, TY, TZ: single);
var
  M: TFloatMatrix4D;
begin
  M := IdentityMatrix4D;
  M[3, 0] := TX;
  M[3, 1] := TY;
  M[3, 2] := TZ;
  FFloatMatrix := Mult(M, FFloatMatrix);
end;

procedure TView3DTransformation.Scale;
var
  M: TFloatMatrix4D;
begin
  M := IdentityMatrix4D;
  M[0, 0] := FScaleX;
  M[1, 1] := FScaleY;
  M[2, 2] := FScaleZ;
  FFloatMatrix := Mult(M, FFloatMatrix);
end;

procedure TView3DTransformation.ApplyPerspective;
var
  M: TFloatMatrix4D;
begin
  M := IdentityMatrix4D;
  M[2, 3] := -1 / FCameraZ; // this is our W
  FFloatMatrix := Mult(M, FFloatMatrix);
end;

procedure TView3DTransformation.SetOrigin(const OX, OY, OZ: single);
begin
  FOriginX := OX;
  FOriginY := OY;
  FOriginZ := OZ;
  Changed;
end;

procedure TView3DTransformation.SetScale(const SX, SY, SZ: single);
begin
  FScaleX := SX;
  FScaleY := SY;
  FScaleZ := SZ;
  Changed;
end;

procedure TView3DTransformation.SetRotationX(Deg: single);
begin
  FRotationX := degtorad(Deg);
  Changed;
end;

procedure TView3DTransformation.SetRotationY(Deg: single);
begin
  FRotationY := degtorad(Deg);
  Changed;
end;

procedure TView3DTransformation.SetRotationZ(Deg: single);
begin
  FRotationZ := degtorad(Deg);
  Changed;
end;

procedure TView3DTransformation.SetCameraZ(V: single);
begin
  FCameraZ := V;
  Changed;
end;

procedure TView3DTransformation.SetTranslation(const TX, TY, TZ: single);
begin
  FTranslationX := TX;
  FTranslationY := TY;
  FTranslationZ := TZ;
  Changed;
end;

procedure TView3DTransformation.SetPointZ(V: single);
begin
  // No need to call Changed here because this property doesn't affect the matrix.
  // This is only used inside TransformXXX() methods.
  FPointZ := V;
end;

procedure TView3DTransformation.Transform(const X, Y, Z: single; out P: TFixedPoint);
var
  DstX, DstY: single;
begin
  SetPointZ(Z);
  if not TransformValid then
    PrepareTransform;
  TransformFloat(X, Y, DstX, DstY);
  P.X := Fixed(DstX);
  P.Y := Fixed(DstY);
end;

procedure TView3DTransformation.Transform(const X, Y, Z: single; out P: TPoint);
var
  DstX, DstY: single;
begin
  SetPointZ(Z);
  if not TransformValid then
    PrepareTransform;
  TransformFloat(X, Y, DstX, DstY);
  P.X := Round(DstX);
  P.Y := Round(DstY);
end;

end.

