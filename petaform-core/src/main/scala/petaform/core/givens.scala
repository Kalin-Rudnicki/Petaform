package petaform.core

given Conversion[RawPetaformAST, RawPetaformAST.Obj.Value] =
  RawPetaformAST.Obj.Value.Provided(false, _)

given Conversion[Interpolation, RawPetaformAST.Obj.Value] =
  i => RawPetaformAST.Obj.Value.Provided(false, RawPetaformAST.Interp(i))
given Conversion[Interpolation, RawPetaformAST] =
  RawPetaformAST.Interp(_)

given Conversion[InterpolatedString, RawPetaformAST.Obj.Value] =
  i => RawPetaformAST.Obj.Value.Provided(false, RawPetaformAST.Str(i))
given Conversion[InterpolatedString, RawPetaformAST] =
  RawPetaformAST.Str(_)
