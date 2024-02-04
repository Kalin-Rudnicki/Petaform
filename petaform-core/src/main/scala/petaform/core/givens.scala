package petaform.core

given Conversion[Stage1PetaformAST, Stage1PetaformAST.Obj.Value] =
  Stage1PetaformAST.Obj.Value.Provided(false, _)

given Conversion[Interpolation, Stage1PetaformAST.Obj.Value] =
  i => Stage1PetaformAST.Obj.Value.Provided(false, Stage1PetaformAST.Interp(i))
given Conversion[Interpolation, Stage1PetaformAST] =
  Stage1PetaformAST.Interp(_)

given Conversion[InterpolatedString, Stage1PetaformAST.Obj.Value] =
  i => Stage1PetaformAST.Obj.Value.Provided(false, Stage1PetaformAST.Str(i))
given Conversion[InterpolatedString, Stage1PetaformAST] =
  Stage1PetaformAST.Str(_)
