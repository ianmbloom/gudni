{ mkDerivation, array, base, bytestring, c2hs, clock, CLUtil
, colour, containers, deepseq, diagrams-lib, directory, file-embed
, filepath, FontyFruity, half, happy, hashable, IfElse, JuicyPixels
, lens, linear, loop, mmorph, monad-memo, MonadRandom, mtl, OpenCL
, OpenGL, OpenGLRaw, parallel, parallel-io, random, sdl2, silently
, stdenv, text, time, transformers, vector
}:
mkDerivation {
  pname = "gudni";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base bytestring clock CLUtil colour containers deepseq
    diagrams-lib directory file-embed filepath FontyFruity half
    hashable JuicyPixels lens linear loop mmorph monad-memo MonadRandom
    mtl OpenCL OpenGL OpenGLRaw parallel parallel-io random sdl2 text
    time transformers vector
  ];
  libraryToolDepends = [ c2hs happy ];
  executableHaskellDepends = [
    base IfElse JuicyPixels lens MonadRandom mtl random silently vector
  ];
  description = "Haskell-centric Fast Accurate Rasterizer";
  license = stdenv.lib.licenses.bsd3;
}
