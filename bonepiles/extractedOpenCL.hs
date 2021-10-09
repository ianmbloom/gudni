
runApplicationThresholdOpenCL :: ( Show s
                                 , Model s
                                 , HasStyle s
                                 , Show (TokenOf (StyleOf s))
                                 , SpaceOf (StyleOf s) ~ SubSpace
                                 )
                              => s
                              -> IO ()
runApplicationThresholdOpenCL state =
    do (rasterizer :: RasterState) <- setupRasterizer
       startApplication rasterizer state

runApplicationDagOpenCL :: ( Show s
                           , Model s
                           , HasStyle s
                           , Show (TokenOf (StyleOf s))
                           , SpaceOf (StyleOf s) ~ SubSpace
                           )
                        => s
                        -> IO ()
runApplicationDagOpenCL state =
    do (rasterizer :: DagOpenCLState) <- setupRasterizer
       startApplication rasterizer state
