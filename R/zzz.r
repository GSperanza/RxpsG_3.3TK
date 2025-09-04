#Loads the autoscroll.tcl library to manage the scrollbars added to widgets
#run the RxpsG software

.onLoad <- function(libname, pkgname){
#---GlobalVar initialization
   assign("activeFName", NULL, envir=.GlobalEnv)
   assign("activeSpectIndx", NULL, envir=.GlobalEnv)
   assign("activeSpectName", NULL, envir=.GlobalEnv)
   assign("XPSSettings", NULL, envir=.GlobalEnv)
   assign("XS_Tbl", NULL, envir=.GlobalEnv)
   assign("Pkgs", NULL, envir=.GlobalEnv)
   assign("quartz", NULL, envir=.GlobalEnv)
   assign("Gdev", NULL, envir=.GlobalEnv)
#---

   addTclPath(system.file("tklibs", package="RxpsG", lib.loc=.libPaths()))
   tcl("source", system.file("tklibs", "autoscroll.tcl", package="RxpsG"))

   xps()
}
                                                                          
