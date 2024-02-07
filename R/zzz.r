#Loads the autoscroll.tcl library to manage the scrollbars added to widgets
#run the RxpsG software

.onLoad <- function(...){
#---GlobalVar initialization     
   assign("activeFName", NULL, envir=.GlobalEnv)
   assign("activeSpectIndx", NULL, envir=.GlobalEnv)
   assign("activeSpectName", NULL, envir=.GlobalEnv)
   assign("XPSSettings", NULL, envir=.GlobalEnv)
   assign("Pkgs", NULL, envir=.GlobalEnv)
   assign("grDevices", NULL, envir=.GlobalEnv)  
   assign("quartz", NULL, envir=.GlobalEnv)
#---
}

.onAttach <- function(...) {
   addTclPath(system.file("tklibs", package="RxpsG", lib.loc=.libPaths()))
   tcl("source", system.file("tklibs", "autoscroll.tcl", package="RxpsG"))

   xps()
}
                                                                          
