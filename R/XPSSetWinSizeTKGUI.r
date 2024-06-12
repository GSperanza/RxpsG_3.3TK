#function to set the dimensions of the analysis window (XPSGUI.r)

#' @title XPSSetWinSize
#' @description XPSSetWinSize function to select a dimensions of the graphic
#'   window depending on the dimensions of the screen used
#' @examples
#' \dontrun{
#' 	XPSSetWinSize()
#' }
#' @export
#'


XPSSetWinSize <- function() {


   WinSize <- NULL
   WinSize <- as.numeric(XPSSettings$General[4])

   WindowS <- tktoplevel()
   tkwm.title(WindowS,"ANALYSIS WINDOW SIZE")
   tkwm.geometry(WindowS, "+100+50")   #position respect topleft screen corner

   GroupS <- ttkframe(WindowS, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(GroupS, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   FrameS <- ttklabelframe(GroupS, text = " WINDOW DIMENSIONS ", borderwidth=2)
   tkgrid(FrameS, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

   WSize <- ttklabel(FrameS, text=paste("Window Size  = ", WinSize, sep=""))
   tkgrid(WSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   SZ <- tclVar(WinSize)
   SliderS <- tkscale(FrameS, from=5, to=15, tickinterval=1, variable=SZ, showvalue=FALSE, orient="horizontal", length=250)
   tkbind(SliderS, "<ButtonRelease>", function(K){
                       WinSize <<- as.numeric(tclvalue(SZ))
                       txt <- paste("Window Size  = ", WinSize, sep="")
                       tkconfigure(WSize, text=txt)
                         graphics.off()
                         O_Sys <- unname(tolower(Sys.info()["sysname"]))
                         O_Sys <- tolower(O_Sys)
                         switch (O_Sys,
                                "linux" =   {
                                            X11(type='cairo', width=WinSize, height=WinSize,
                                                xpos=700, ypos=20, title= ' ')
                                            XPSSettings$General[4] <<- WinSize
                                            },
                                "windows" = {
                                            x11(width=WinSize, height=WinSize,
                                                xpos=700, ypos=20, title= ' ')
                                            XPSSettings$General[4] <<- WinSize
                                            },
                                "macos"  =  {
                                            tkmessageBox(message="Cannot set Quartz Window Dimensions", type="WARNING", icon="warning")
                                            WinSize <<- dev.size()
                                            }
                                      )

                    })
   tkgrid(SliderS, row = 2, column = 1, padx = 5, pady = 5, sticky="we")


   SaveBtn <- tkbutton(GroupS, text=" SET SIZE and EXIT ", width=30, command=function(){
                      assign("WinSize", WinSize, envir=.GlobalEnv)
                      tkdestroy(WindowS)
                  })
   tkgrid(SaveBtn, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   SaveDefaultBtn <- tkbutton(GroupS, text=" SET SIZE as DEFAULT and EXIT ", width=30, command=function(){
                      XPSSettings <- get("XPSSettings", envir=.GlobalEnv)   #set the new WinSize value in the XPSSettigs
                      assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                      assign("WinSize", WinSize, envir=.GlobalEnv)
                      tkdestroy(WindowS)
                  })
   tkgrid(SaveDefaultBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

   exitBtn <- tkbutton(GroupS, text=" EXIT ", width=30, command=function(){
                      tkdestroy(WindowS)
                  })
   tkgrid(exitBtn, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

}
