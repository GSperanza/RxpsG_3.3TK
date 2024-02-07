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


   winsize<-NULL
   WindowS <- tktoplevel()
   tkwm.title(WindowS,"ANALYSIS WINDOW SIZE")
   tkwm.geometry(WindowS, "+100+50")   #position respect topleft screen corner

   GroupS <- ttkframe(WindowS, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(GroupS, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   FrameS <- ttklabelframe(GroupS, text = " WINDOW DIMENSIONS ", borderwidth=2)
   tkgrid(FrameS, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

   WSize <- ttklabel(FrameS, text="Window Size  = 1.6")
   tkgrid(WSize, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   SZ <- tclVar()
   SliderS <- ttkscale(FrameS, from=1, to=3, variable=SZ, orient="horizontal", length=200)
   tkbind(SliderS, "<ButtonRelease>", function(K){
                       winsize <<- as.numeric(tclvalue(SZ))
                       txt <- paste("Window Size  = ", winsize, sep="")
                       tkconfigure(WSize, text=txt)
                  })
   tkgrid(SliderS, row = 2, column = 1, padx = 5, pady = 5, sticky="we")


   SaveBtn <- tkbutton(GroupS, text=" SET SIZE and EXIT ", width=30, command=function(){
                      assign("winsize", winsize, envir=.GlobalEnv)
                      tkdestroy(WindowS)
                  })
   tkgrid(SaveBtn, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   SaveDefaultBtn <- tkbutton(GroupS, text=" SET SIZE as DEFAULT and EXIT ", width=30, command=function(){
                      XPSSettings <- get("XPSSettings", envir=.GlobalEnv)   #set the new WinSize value in the XPSSettigs
                      XPSSettings$General[4] <- winsize
                      assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                      assign("winsize", winsize, envir=.GlobalEnv)
                      tkdestroy(WindowS)
                  })
   tkgrid(SaveDefaultBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

   exitBtn <- tkbutton(GroupS, text=" EXIT ", width=30, command=function(){
                      tkdestroy(WindowS)
                  })
   tkgrid(exitBtn, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

}
