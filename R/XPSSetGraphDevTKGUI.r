#Setting the Graphic device for different Operating Systems

#' @title XPSSetGraphDev
#' @description function to select the kind of graphic device compatible
#'   with the operating system in use.
#'   Also a list of graphic formats is provided to save the content 
#'   of the current graphic device.
#' @examples
#' \dontrun{
#' 	XPSSetGraphDev()
#' }
#' @export
#'


XPSSetGraphDev <- function() {

   ChDir <- function(){
          workingDir <- getwd()
          PathName <- tk_choose.dir(default=workingDir)
          PathName <- paste(dirname(PathName), "/", basename(PathName), sep="") #changes from \\ to /
          return(PathName)
   }

   CutPathName <- function(PathName){  #taglia il pathname ad una lunghezza determinata
             splitPathName <- strsplit(PathName, "/")
             LL <- length(splitPathName[[1]])    
             headPathName <- paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="") 
             ShortPathName <- paste(headPathName, splitPathName[[1]][LL], sep="")
             ii=0
             tailPathName <- ""
             while (nchar(ShortPathName) < 25) {
                   tailPathName <- paste("/",splitPathName[[1]][LL-ii],tailPathName, sep="")
                   ShortPathName <- paste(headPathName, tailPathName, sep="") 
                   ii <- ii+1
             }       
             return(ShortPathName)
   }



#--- variables
   OSList <- c("Windows", "macOS", "Linux")
   FormatList <- c("jpeg", "png", "bmp", "tiff", "eps", "pdf")
   quartz <- NULL
   pathName <- getwd()
   XPSSettings <- get("XPSSettings", envir=.GlobalEnv)
   WinSize <- as.numeric(XPSSettings$General[4])
   if (WinSize < 3) { WinSize <- 3 }
   Gdev <- NULL

# --- Widget ---
   GDwin <- tktoplevel()
   tkwm.title(GDwin,"GRAPHIC DEVICE SETTINGS")
   tkwm.geometry(GDwin, "+100+50")   #position respect topleft screen corner

   GDgroup <- ttkframe(GDwin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(GDgroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   frame0 <- ttklabelframe(GDgroup, text = " CHANGE THE GRAPHIC WINDOW DIMENSIONS ", borderwidth=2)
   tkgrid(frame0, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

   txt <- paste("Graphic Window size : ", WinSize, sep="")
   WSvalue <- ttklabel(frame0, text=txt)
   tkgrid(WSvalue, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   WS <- tclVar(7)
   WS_Slider <- ttkscale(frame0, from=5, to=15, tickinterval=1, variable=WS,
                         orient="horizontal", showvalue=FALSE, length=280)
   tkbind(WS_Slider, "<ButtonRelease>", function(K){
                        WinSize <<- as.numeric(tclvalue(WS))
                        txt <- paste("Graphical Window size: ", WinSize, sep="")
                        tkconfigure(WSvalue, text=txt)
                        graphics.off() #switch off the graphic window
                        O_Sys <- tclvalue(OS)
                        switch (O_Sys,
                            "Linux"   = {Gdev <- "X11(type='cairo', xpos=700, ypos=20, title=' ', width=WinSize, height=WinSize )" },
                            "Windows" = {Gdev <- "x11(xpos=700, ypos=20, title=' ', width=WinSize, height=WinSize )" },
                            "macOS" = {VerMajor <- as.numeric(version[6])
                                         VerMinor <- as.numeric(version[7])
                                         if (VerMajor < 3 || (VerMajor==3 && VerMinor < 6.2)) {
                                             txt <- paste("This R version does not support quartz graphic device.\n",
                                                          "Install R.3.6.2 or a higher version.", collapse="")
                                             tkmessageBox(message=txt, type="ERROR", icon="error")
                                             return()
                                         }
                                         tkmessageBox(message="Cannot set Graphic Window Dimensions for MacOS", title="WARNING", icon="warning")
                                         import::from(grDevices, quartz)
                                         quartz(title= ' ')   #quartz() does allow setting the opening position
                                         return()
                                   })
                        XPSSettings$General[4] <<- WinSize
                  })
   tkgrid(WS_Slider, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   frame1 <- ttklabelframe(GDgroup, text = " SELECT YOUR OPERATING SYSTEM ", borderwidth=2)
   tkgrid(frame1, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
   OS_Group <- ttkframe(frame1, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(OS_Group, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   OS <- tclVar()
   LL <- length(OSList)

   for(ii in 1:LL){
       OS_Radio <- ttkradiobutton(OS_Group, text=OSList[ii], variable=OS, value=OSList[ii], command=function(){
                        O_Sys <- tclvalue(OS)
                        switch (O_Sys,
                            "Linux"   = {Gdev <- "X11(type='cairo', xpos=700, ypos=20, title=' ')" },
                            "Windows" = {Gdev <- "x11(xpos=700, ypos=20, title=' ')"},
                            "macOS" = {VerMajor <- as.numeric(version[6])
                                         VerMinor <- as.numeric(version[7])
                                         if (VerMajor < 3 || (VerMajor==3 && VerMinor < 6.2)) {
                                             txt <- paste("This R version does not support quartz graphic device.\n",
                                                          "Install R.3.6.2 or a higher version.", collapse="")
                                             tkmessageBox(message=txt, type="ERROR", icon="error")
                                             return()
                                         }
                                         import::from(grDevices, quartz)
                                         quartz(title= ' ')   #quartz() does allow setting the opening position
                                         Gdev <- "quartz(title= ' ')"
                                       })


                        XPSSettings$General[6] <<- Gdev
       })
       tkgrid(OS_Radio, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
   }


   ResetBtn <- tkbutton(frame1, text=" RESET THE GRAPHIC WINDOW ", width=27, command=function(){
                        Gdev <- XPSSettings$General[6]
                        graphics.off()
                        O_Sys <- tclvalue(OS)
                        switch (O_Sys,
                            "Linux"   = {X11(type='cairo', xpos=700, ypos=20, title= ' ') },
                            "Windows" = {x11(xpos=700, ypos=20, title= ' ')},
                            "macOS" = {VerMajor <- as.numeric(version[6])
                                         VerMinor <- as.numeric(version[7])
                                         if (VerMajor < 3 || (VerMajor==3 && VerMinor < 6.2)) {                                            
                                             txt <- paste("This R version does not support quartz graphic device.\n",
                                                          "Install R.3.6.2 or a higher version.", collapse="")
                                             tkmessageBox(message=txt, type="ERROR", icon="error")
                                             return()
                                         }
                                         import::from(grDevices, quartz)
                                         quartz(title= ' ')   #quartz() does allow setting the opening position
                                         Gdev <- "quartz(title= ' ')"
                                       })
                        tkconfigure(WS_Slider, variable=7)
                        tkconfigure(WSvalue, text=paste("Graphical Window size: ", 7, sep=""))
                        XPSSettings$General[4] <<- 7
          })
   tkgrid(ResetBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   frame2 <- ttklabelframe(GDgroup, text = " EXPORT GRAPHIC PLOT AS IMAGE ", borderwidth=2)
   tkgrid(frame2, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
   tkgrid( ttklabel(frame2, text="Select the Format to Export the Graphic  Window"),
          row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   FMT_Group <- ttkframe(frame2, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(FMT_Group, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
   FMT <- tclVar(FormatList[1])

   for(ii in 1:3){
       FMT_Radio <- ttkradiobutton(FMT_Group, text=FormatList[ii], variable=FMT, value=FormatList[ii])
       tkgrid(FMT_Radio, row = 1, column = ii, padx = 5, pady = 5, sticky="w")
       FMT_Radio <- ttkradiobutton(FMT_Group, text=FormatList[ii+3], variable=FMT, value=FormatList[ii+3])
       tkgrid(FMT_Radio, row = 2, column = ii, padx = 5, pady = 5, sticky="w")
   }

   ExportBtn <- tkbutton(frame2, text=" EXPORT TO FILE ", width=25, command=function(){
                        Format <- tclvalue(FMT)
                        PathFileName <- tclvalue(tkgetSaveFile(initialdir = getwd(), initialfile = "", title = "SAVE FILE"))
                        PathName <- dirname(PathFileName)
                        FileName <- basename(PathFileName)
                        FileName <- unlist(strsplit(FileName, "\\."))[1] #drop the extension
                        FileName <- paste(FileName, ".", Format, sep="") #reconstruct the FileName with correct extension
                        PathFileName <- paste(PathName, "/", FileName, sep="")
                        if (Format == "png") dev.print(file=PathFileName, device=png, bg="white", width=1024)
                        if (Format == "jpeg") dev.print(file=PathFileName, device=jpeg,  bg="white", width=1024)
                        if (Format == "bmp") dev.print(file=PathFileName, device=bmp,  bg="white",width=1024)
                        if (Format == "tiff") dev.print(file=PathFileName, device=tiff, bg="white", width=1024)
                        if (Format == "eps") dev.print(file=PathFileName, device=postscript, horizontal=FALSE, pointsize=1)
                        if (Format == "pdf") dev.print(file=PathFileName,device=pdf)
                        cat("\n Graphic Window Exported to File: ", PathFileName)
                        Gdev <- dev.cur()
          })
   tkgrid(ExportBtn, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

   SaveExitBtn <- tkbutton(frame2, text=" SAVE SETTINGS & EXIT ", width=25, command=function(){
   #--- get System info and apply correspondent XPS Settings ---
                        Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG")
                        if (file.exists(Ini.pthName)) {
                            ColNames <- names(XPSSettings)
                            write.table(XPSSettings, file = Ini.pthName, sep=" ", eol="\n", row.names=FALSE, col.names=ColNames)
                            assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                            tkdestroy(GDwin)
                        } else {
                            tkmessageBox(message="ATTENTION: XPSSettings.ini file is lacking. Check the RxpsG package", title="WARNING", icon="warning" )
                            tkdestroy(GDwin)
                            return()
                        }
                        assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                        tkdestroy(GDwin)
          })
   tkgrid(SaveExitBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
   ww <- as.numeric(tkwinfo("reqwidth", SaveExitBtn))

   ExitBtn <- tkbutton(frame2, text=" EXIT ", width=10, command=function(){
                        tkdestroy(GDwin)
          })
   tkgrid(ExitBtn, row = 4, column = 1, padx = c(ww+15, 5), pady = 5, sticky="w")

}


