#function to set the dimensions of the analysis window (XPSGUI.r) and personal settings
#XPSsettings structure
#General	       Colors	     LType	     Symbols	   SymIndx	BaseColor Comp.Color	FitColor
#
#Courier	       black	      solid	     VoidCircle	      1	 cadetblue	 grey45	 orangered
#normal	        red3	       dashed	    VoidSquare	      0	 cadetblue	 grey45	 orangered
#10	            limegreen	  dotted	    VoidTriangleUp	  2	 cadetblue	 grey45	 orangered
#1.8	           blue	       dotdash	   VoidTriangleDwn  6	 cadetblue	 grey45	 orangered
#1486.6	        magenta	    longdash 	 Diamond	         5	 cadetblue	 grey45	 orangered
#windows        orange	     twodash	   SolidCircle	     16	cadetblue	 grey45	 orangered
#personal WD    cadetblue	  F8	        SolidSquare	     15	cadetblue	 grey45	 orangered
# NA            sienna	     431313	    SolidTriangleUp  17	cadetblue	 grey45	 orangered
# NA	           darkgrey	   22848222	  SolidTriangleDwn 25	cadetblue	 grey45	 orangered
# NA	           forestgreen	12126262	  SolidDiamond	    18	cadetblue	 grey45	 orangered
# NA	           gold	       12121262	  X	               4	 cadetblue	 grey45	 orangered
# NA	           darkviolet	 12626262	  Star	            8	 cadetblue	 grey45	 orangered
# NA	           greenyellow	52721272   CrossSquare	     7	 cadetblue	 grey45	 orangered
# NA	           cyan	       B454B222	  CrossCircle	     10	cadetblue	 grey45	 orangered
# NA	           lightblue	  F313F313	  SolidDiamond	    18	cadetblue	 grey45	 orangered
# NA	           dodgerblue 	71717313	  DavidStar	       11	cadetblue	 grey45	 orangered
# NA	           deeppink3	  93213321	  SquareCross	     12	cadetblue	 grey45	 orangered
# NA	           wheat	      66116611	  SquareTriang	    14	cadetblue	 grey45	 orangered
# NA	           thistle	    23111111	  CircleCross	     13	cadetblue	 grey45	 orangered
# NA	           grey40	     222222A2	  Cross	           3	 cadetblue	 grey45	 orangered

#' @title XPSPreferences
#' @description XPSPreferences function allows selection of preferences as
#'   - plot colors, the default set of line patterns and set of symbols
#'   - the character style used in RxpsG outputs
#'   - the XPS excitatio source (Al, Mg)
#'   - the default working directory
#'   - the default operating system
#' Preferences are stored in the XPSSettings.ini file saved in the ...Library/RxpsG/extdata.
#' @examples
#' \dontrun{
#' 	XPSPreferences()
#' }
#' @export
#'


XPSPreferences <- function() {

   MatchSymbol <- function(Sym,SymIndx,ii){
            switch(Sym,
                  "VoidCircle" = {SymIndx[ii] <- 1},
                  "VoidSquare" = {SymIndx[ii] <- 0},
                  "VoidTriangleUp" = {SymIndx[ii] <- 2},
                  "VoidTriangleDwn" = {SymIndx[ii] <- 6},
                  "Diamond" = {SymIndx[ii] <- 5},
                  "SolidCircle" = {SymIndx[ii] <- 16},
                  "SolidSquare" = {SymIndx[ii] <- 15},
                  "SolidTriangleUp" = {SymIndx[ii] <- 17},
                  "SolidTriangleDwn" = {SymIndx[ii] <- 25},
                  "SolidDiamond" = {SymIndx[ii] <- 18},
                  "X" = {SymIndx[ii] <- 4},
                  "Star" = {SymIndx[ii] <- 8},
                  "CrossSquare" = {SymIndx[ii] <- 7},
                  "CrossCircle" = {SymIndx[ii] <- 10},
                  "CrossDiamond" = {SymIndx[ii] <- 9},
                  "DavidStar" = {SymIndx[ii] <- 11},
                  "SquareCross" = {SymIndx[ii] <- 12},
                  "SquareTriang" = {SymIndx[ii] <- 14},
                  "CircleCross" = {SymIndx[ii] <- 13},
                  "Cross" = {SymIndx[ii] <- 3},
                  "Bullet" = {SymIndx[ii] <- 20},
                  "FilledCircle" = {SymIndx[ii] <- 21},
                  "FilledSquare" = {SymIndx[ii] <- 22},
                  "FilledDiamond" = {SymIndx[ii] <- 23},
                  "FilledTriangleUp" = {SymIndx[ii] <- 24})
            return(SymIndx)
   }




#---variables
   #--XPSSettings is a Global variable defined in RXPSG.r
   XraySource <- c("Al", "Mg")               #X-ray source (at moment not used)
   OSList <- c("Windows", "MacOS", "Linux")#Possible operating systems
   fontPreferences <- list(font=c("Courier", "LucidaConsole", "Consolas", "SimplifiedArabicFixed", "OCRA-Extended"),   #fonnt used in the quantification table
                           style=c("normal", "italic", "oblique"),                                                      #font style
                           size=c(8, 10, 12, 14))                                                                       #font size

   Font <- XPSSettings$General[1]
   Style <- XPSSettings$General[2]
   Size <- XPSSettings$General[3]
   WinSize <- XPSSettings$General[4]
   XSource <- XPSSettings$General[5]
   Gdev <- XPSSettings$General[6]
   WorkingDir <- XPSSettings$General[7]  #personal Working Dir
   Colors <- XPSSettings$Colors
   LType <- XPSSettings$LType
   Symbols <- XPSSettings$Symbols
   BaseLineColor <- XPSSettings$BaseColor
   ComponentsColor <- XPSSettings$ComponentsColor
   FitColor <- XPSSettings$FitColor
   LType <- encodeString(as.character(LType), width=20, justify="left")
   Symbols <- encodeString(as.character(Symbols), width=20, justify="left")
   GStyleParam <- data.frame(LType=LType, Symbols=Symbols, stringsAsFactors=FALSE)

   ColNames <- names(XPSSettings)
   CLcolor <- list()  #to store widget ID
   FCcolor <- list()  #to store widget ID
   FTcolor <- list()  #to store widget ID

   CHRM <- tclVar("MonoChromeFC")
   if (XPSSettings$General[8] == "FC.PolyChrome") { CHRM <- tclVar("PolyChromeFC") }


#---GUI                       BaseLineColor ComponentsColor  FitColor

   PrefWindow <- tktoplevel()
   tkwm.title(PrefWindow,"PREFERENCES")
   tkwm.geometry(PrefWindow, "+100+50")   #position respect topleft screen corner

   MainGroup <- ttkframe(PrefWindow, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   group0 <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(group0, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

#--- widget left side

   group1 <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(group1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   frameDim <- ttklabelframe(group1, text = " WINDOW DIMENSIONS ", borderwidth=2)
   tkgrid(frameDim, row = 1, column = 1, padx = 5, pady = 3, sticky="we")
   txt <- paste("Graphic Window size : ", WinSize, sep="")
   WSizeLabel <- ttklabel(frameDim, text=txt)
   tkgrid(WSizeLabel, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
   WS <- tclVar(WinSize)
   WindowSize <- ttkscale(frameDim, from=3, to=15, value=as.numeric(WinSize), variable=WS, orient="horizontal", length=300)
   tkgrid(WindowSize, row = 2, column = 1, padx = 5, pady = 3, sticky="we")
   tkbind(WindowSize, "<ButtonRelease>", function(K){
                         WinSize <<- as.numeric(tclvalue(WS))
                         tkconfigure(WSizeLabel, text=paste("Graphical Window size: ", WinSize, sep=""))
                         WinSize <<- dev.size()*WinSize   #rescale the graphic window
                    })

   frameDev <- ttklabelframe(group1, text = "SELECT THE OPERATING SYSTEM FOR GRAPHICS", borderwidth=2)
   tkgrid(frameDev, row = 2, column = 1, padx = 5, pady = 3, sticky="we")
   OS <- tclVar("Windows")
   LL <- length(OSList)
   for(ii in 1:LL){
       SysRadio <- ttkradiobutton(frameDev, text=OSList[ii], variable=OS, value=OSList[ii],
                         command=function(){
                         if (tclvalue(OS) == "Linux") {Gdev <- "X11(type='cairo', xpos=700, ypos=5, title=' ')"}
                         if (tclvalue(OS) == "Windows") {Gdev <- "x11(xpos=700, ypos=5, title=' ')"} #top right position
                         if (tclvalue(OS) == "MacOS") {Gdev <- "quartz(title=' ')"} #quartz() doesn't allow to set the opening position
                    })
       tkgrid(SysRadio, row = ii, column = 1, padx = 5, pady = 2, sticky="w")
   }

   group2 <- ttkframe(group1, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(group2, row = 3, column = 1, padx = 0, pady = 0, sticky="w")
   frameFont <- ttklabelframe(group2, text = " FONT ", borderwidth=2)
   tkgrid(frameFont, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
   FNT <- tclVar(Font)
   LL <- length(fontPreferences$font)
   for(ii in 1:LL){
       FntRadio <- ttkradiobutton(frameFont, text=fontPreferences$font[ii], variable=FNT, value=fontPreferences$font[ii],
                         command=function(){
                         Font <<- tclvalue(FNT)
                    })
       tkgrid(FntRadio, row = ii, column = 1, padx = 5, pady = 2, sticky="w")
   }

   frameStyle <- ttklabelframe(group2, text = " STYLE ", borderwidth=2)
   tkgrid(frameStyle, row = 1, column = 2, padx = 5, pady = 3, sticky="w")
   STY <- tclVar(Style)
   LL <- length(fontPreferences$style)
   for(ii in 1:LL){
       StyleObj <- ttkradiobutton(frameStyle, text=fontPreferences$style[ii], variable=STY, value=fontPreferences$style[ii],
                         command=function(){
                         Style <<- tclvalue(STY)
                    })
       tkgrid(StyleObj, row = ii, column = 1, padx = 5, pady = 2, sticky="w")
   }

   frameSize <- ttklabelframe(group2, text = " SIZE ", borderwidth=2)
   tkgrid(frameSize, row = 1, column = 3, padx = 5, pady = 3, sticky="w")
   SZE <- tclVar(Size)
   LL <- length(fontPreferences$size)
   for(ii in 1:LL){
       SizeObj <- ttkradiobutton(frameSize, text=fontPreferences$size[ii], variable=SZE, value=fontPreferences$size[ii],
                         command=function(){
                         Size <<- as.numeric(tclvalue(SZE))
                    })
       tkgrid(SizeObj, row = ii, column = 1, padx = 5, pady = 2, sticky="w")
   }

   frameX <- ttklabelframe(group1, text = " X-SOURCE ", borderwidth=2)
   tkgrid(frameX, row = 4, column = 1, padx = 5, pady = 3, sticky="we")
   XRY <- tclVar("Al")
   if (XSource == 1254.6) {XRY <- tclVar("Mg")}
   LL <- length(fontPreferences$size)
   for(ii in 1:2){
       Xobj <- ttkradiobutton(frameX, text=XraySource[ii], variable=XRY, value=XraySource[ii],
                         command=function(){
                         if (tclvalue(XRY) == "Al") {XSource <<- 1486.6}
                         if (tclvalue(XRY) == "Mg") {XSource <<- 1254.6}
                    })
       tkgrid(Xobj, row = 1, column = ii, padx = 5, pady = 2, sticky="w")
   }

   frameWDir <- ttklabelframe(group1, text = "SELECT THE NEW WORKING DIR", borderwidth=10)
   tkgrid(frameWDir, row = 5, column = 1, padx = 5, pady = 3, sticky="we")

   WDbtn <- tkbutton(frameWDir, text=" Browse Dir ", width=15, command=function(){
                    SysName <- Sys.info()
                    SysName <- SysName[1]
                    WDir <- tk_choose.dir()
                    WorkingDir <<- paste(dirname(WDir), "/", basename(WDir), sep="") #exchanges backslash from \\ to /
                    ForbidChars <- c("-")
                    xxx <- sapply(ForbidChars, grep, x=WorkingDir)
                    xxx <- sapply(xxx, length )
                    if (sum(xxx) > 0) {
                        tkmessageBox(message="WARNING: Forbidden Character '-' in the Path or Filename. Please remove!" , title = "WARNING",  icon = "warning")
                        return()
                    }
                    setwd(WorkingDir)
                    cat("\n New Working Directory: ", WorkingDir)
                    ShortPathName <- WorkingDir
                    if (nchar(ShortPathName) > 40){   #cut workingDir to less than 40 chars
                       splitPathName <- strsplit(ShortPathName, "/")
                       LL <- length(splitPathName[[1]])
                       HeadPathName <- paste(splitPathName[[1]][1],"/", splitPathName[[1]][2], "/ ... ", sep="")
                       ShortPathName <- paste(HeadPathName, substr(splitPathName, LL-30, LL), sep="")
                    }
                    tkconfigure(dispWD, text=ShortPathName)
              })
   tkgrid(WDbtn, row = 1, column = 1, padx = 5, pady = 2, sticky="w")
   dispWD <- tklabel(frameWDir, text="W.Dir: ")
   tkgrid(dispWD, row = 2, column = 1, padx = 5, pady = 2, sticky="w")

   SetBtn <- tkbutton(group1, text=" SAVE as DEFAULT and EXIT ", command=function(){
   #--- get System info and apply correspondent XPS Settings ---
                         Ini.pthName <- system.file("extdata/XPSSettings.ini", package="RxpsG")
                         if (file.exists(Ini.pthName)) {
                             XPSSettings$General[1] <<- Font
                             XPSSettings$General[2] <<- Style
                             XPSSettings$General[3] <<- Size
                             XPSSettings$General[4] <<- WinSize
                             XPSSettings$General[5] <<- XSource
                             XPSSettings$General[6] <<- Gdev
                             XPSSettings$General[7] <<- WorkingDir     #personal Working Dir
                             XPSSettings$General[8] <<- tclvalue(CHRM) #personal Working Dir
                             for (jj in 9:20){ XPSSettings$General[jj] <<- NA }
                             XPSSettings$Colors <<- gsub("\\s", "", Colors)   #removes all the blank spaces from  Color string vector
                             XPSSettings$LType <<- gsub("\\s", "",GStyleParam$LType)     #removes all the blank spaces from  LType string vector
                             XPSSettings$Symbols <<- gsub("\\s", "",GStyleParam$Symbols) #removes all the blank spaces from  Symbols string vector
                             for(jj in 1:20){
                                 XPSSettings$SymIndx <<- MatchSymbol(XPSSettings$Symbols[jj],XPSSettings$SymIndx,jj)
                             }
                             XPSSettings$BaseColor <<- gsub("\\s", "",BaseLineColor)   #removes all the blank spaces from  Color string vector
                             XPSSettings$ComponentsColor<<- gsub("\\s", "",ComponentsColor)   #removes all the blank spaces from  Color string vector
                             XPSSettings$FitColor <<- gsub("\\s", "",FitColor)   #removes all the blank spaces from  Color string vector
                             ColNames <<- names(XPSSettings)
                             write.table(XPSSettings, file = Ini.pthName, sep=" ", eol="\n", row.names=FALSE, col.names=ColNames)
                             assign("XPSSettings", XPSSettings, envir=.GlobalEnv)
                             tkdestroy(PrefWindow)
                         } else {
                             tkmessageBox(message="ATTENTION: XPSSettings.ini file is lacking. Check the RxpsG package", title = "WARNING",icon = "warning" )
                             tkdestroy(PrefWindow)
                             return()
                         }
          })
   tkgrid(SetBtn, row = 6, column = 1, padx = 5, pady = 12, sticky="w")


#--- widget right side
   group3 <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(group3, row = 1, column = 2, padx = 0, pady = 0, sticky="w")


   frameColLinSym <- ttklabelframe(group3, text = " COLORS LINES SYMBOLS ", borderwidth=2)
   tkgrid(frameColLinSym, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#   group4 <- ttkframe(frameColLinSym, borderwidth=0, padding=c(0,0,0,0) )
#   tkgrid(group4, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   tkgrid( ttklabel(frameColLinSym, text="Double click to change color"),
           row = 1, column = 1, padx = 5, pady = 0)

   frameColors <- ttklabelframe(frameColLinSym, text = "Spectra ----------- FitComp", borderwidth=10)
   tkgrid(frameColors, row = 2, column = 1, padx = 5, pady = 5, sticky="wn")

   #building the widget to change CL colors
   for(ii in 1:20){ #column1 colors 1 - 20
       CLcolor[[ii]] <- ttklabel(frameColors, text=as.character(ii), width=6, font="Serif 8", background=Colors[ii])
       tkgrid(CLcolor[[ii]], row = ii, column = 1, padx = c(5,0), pady = 1, sticky="w")
       tkbind(CLcolor[[ii]], "<Double-1>", function(){
                    X <- as.numeric(tkwinfo("pointerx", PrefWindow))
                    Y <- tkwinfo("pointery", PrefWindow)
                    WW <- tkwinfo("containing", X, Y)
                    BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                    BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                    colIdx <- grep(BKGcolor, Colors) #
                    BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                    Colors[colIdx] <<- BKGcolor
                    tkconfigure(CLcolor[[colIdx]], background=Colors[colIdx])
                 })
   }

   BLColor <- ttklabel(frameColors, text=as.character(1), width=6, font="Serif 8", background=BaseLineColor[1])
   tkgrid(BLColor, row = 1, column = 2, padx = c(12,0), pady = 1, sticky="w")
   tkbind(BLColor, "<Double-1>", function(){
                    BaseLineColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                    BaseLineColor <<- rep(BaseLineColor[1], 20)
                    tkconfigure(BLColor, background=BaseLineColor[1])
                 })

   #If FitComp = Multicolor building the widget to change FitComp colors
   if (tclvalue(CHRM) == "PolyChromeFC"){
       for(ii in 1:20){
           FCcolor[[ii]] <- ttklabel(frameColors, text=as.character(ii), width=6, font="Serif 8", background=ComponentsColor[ii])
           tkgrid(FCcolor[[ii]], row = ii, column = 3, padx = c(12,0), pady = 1, sticky="w")
           tkbind(FCcolor[[ii]], "<Double-1>", function(){
                        X <- as.numeric(tkwinfo("pointerx", PrefWindow))
                        Y <- tkwinfo("pointery", PrefWindow)
                        WW <- tkwinfo("containing", X, Y)
                        BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                        BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                        colIdx <- grep(BKGcolor, ComponentsColor) #index of the selected color
                        BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                        ComponentsColor[colIdx] <<- BKGcolor
                        tkconfigure(FCcolor[[colIdx]], background=ComponentsColor[colIdx])
                     })
       }
   }
   #If FitComp = Singlecolor building the widget to change the Baseline FitComp and Fit colors
   if (tclvalue(CHRM) == "MonoChromeFC"){
        FCcolor <- ttklabel(frameColors, text=as.character(1), width=6, font="Serif 8", background=ComponentsColor[1])
        tkgrid(FCcolor, row = 1, column = 3, padx = c(12,0), pady = 1, sticky="w")
        tkbind(FCcolor, "<Double-1>", function(){
                         ComponentsColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                         tkconfigure(FCcolor, background=ComponentsColor[1])
                     })
   }

   #Fit Color
   FTColor <- ttklabel(frameColors, text=as.character(1), width=6, font="Serif 8", background=FitColor[1])
   tkgrid(FTColor, row = 1, column = 4, padx = c(12,0), pady = 1, sticky="w")
   tkbind(FTColor, "<Double-1>", function(){
                    FitColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                    FitColor <<- rep(FitColor[1], 20)
                    tkconfigure(FTColor, background=FitColor[1])
                 })

   MonoPoly <- c("MonoChromeFC", "PolyChromeFC")
   WW <- 0
   for(ii in 1:2){
       MonoPolyobj <- ttkradiobutton(frameColLinSym, text=MonoPoly[ii], variable=CHRM, value=MonoPoly[ii],
                    command=function(){
                       MonoPolyCol <- tclvalue(CHRM)
                       if (MonoPolyCol == "MonoChromeFC") {
                           for(ii in 1:20){ tkdestroy(FCcolor[[ii]]) }
                           FCcolor <<- ttklabel(frameColors, text=as.character(1), width=6, font="Serif 8", background=ComponentsColor[1])
                           tkgrid(FCcolor, row = 1, column = 3, padx = c(12,0), pady = 1, sticky="w")
                           tkbind(FCcolor, "<Double-1>", function(){
                                  ComponentsColor[1] <<- as.character(.Tcl('tk_chooseColor'))
                                  tkconfigure(FCcolor, background=ComponentsColor[1])
                           })
                       }
                       if (MonoPolyCol == "PolyChromeFC") {
                           tkdestroy(FCcolor)
                           for(ii in 1:20){
                               FCcolor[[ii]] <<- ttklabel(frameColors, text=as.character(ii), width=6, font="Serif 8", background=ComponentsColor[ii])
                               tkgrid(FCcolor[[ii]], row = ii, column = 3, padx = c(12,0), pady = 1, sticky="w")
                               tkbind(FCcolor[[ii]], "<Double-1>", function(){
                                      X <- as.numeric(tkwinfo("pointerx", PrefWindow))
                                      Y <- tkwinfo("pointery", PrefWindow)
                                      WW <- tkwinfo("containing", X, Y)
                                      BKGcolor <- tclvalue(tcl(WW, "cget", "-background"))
                                      BKGcolor <- paste("\\b", BKGcolor, "\\b", sep="") #to match the exact word
                                      colIdx <- grep(BKGcolor, ComponentsColor) #index of the selected color
                                      BKGcolor <- as.character(.Tcl('tk_chooseColor'))
                                      ComponentsColor[colIdx] <<- BKGcolor
                                      tkconfigure(FCcolor[[colIdx]], background=ComponentsColor[colIdx])
                               })
                           }
                       }
                 })
       tkgrid(MonoPolyobj, row = 1, column = 2, padx = c(WW, 0), pady = 5, sticky="w")
       WW <- as.numeric(tkwinfo("reqwidth",MonoPolyobj))+20
   }

   group5 <- ttklabelframe(frameColLinSym, text="LineTypes & Symbols", borderwidth=2)
   tkgrid(group5, row = 2, column = 2, padx = 0, pady = 0, sticky="w")
   DFrameTable(Data="GStyleParam", Title=" LINETYPE & SYMBOLS", # <<- needed to save modified GStyleParam
                               ColNames=c("LineType", "Symbols"),
                               RowNames="", Width=15, Env=environment(),
                               parent=group5, Row=1, Column=1)


}

