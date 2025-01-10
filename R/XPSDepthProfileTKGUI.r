# XPSDepthProfile reconstruction of Depth Profiles
#
#' @title XPSDepthProfile reconstruction of Depth Profiles
#' @description XPSDepthProfile function is used to manipulate single or
#'   multiple XPS-Samples acquired by varying the tilt angle (ARXPS) or after 
#'   sputter-etch cycles (sputter-depth-profiles)
#'   The function loads the spectra at variopus angles or at various sputter-cycles
#'   defines the Baselines and computes the element quantification.
#'   Results are automatically plotted as a function of the tilt angle or
#'   of the etch cycles
#' @examples
#' \dontrun{
#' 	XPSDepthProfile()
#' }
#' @export
#'


XPSDepthProfile <- function() {   

     Get_PE <- function(CL){
          info <- CL@Info[1]   #retrieve info containing PE value
          xxx <- strsplit(info, "Pass energy")  #extract PE value
          PE <- strsplit(xxx[[1]][2], "Iris") #PE value
          PE <- gsub(" ", "", PE[[1]][1]) #PE value
          return(as.numeric(PE))
     }


     CK.Elmts <- function(){ #extracts the name of the profiled elements
             TmpNames <- CLnames
             LL <- length(CLnames)
             ii <- 1
             jj <- 1
             while (LL > 0){
                Sym <- TmpNames[jj]
                if(Sym == "Survey" || Sym == "survey"){
                   jj <- jj+1
                } else {
                   idx1 <- which(CLnames == Sym)  #extracts indexes of all corelines having name == Sym
                   idx2 <- which(TmpNames == Sym) #extracts indexes of all corelines having name == Sym
                   CoreLineList[[ii]] <<- idx1
                   names(CoreLineList)[ii] <<- Sym
                   TmpNames <- TmpNames[-idx2]
                   ii <- ii+1
                   LL <- length(TmpNames)
                }
                if (jj > LL) {break}
             }
     }

     MK.ClCkBox <- function(){  #construct the list of checkboxes describing all the XPSSample corelines
             CL.Sym <<- names(CoreLineList)
             N.CL <<- length(CL.Sym) #Number of acquired elements
             N.Cycles <<- max(sapply(CoreLineList, function(x) length(x)))
             # (<<<===)  This operation transform a list in a matrix maintaining column names
             # length<- introduces NA if data lacking
             CoreLineList <<- lapply(CoreLineList, "length<-", N.Cycles)

             #CheckBoxGroup to slect the CL to analyze
             tkdestroy(CoreLineCK) #eliminates the blank label to insert the checkbuttons

             LL <- length(CoreLineList)

             NR <- ceiling(LL/5)   #TKcheckbox will be split in solumns of 5 elements
             kk <- 1
             for(jj in 1:NR) {
                 NN1 <- (jj-1)*5+1
                 NN2 <- (jj-1)*5+5
                 if (NN2 > LL){
                     NN2 <- LL
                 }
                 for(ii in NN1:NN2) {
                     CoreLineCK[[ii]] <<- tkcheckbutton(T1frameCoreLines, text=CL.Sym[kk], variable=CL.Sym[kk],
                                                 onvalue = CL.Sym[kk], offvalue = 0, command=function(){
                                                      for(jj in LL:1){
                                                          SelectedCL[jj] <<- tclvalue(CL.Sym[jj])
                                                          if (SelectedCL[jj] == "0") { SelectedCL <<- SelectedCL[-jj] }
                                                      }
                                                 })
                     tclvalue(CL.Sym[kk]) <- FALSE   #initial checkbutton setting
                     kk <- kk+1
                     tkgrid(CoreLineCK[[ii]], row = jj, column = ii, padx = 5, pady=5, sticky="w")
                 }
             }
     }

     MK.Baseline <- function(idx, splinePoints){
        if (BaseLine == "shirley"){BaseLine <- "Shirley"}       #different names for old/new RXPSG packages
        if (BaseLine == "2p.shirley"){BaseLine <- "2P.Shirley"} #transform to new BaseLineNames.
        if (BaseLine == "3p.shirley"){BaseLine <- "3P.Shirley"} #Exact Baseline Names required to generate the Baseline see XPSClass
        if (BaseLine == "lp.shirley"){BaseLine <- "LP.Shirley"}
        if (BaseLine == "2p.tougaard"){BaseLine <- "2P.Tougaard"}
        if (BaseLine == "3p.tougaard"){BaseLine <- "3P.Tougaard"}
        if (BaseLine == "4p.tougaard"){BaseLine <- "4P.Tougaard"}

        XPSSample[[idx]] <<- XPSsetRegionToFit(XPSSample[[idx]])
        XPSSample[[idx]] <<- XPSbaseline(XPSSample[[idx]], BaseLine, deg=0, Wgt=0, splinePoints )
        XPSSample[[idx]] <<- XPSsetRSF(XPSSample[[idx]], XPSSample[[idx]]@RSF)
     }

     ResetVars <- function(){
        #reset checkbox and radio buttons
        LL <- length(FNameList)
        for(ii in 1:LL){
            tclvalue(FNameList[ii]) <- FALSE
        }

        for(ii in 1:length(CoreLineList)){
                tkdestroy(CoreLineCK[[ii]])
        }
        CoreLineCK <<- ttklabel(T1frameCoreLines, text="            ")
        tkgrid(CoreLineCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
        tclvalue(DP) <- FALSE
        tclvalue(BL) <- FALSE
        tcl(Results, "delete", "0.0", "end")
#---
        N.XS <<- NULL
        N.CL <<- NULL
        N.Cycles <<- NULL
        CL <<- NULL
        CL.Sym <<- NULL
        Matched <<- NULL         #CoreLines present in all the XPSSample
        CLnames <<- NULL          #named of the profiled elements
        SelectedCL <<- NULL
        SelectedXPSSamp <<- list()
        XPSSample <<- NULL
        CoreLineList <<- list()   #define a list for the XPSSample corelines
        FitDone <<- FALSE
        BaseLine <<-  NULL        #by default baseline selected for BKGsubtraction
        splinePoints <<- list(x=NULL, y=NULL)
        BL.Ends <<- list(x=NULL, y=NULL)
        QntMat <<- NULL
        DPrflType <<- NULL
        TkoffAngles <<- NULL
        plot.new()

     }

#--- Variables
     activeFName <- get("activeFName", envir = .GlobalEnv)
     if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
         tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
         return()
     }
     options(warn = -1)
     N.XS <- NULL
     N.CL <- NULL
     N.Cycles <- NULL
     CL <- NULL
     CL.Sym <- NULL
     Matched <- NULL         #CoreLines present in all the XPSSample
     CLnames <- NULL          #named of the profiled elements
     SelectedCL <- NULL
     SelectedXPSSamp <- NULL
     XPSSample <- NULL
     CoreLineList <- list()   #define a list for the XPSSample corelines
     DepthPrflCK <- list()      #pointer to the XPSSamp type checkbox
     CoreLineCK <- list()       #pointer to the profiled checkbox corelines
     BLRadio <- list()       #pointer to the Baseline radiobutton
     FitDone <- FALSE
     TkoffAngles <- NULL
     BaseLine <-  NULL        #by default baseline selected for BKGsubtraction
     splinePoints <- list(x=NULL, y=NULL)
     BL.Ends <- list(x=NULL, y=NULL)
     QntMat <- NULL
     DPrflType <- NULL
     MatCol <- c("black", "red2", "limegreen","blue",
            "orange3","purple","chartreuse4","deepskyblue",
            "yellow3","orange","palegreen","dodgerblue",
            "grey78","red4","green4", "lightblue3",
            "orangered","olivedrab1","deepskyblue","rosybrown3",
            "lightseagreen","goldenrod1","olivedrab1","dodgerblue2",
            "lightskyblue3","goldenrod4","olivedrab4","dodgerblue4",
            "lightskyblue4","deeppink4","darkgreen","darkblue",   "darkorchid2","gold","springgreen","darkmagenta",
            "deepskyblue4","brown4","olivedrab","blueviolet",     "grey40","orangered","green3","blue3",
            "steelblue4","yellow","yellowgreen","turquoise3",  "plum1","magenta3", "darkturquoise")

     activeFName <- get("activeFName", envir = .GlobalEnv)
     if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
         tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
         return()
     }
     FNameList <- XPSFNameList()     #list of all loaded XPSSamples in .GlobalEnv
     LL <- length(FNameList)

#--- GUI
     DPwin <- tktoplevel()
     tkwm.title(DPwin,"XPS DEPTH PROFILE")
     tkwm.geometry(DPwin, "+100+50")   #position respect topleft screen corner
     MainGroup <- ttkframe(DPwin, borderwidth=0, padding=c(0,0,0,0) )
     tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

     DPGroup1 <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
     tkgrid(DPGroup1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

     T1frameFName <- ttklabelframe(DPGroup1, text = "Select the XPS-SAMPLES TO ANALYZE", borderwidth=5)
     tkgrid(T1frameFName, row = 1, column = 1, padx = 5, pady = 5, sticky="we")

     NCol <- ceiling(LL/5) #ii runs on the number of columns
     for(ii in 1:NCol) {   #5 elements per column
         NN <- (ii-1)*5    #jj runs on the number of column_rows
         for(jj in 1:5) {
             if((jj+NN) > LL) {break}
             SourceXS <- tkcheckbutton(T1frameFName, text=FNameList[(jj+NN)], variable=FNameList[(jj+NN)], onvalue = FNameList[(jj+NN)], offvalue = 0,
                           command=function(){
                               for(kk in LL:1){
                                  SelectedXPSSamp[kk] <<- tclvalue(FNameList[kk])
                                  if (SelectedXPSSamp[kk] == "0") { SelectedXPSSamp <<- SelectedXPSSamp[-kk] }
                               }
                           })
             tclvalue(FNameList[(jj+NN)]) <- FALSE   #initial checkbutton setting
             tkgrid(SourceXS, row = jj, column = ii, padx = 5, pady = 2, sticky="w")
         }
     }



     T1frameDPType <- ttklabelframe(DPGroup1, text = "ARXPS or Sputter Depth Profile?", borderwidth=5)
     tkgrid(T1frameDPType, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
     DP <- tclVar()   #starts with cleared buttons
     ChckBtns <- list()
     txt <- c("ARXPS", "Sputt. Dpth-Prof.")
     for(ii in 1:2){
          DepthPrflCK[[ii]] <- tkcheckbutton(T1frameDPType, text=txt[ii], variable=DP, onvalue=txt[ii], offvalue=0,
                                command=function(){
                                   DPrflType <<- tclvalue(DP)
                                   N.XS <<- length(SelectedXPSSamp)
                                   if (length(SelectedXPSSamp) == 0){
                                       tclvalue(DP) <- FALSE
                                       tkmessageBox(message="Select the XPSSample to analyze first", title="WARNING", icon="warning")
                                       return()
                                   }
                                   XPSSample <<- new("XPSSample")
                                   if (N.XS == 1) {  #In just one XPSSample all the CoreLines acquired at different tilt angles
                                      XPSSample <<- get(SelectedXPSSamp[1], envir=.GlobalEnv)
                                      assign("activeFName", SelectedXPSSamp[1], envir=.GlobalEnv)
                                      CLnames <<- XPSSample@names
                                   } else if (N.XS > 1){  #Different XPSSample for different tilt angles
                                      Filename <- NULL
                                      tmp <<- get(SelectedXPSSamp[1], envir=.GlobalEnv)   #load the active XPSSample in memory
                                      LLref <- length(tmp)
                                      for(ii in 1:(N.XS*LLref)){ #Define the new XPSSample where to save all the ARXPS data
                                          XPSSample[[ii]] <<- new("XPSCoreLine")
                                      }
                                      LL2 <- 0
                                      CLnames <<- NULL
                                      for(ii in 1:N.XS){
                                          tmp <- get(SelectedXPSSamp[ii], envir=.GlobalEnv)
                                          LL1 <- length(tmp)
                                          if (LL1 != LLref){
                                              tkmessageBox(" Acquisitions Made on Different Number of Core.Lines. \n Cannot Proceed with Depth Porofile!",
                                                             title="ERROR", icon="error")
                                              return()
                                          }
                                          for(jj in 1:LL1){
                                              XPSSample[[LL2+jj]] <<- tmp[[jj]]
                                              XPSSample[[(LL2+jj)]]@Components <<- tmp[[jj]]@Components #Fit & Components loaded if already defined
                                              XPSSample[[(LL2+jj)]]@Fit <<- tmp[[jj]]@Fit
                                          }
                                          LL2 <- LL2 + LL1
                                          Filename <- paste(Filename, tmp@Filename,",", sep="")
                                          CLnames <<- c(CLnames, tmp@names) #cannot initialize XPSSample@names to NULL
                                          XPSSample@names <<- CLnames
                                      }
                                      XPSSample@Project <<- ""
                                      XPSSample@Sample <<- dirname(tmp@Sample)
                                      XPSSample@Comments <<- paste(DPrflType, Filename, spe=" ")
                                      XPSSample@User <<- ""
                                      if (DPrflType == "ARXPS"){  #assign the filename to save the analysis in a unique datafile
                                          XPSSample@Filename <<- "ARXPS.RData"
                                          assign("activeFName", "ARXPS.RData", envir=.GlobalEnv)
                                      } else if (DPrflType == "Sputt. Dpth-Prf."){
                                          XPSSample@Filename <<- "SputtDP.RData"
                                          assign("activeFName", "SputtDP.RData", envir=.GlobalEnv)
                                      }

                                   }

                                   CK.Elmts()

                                   if (DPrflType == "ARXPS"){
                                       if (N.XS == 1){
                                           NAng <- max(sapply(CoreLineList, function(x) length(x))) #All tilt in just one XPSSample
                                       } else if (N.XS > 1){
                                           NAng <- length(SelectedXPSSamp)  #Different tilt in different XPSSamples
                                       }
                                       winTKAng <- tktoplevel()
                                       tkwm.title(winTKAng,"ARXPS TILT ANGLES")
                                       tkwm.geometry(winTKAng, "+200+200")   #position respect topleft screen corner
                                       txt <- paste("Please enter the values of the ", NAng, " Take-off Angles", sep="")
                                       FrameTKAng <- ttklabelframe(winTKAng, text=txt, borderwidth=5, padding=c(5,5,5,5))
                                       tkgrid(FrameTKAng, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

                                       AA <- list()
                                       TkOff <- list()
                                       lapply(seq(from=1, to=NAng, by=1), function(ii){
                                           tkgrid( ttklabel(FrameTKAng, text=SelectedXPSSamp[ii]),
                                                   row = ii, column = 1, padx = 5, pady = 5, sticky="w")
                                           AA[[ii]] <- tclVar("Take-off ?")
                                           TkOff[[ii]] <- ttkentry(FrameTKAng, textvariable=AA[[ii]], width=15, foreground="grey", width=7)
                                           tkbind(TkOff[[ii]], "<FocusIn>", function(K){
                                                             tclvalue(AA[[ii]]) <- ""
                                                             tkconfigure(TkOff[[ii]], foreground="red")
                                                          })
                                           tkbind(TkOff[[ii]], "<Key-Return>", function(K){
                                                             tkconfigure(TkOff[[ii]], foreground="black")
                                                             TkoffAngles[[ii]] <<- as.numeric(tclvalue(AA[[ii]]))
                                                          })
                                           tkgrid(TkOff[[ii]], row = ii, column = 2)
                                       })
                                       savexitBtn <- tkbutton(FrameTKAng, text="SAVE & EXIT", width=15, command=function(){
                                                             tkdestroy(winTKAng)
                                                          })
                                       tkgrid(savexitBtn, row = ii+2, column = 1, padx=5, pady=5, sticky="w")
                                       tkwait.window(winTKAng)
                                   }

                                   MK.ClCkBox()

                                   plot(XPSSample)
                                   for(ii in 1:6){ WidgetState(BLRadio[[ii]], "normal") }
                                   WidgetState(SelectButt, "normal")
                                   WidgetState(ConcButt, "normal")
                           })
          tclvalue(DP) <- FALSE
          tkgrid(DepthPrflCK[[ii]], row = 1, column = ii, padx = 10, pady = 2, sticky="w")
     }



     T1frameCoreLines <- ttklabelframe(DPGroup1, text = "Select the CORE LINES to analyze", borderwidth=5)
     tkgrid(T1frameCoreLines, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
     CoreLineCK <- ttklabel(T1frameCoreLines, text="            ")
     tkgrid(CoreLineCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T1frameAnalysis <- ttklabelframe(DPGroup1, text = "Fit done on ALL Core Lines?", borderwidth=5)
     tkgrid(T1frameAnalysis, row = 4, column = 1, padx = 5, pady = 5, sticky="we")
     FITDONE <- tclVar(FALSE)
     AnalCK <- tkcheckbutton(T1frameAnalysis, text="Fit Done", variable=FITDONE, onvalue=1, offvalue=0,
                                command=function(){
                                   if (tclvalue(FITDONE) == 1){
                                       FitDone <<- TRUE
                                       WidgetState(T1frameBaseline, "disabled")
                                       WidgetState(BLGroup, "disabled")
                                   } else {
                                       FitDone <<- FALSE
                                       WidgetState(T1frameBaseline, "normal")
                                       WidgetState(BLGroup, "normal")
                                   }
                                })
     tkgrid(AnalCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")


     T1frameBaseline <- ttklabelframe(DPGroup1, text = "Select the BASELINE to apply", borderwidth=5)
     tkgrid(T1frameBaseline, row = 5, column = 1, padx = 5, pady = 5, sticky="we")

     BLGroup <- ttkframe(T1frameBaseline, borderwidth=0, padding=c(0,0,0,0) )
     tkgrid(BLGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
     BL <- tclVar()
     BaseLine <- c("Linear", "Shirley", "2P.Shirley", "2P.Tougaard", "3P.Tougaard", "Spline")
     for(ii in 1:3){
         BLRadio[[ii]] <- ttkradiobutton(BLGroup, text=BaseLine[ii], variable=BL, value=BaseLine[ii],
                                 command=function(){
                                     BaseLine <<- tclvalue(BL)
                           })
         tkgrid(BLRadio[[ii]], row = 1, column = ii, padx = 2, pady=2, sticky="w")
         WidgetState(BLRadio[[ii]], "disabled")
     }
     for(ii in 4:6){
         BLRadio[[ii]] <- ttkradiobutton(BLGroup, text=BaseLine[ii], variable=BL, value=BaseLine[ii],
                                 command=function(){
                                     BaseLine <<- tclvalue(BL)
                           })
         tkgrid(BLRadio[[ii]], row = 2, column = (ii-3), padx = 2, pady=2, sticky="w")
         WidgetState(BLRadio[[ii]], "disabled")
     }
     tclvalue(BL) <- FALSE

     SelectButt <- tkbutton(T1frameBaseline, text=" SELECT/ADD BASELINE ", command=function(){
                                #--- Build data matrix for plotting
                                Matched <<- match(SelectedCL, CL.Sym) #index of the selected CL among the acquired CL
                                N.CL <<- length(Matched)
                                if (N.CL == 0) {
                                    tkmessageBox(message="Please Select the Core-Lines to Analyze First!", title="WARNING", icon="warning")
                                    return()
                                }
                                for(ii in Matched){
                                    if (hasBaseline(XPSSample[[CL.Sym[ii]]])){
                                       txt <- paste("BaseLine already defined for ", CL.Sym[ii], sep="")
                                       tkmessageBox(message=txt, title="WARNING", icon="warning")
                                    } else {
                                       idx <- CoreLineList[[ii]][1]
                                       xrange <- range(XPSSample[[idx]]@.Data[[1]]) #initialize xrange
                                       answ <- "no"
                                       while(answ == "no"){
                                          #---- Graphics: generate the data matrix (spectra only) for plotting
                                          X <- Y <- list()
                                          LL <- 0
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[[ii]][jj]
                                              X <- cbind(X, XPSSample[[idx]]@.Data[[1]])
                                              Y <- cbind(Y, XPSSample[[idx]]@.Data[[2]])
                                              xrange[1] <- max(xrange[1], range(XPSSample[[idx]]@.Data[[1]])[1])
                                              xrange[2] <- min(xrange[2], range(XPSSample[[idx]]@.Data[[1]])[2])
                                          }
                                          LL <- max(sapply(X, function(z) length(z))) #max length of X column
                                          X <- sapply(XPSSample[[idx]]@.Data[[1]], "length<-", LL)  #introduces NA if length XSamp@.Data[[1]] < LL
                                          Y <- sapply(XPSSample[[idx]]@.Data[[2]], "length<-", LL)  #introduces NA if length XSamp@.Data[[2]] < LL

                                          if (XPSSample[[idx]]@Flags[1]) xrange <- sort(xrange, decreasing=TRUE)
                                          matplot(X, Y, xlim=xrange, type="l", lty=1, col=MatCol[1:N.Cycles], main=CL.Sym[ii], cex.axis=1.25,
                                                  cex.lab=1.3, xlab=XPSSample[[idx]]@units[1], ylab=XPSSample[[idx]]@units[2])
#                                          while(length(BaseLine)==0){
#                                                txt <- paste(" Select BaseLine for Core-Line ", CL.Sym[ii],"\n Then press OK to proceed.", sep="")
#                                                tkmessageBox(message=txt, title="SELECT BASELINE", icon="info")
#                                          }
#-----
                                          cat("\n ==> Applying ", BaseLine, "BaseLine for background subtraction")
                                          if (BaseLine == "Spline") {
                                              splinePoints <<- list(x=NULL, y=NULL)
                                              txt <- "Spline background \n ==> LEFT click to set spline points; RIGHT to exit"
                                              tkmessageBox(message=txt, title="HELP INFO", icon="info")
                                              pos <- c(1,1) # only to enter in  the loop
                                              while (length(pos) > 0) {  #pos != NULL => mouse right button not pressed
                                                     pos <- locator(n=1, type="p", col="green3", cex=1.5, lwd=2, pch=16)
                                                     if (length(pos) > 0) {
                                                         splinePoints$x <<- c(splinePoints$x, pos$x)  # $x and $y must be separate to add new coord to splinePoints
                                                         splinePoints$y <<- c(splinePoints$y, pos$y)
                                                     }
                                              }
                                              decr <- FALSE #Kinetic energy set
                                              if (XPSSample[[idx]]@Flags[1] == TRUE) { decr <- TRUE }
                                              kk <- order(splinePoints$x, decreasing=decr)
                                              splinePoints$x <<- splinePoints$x[kk] #splinePoints$x in ascending order
                                              splinePoints$y <<- splinePoints$y[kk] #following same order select the correspondent splinePoints$y
                                              LL <- length(splinePoints$x)
                                              BL.Ends$x <- c(splinePoints$x[1],splinePoints$x[LL]) #set the boundaries of the baseline
                                              BL.Ends$y <- c(splinePoints$y[1],splinePoints$y[LL])
                                          } else {
                                              tkmessageBox(message="Please select the BaseLine end-Points", title="DEFINE END-POINTS", icon="info")
                                              BL.Ends <- locator(n=2, type="p", col=2, lwd=2, pch=16)
                                          }
                                          cat("\n ==> Perform Background subtraction")
                                          for(jj in 1:N.Cycles){    #given the BL.End$x limits finds the corres[pondent ordinates
                                              idx <- CoreLineList[[ii]][jj]
                                              XPSSample[[idx]]@Boundaries$x <<- BL.Ends$x
                                              kk <- findXIndex(XPSSample[[idx]]@.Data[[1]], BL.Ends$x[1])
                                              nn <- 4
                                              while( (kk-nn) <= 0){
                                                 nn <- nn-1
                                              }
                                              XPSSample[[idx]]@Boundaries$y[1] <<- mean(XPSSample[[idx]]@.Data[[2]][(kk-nn):(kk+nn)])
                                              kk <- findXIndex(XPSSample[[idx]]@.Data[[1]], BL.Ends$x[2])
                                              XPSSample[[idx]]@Boundaries$y[2] <<- mean(XPSSample[[idx]]@.Data[[2]][(kk-nn):(kk+nn)])
                                              if (BaseLine == "Spline") {   #given SplinePoints$X for the first spectrum find the SplinePoints$Y for the other spectra
                                                  Npti <- length(splinePoints$x)
                                                  for(ll in 1:Npti){
                                                      kk <- findXIndex(XPSSample[[idx]]@.Data[[1]], splinePoints$x[ll])
                                                      splinePoints$y[ll] <<- mean(XPSSample[[idx]]@.Data[[2]][(kk-1):(kk+1)])
                                                  }
                                              }
                                              MK.Baseline(idx, splinePoints)
                                          }
                                          #---- Graphics: generate the data matrix (spectra + BKGD) for plotting
                                          Colr <- NULL
                                          X <- Y <- NULL
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[[ii]][jj]
                                              X <- cbind(X, XPSSample[[idx]]@Baseline$x)
                                              Y <- cbind(Y, XPSSample[[idx]]@Baseline$y)
                                              Colr <- c(Colr, 584)  #Sienna color for the Baselines
                                          }
                                          Colr <- c(Colr, MatCol[1:N.Cycles])
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[[ii]][jj]
                                              X <- cbind(X, XPSSample[[idx]]@RegionToFit$x)
                                              Y <- cbind(Y, XPSSample[[idx]]@RegionToFit$y)
                                          }
                                          xrange <- range(XPSSample[[idx]]@RegionToFit$x)
                                          if (XPSSample[[idx]]@Flags[1]) xrange <- sort(xrange, decreasing=TRUE)
                                          matplot(X, Y, xlim=xrange, type="l", lty=1, col=Colr, main=CL.Sym[ii], cex.axis=1.25,
                                                  cex.lab=1.3, xlab=XPSSample[[idx]]@units[1], ylab=XPSSample[[idx]]@units[2])
                                          #-----

                                          answ <- tclvalue(tkmessageBox(message="BaseLine OK?", type="yesno", title="BASELINE CTRL", icon="info"))
                                          if(answ == "no"){
                                             tclvalue(BL) <- FALSE
                                             txt <- paste(" Change BaseLine for Core-Line ", CL.Sym[ii],"\n Then press OK to proceed.", sep="")
                                             tkmessageBox(message=txt, title="CHANGE BASELINE", icon="warning")
                                          }
                                       }
                                    }
#                                    BaseLine <<- NULL #activate selection BaseLine for the next Core-Line
                                }
                            })
     tkgrid(SelectButt, row = 2, column = 1, padx=5, pady=2, sticky="w")
     WidgetState(SelectButt, "disabled")

     T1frameProf <- ttklabelframe(DPGroup1, text = "Compute The Concentration Profile", borderwidth=5)
     tkgrid(T1frameProf, row = 6, column = 1, padx = 5, pady = 5, sticky="we")
     ConcButt <- tkbutton(T1frameProf, text=" CONCENTRATION PROFILE ", command=function(){
#among the elements determine the max number of acquired CL. Likely same number of CL for all elements = N Cycles etching
                                QntMat <- matrix(data=NA, ncol=N.CL, nrow=N.Cycles)
                                if (length(SelectedCL) == 0){
                                    tkmessageBox(message="Select Elements to Profile first!", title="WARNING", icon="warning")
                                    return()
                                }

                                idx <- CoreLineList[[1]][1]
                                ReferencePE <- Get_PE(XPSSample[[idx]])
                                for(jj in 1:N.Cycles){ #N.Cycles runs on the DpthProf cycles or on the ARXPS tilt angles
                                    XSampTmp <- NULL
                                    XSampTmp <- new("XPSSample")  #generate a temporary XPSSample
                                    kk <- 0
                                    Matched <<- match(SelectedCL, CL.Sym) #index of the selected CL among the acquired CL
                                    N.CL <<- length(Matched)
                                    for(ii in Matched){         #for now runs only on the selected CL
                                        idx <- CoreLineList[[ii]][jj]
                                        if (FitDone == TRUE && hasComponents(XPSSample[[idx]]) == FALSE){
                                               msg <- paste(" Attention: Fit Done on ALL CORE LINES Selected but NO Fit Found on ", XPSSample[[idx]],
                                                            "\n Cannot Proceed with Depth Profiling", sep="")
                                               tkmessageBox(message=msg, title="ERROR", icon="error")
                                               return()
                                        }
                                        if(!is.na(CoreLineList[[ii]][jj])){
                                           kk <- kk+1
                                           PE <- Get_PE(XPSSample[[idx]])
                                           if (PE != ReferencePE){
                                               msg <- paste(" Attention: Pass Energy of CoreLine ", XPSSample[[idx]]@Symbol, " = ", PE, " different from Reference PE = ", ReferencePE,
                                                            "\n Cannot compute the DepthProfile. Run the Quantification and Normalize for the different PE", sep="")
                                               tkmessageBox(message=msg, title="WARNING", icon="warning")
                                               return()
                                           }
                                           XSampTmp[[kk]] <- XPSSample[[idx]] #load only the selected coreline at etch cycle / tilt angle ii

                                        }
                                    }
#cannot load directly quantification results into QntMat because acquisition of some coreline could be stopped
#this generates a CoreLineList where some elements (corelines) are lacking and NA is inserted see ((<<<===))
                                    tmp <- XPSquantify(XSampTmp, verbose=TRUE)   #compute the quantification
                                    TotQ <- sum(unlist(sapply(tmp, function(x) x$quant)))
                                    kk <- 0
                                    for(ii in Matched){
                                        if(!is.na(CoreLineList[[ii]][jj])){
                                            kk <- kk+1
                                            QntMat[jj,kk] <- sum(unlist(tmp[[kk]]$quant))/TotQ #load quantification data into the QntMat matrix
                                        }
                                    }
                                }

                                Lgnd <- NULL
                                for(ii in Matched){
                                    Lgnd <- c(Lgnd, CL.Sym[ii])
                                }
                                X <- seq(from=1, to=N.Cycles, by=1) #make the abscissa matrix
                                X <- rep(X, N.CL)
                                X <- matrix(X, nrow=N.Cycles, ncol=N.CL)
                                Xlab <- "Etch Cycles"
                                if(DPrflType == "ARXPS"){
                                   Xlab <- "Take-off Angle (deg.)"
                                   X <- TkoffAngles
                                   X <- rep(X, N.CL)
                                   X <- matrix(X, nrow=N.Cycles, ncol=N.CL)
                                }
                                xx <- min(X)
                                yy <- 1.2*max(QntMat)
                                SymIdx <- c(19,15,17,25,18,1,0,2,6,5,4,8,7,10,9,11,12,14,13,3)

                                matplot(X, QntMat, ylim=c(0,yy), type="b", lty=1, lwd=2, pch=SymIdx, col=MatCol,
                                        cex=1.2, cex.axis=1.3, cex.lab=1.35, xlab=Xlab, ylab="Concentration (%)")
                                legend(x=xx, y=yy, xjust=0, legend=Lgnd, ncol=(N.CL+1), lty=1, pch=SymIdx,
                                        lwd=2, bty="n", col=MatCol, border=MatCol, text.col=MatCol, text.font=2)
                                colnames(QntMat) <- CL.Sym[Matched]
                                QntMat <- 100*QntMat  #matrix of concentrations in percent

                                #--- NOW build textual table of concentrations
                                txt <- ""
                                txt <- paste(txt, "\n", paste(activeFName, " Depth Profile",sep=""), "\n", sep="")
                                txt <- paste(txt, "\n", paste("Quantification: ", sep=""), "\n\n", sep="")
                                txt <- paste(txt, "            ", sep="") #space for N.Cycle or Take-off angle
                                for(ii in Matched){    #Column names
                                    txt <- paste(txt, format(paste(CL.Sym[ii], "%", sep=""), digits=2, justify="right", width=7), sep="")
                                }
                                txt <- paste(txt, "\n", sep="")
                                #separator
                                txt <- paste(txt, paste(rep("--------", (N.CL+1)), collapse=""), " \n", sep="")
                                #now concentration table
                                for(jj in 1:N.Cycles){
                                    lbl <- paste("Etch Cycle ", jj, sep="")
                                    if(DPrflType == "ARXPS"){
                                       lbl <- paste("Take-Off ", TkoffAngles[jj], ":", sep="")
                                    }
                                    txt <- paste(txt, lbl, sep="")
                                    for(ii in 1:N.CL){
                                        txt <- paste(txt, format(QntMat[jj,ii], digits=3, justify="centre", width=8), sep="")
                                    }
                                    txt <- paste(txt, " \n", sep="")
                                }
                                tkinsert(Results, "0.0", txt) #write Dpth profile values in Results
                                tkconfigure(Results, wrap="none")

                                cat("\n", txt)
                           })
     tkgrid(ConcButt, row = 1, column = 1, padx=5, pady=5, sticky="w")
     WidgetState(ConcButt, "disabled")

     ResButt <- tkbutton(DPGroup1, text="  RESET ANALYSIS  ", command=function(){
                                ResetVars()
                                LL <- length(FNameList)
                                for(ii in LL){          #build the checkboxes for XPSSample selection
                                    tclvalue(FNameList[ii]) <- FALSE   #initial cehckbutton setting
                                }
                                tclvalue(BL) <- FALSE
                                tclvalue(DP) <- FALSE
                                WidgetState(BLRadio, "normal")
                                WidgetState(SelectButt, "normal")
                                WidgetState(ConcButt, "normal")
                           })
     tkgrid(ResButt, row = 7, column = 1, padx = 5, pady = 5,  sticky="we")

     SaveExitButt <- tkbutton(DPGroup1, text=" SAVE & EXIT ", command=function(){
     	                          tkdestroy(DPwin)
                                for(jj in 1:N.Cycles){
                                    for(ii in Matched){
                                        if(DPrflType == "ARXPS"){
                                           Info <- paste("   ::: ARXPS Take-off Angle (deg.): ", TkoffAngles[jj], sep="")
                                        }
                                        if(DPrflType == "Sputt. Dpth-Prf."){
                                           Info <- paste("   ::: Sputter Depth-Profile Cycle N.", jj, sep="")
                                        }
                                        idx <- CoreLineList[[ii]][jj]
                                        XPSSample[[idx]]@Info <<- Info
                                    }
                                }
                                assign(activeFName, XPSSample, envir = .GlobalEnv)
                                XPSSaveRetrieveBkp("save")
                                options(warn = 0)
                           })
     tkgrid(SaveExitButt, row = 8, column = 1, padx = 5, pady = 5,  sticky="we")

     ExitButt <- tkbutton(DPGroup1, text=" EXIT ", command=function(){
     	                          tkdestroy(DPwin)
                                assign("activeFName", SelectedXPSSamp[1], envir = .GlobalEnv)
                                XPSSaveRetrieveBkp("save")
                                options(warn = 0)
                           })
     tkgrid(ExitButt, row = 9, column = 1, padx = 5, pady = 5,  sticky="we")

     ResFrame <- ttklabelframe(MainGroup, text = "CONCENTRATION PROFILE", borderwidth=2)
     tkgrid(ResFrame, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     Results <- tktext(ResFrame, width=35, height=27)
     tkgrid(Results, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     tkgrid.rowconfigure(ResFrame, 1, weight=4)
     addScrollbars(ResFrame, Results, type="x", Row = 1, Col = 1, Px=0, Py=0)
#     tkwait.window(DPwin)

}


