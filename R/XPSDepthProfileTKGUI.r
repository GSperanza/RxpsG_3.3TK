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
             ii <- ii-1
             NN <- 10^10 #initialize NN to a high non realistic number of acquired spectra
             for(jj in 1:ii){
                 LL <- length(na.omit(CoreLineList[[jj]]))
                 if (LL < NN) { NN <- LL }
             }
             #now eliminate surplus CoreLines for all elements CoreLineList has the same number of objects
             for(jj in 1:ii){
                 CoreLineList[[jj]] <<- CoreLineList[[jj]][1:NN]
             }
     }

     MK.ClCkBox <- function(){  #construct the list of checkboxes describing all the XPSSample corelines
             CL.Sym <<- names(CoreLineList)
             N.CL <<- length(CL.Sym) #Number of acquired elements
             N.Cycles <<- max(sapply(CoreLineList, function(x) length(x)))
             tkconfigure(SliderS, to=N.Cycles)
             # (<<<===)  This operation transform a list in a matrix maintaining column names
             # length<- introduces NA if data lacking
             CoreLineList <<- lapply(CoreLineList, "length<-", N.Cycles) # (<<<===)

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

    
     Ctrl.Edges <- function(ii, N.Cycles, BL.Ends){
        cat("\n     Core.Line", SelectedCL[ii])
        idx <- CoreLineList[[ii]][1] #it is supposed Core.Lines(same Elmnt) acquired with same Estep along the Profile
        Estp <- abs(XPSSample[[idx]]@.Data[[1]][1] - XPSSample[[idx]]@.Data[[1]][2])
        NN <- length(BL.Ends)   #BL.Ends could contain Spline points...
        BL.Ends$x[1] <- BL.Ends$x[1] + 4*Estp   #needed for the definition of the Baseline limits
        BL.Ends$x[NN] <- BL.Ends$x[NN] - 4*Estp
        for(jj in 1:N.Cycles){
            xrange <- NULL
            tmpX <- NULL
            tmpY <- NULL
            xrange[1] <- max(range(XPSSample[[idx]]@.Data[[1]]))
            xrange[2] <- min(range(XPSSample[[idx]]@.Data[[1]]))

            if (XPSSample[[idx]]@Flags[1] == FALSE){ #KE scale reverse axis and data
                XPSSample[[idx]]@.Data[[1]] <- rev(XPSSample[[idx]]@.Data[[1]])
                XPSSample[[idx]]@.Data[[2]] <- rev(XPSSample[[idx]]@.Data[[2]])
            }
            if (xrange[1] < BL.Ends$x[1]){  #High BE edge of the Core.Line
                Diff_E <- BL.Ends$x[1] - xrange[1]
                tmpX <- XPSSample[[idx]]@.Data[[1]][1] + Estp
                tmpY <- XPSSample[[idx]]@.Data[[2]][1]
                while(Diff_E > 0){
                      xrange[1] <- xrange[1] + Estp
                      tmpX <- c(tmpX, xrange[1])
                      tmpY <- c(tmpY, XPSSample[[idx]]@.Data[[2]][1])  #add constant Y values in the energies from xrange[1] to BL.Ends$x[1]
                      Diff_E <- BL.Ends$x[1] - xrange[1]
                }
                XPSSample[[idx]]@.Data[[1]] <<- c(tmpX , XPSSample[[idx]]@.Data[[1]])
                XPSSample[[idx]]@.Data[[2]] <<- c(tmpY , XPSSample[[idx]]@.Data[[2]])
            }

            if (xrange[2] > BL.Ends$x[NN]){  #Low BE edge of the Core.Line
                Diff_E <- xrange[2] - BL.Ends$x[NN]
                LL <- length(XPSSample[[idx]]@.Data[[1]])
                tmpX <- XPSSample[[idx]]@.Data[[1]][LL]
                tmpY <- XPSSample[[idx]]@.Data[[2]][LL]
                while(Diff_E > 0){
                      xrange[2] <- xrange[2] - Estp
                      tmpX <- c(tmpX, xrange[2])
                      tmpY <- c(tmpY, XPSSample[[idx]]@.Data[[2]][LL])  #add constant Y values in the energies from xrange[1] to BL.Ends$x[1]
                      Diff_E <- xrange[2] - BL.Ends$x[NN]
                }
                XPSSample[[idx]]@.Data[[1]] <<- c(XPSSample[[idx]]@.Data[[1]], tmpX)
                XPSSample[[idx]]@.Data[[2]] <<- c(XPSSample[[idx]]@.Data[[2]], tmpY)
            }
            sapply(XPSSample[[idx]]@.Data[[1]], function(z) {
                         if(is.na(z)){
                            cat("\n IS.NA_X()", idx)
                            print(XPSSample[[idx]]@.Data[[1]])
                            cat("\n Error: Core.Line Edges Control Failed: NA data found!\n Analysis Stops...")
                            return()
                         }
                  })
            sapply(XPSSample[[idx]]@.Data[[2]], function(z) {
                         if(is.na(z)){
                            cat("\n IS.NA_Y()", idx)
                            print(XPSSample[[idx]]@.Data[[2]])
                            cat("\n Error: Core.Line Edges Control Failed: NA data found!\n Analysis Stops...")
                            return()
                         }
                  })

            if (XPSSample[[idx]]@Flags[1] == FALSE){ #Go back to KE scale
                XPSSample[[idx]]@.Data[[1]] <<- rev(XPSSample[[idx]]@.Data[[1]])
                XPSSample[[idx]]@.Data[[2]] <<- rev(XPSSample[[idx]]@.Data[[2]])
            }

            if (max(XPSSample[[idx]]@.Data[[1]]) < BL.Ends$x[1] ||
                min(XPSSample[[idx]]@.Data[[1]]) > BL.Ends$x[NN]){
                cat("\n II, JJ", ii, jj)
                print(BL.Ends)
                print(range(XPSSample[[idx]]@.Data[[1]]))
                break()
            }
#cat("\n II, JJ", ii, jj, idx, length(XPSSample[[idx]]@.Data[[1]]), length(XPSSample[[idx]]@.Data[[2]]))

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
        XPSSample[[idx]] <<- XPSsetRSF(XPSSample[[idx]], CL_RSF[idx])
     }

     ResetVars <- function(){
        #reset checkbox and radio buttons
        LL <- length(FNameList)
        for(ii in 1:LL){
            tclvalue(FNameList[ii]) <- FALSE
        }
        tclvalue(DP) <- FALSE
        for(ii in 1:length(CoreLineList)){
                tkdestroy(CoreLineCK[[ii]])
        }
        CoreLineList <- list()   #define a list for the XPSSample corelines
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
        SelectedXPSSamp <<- NULL
        XPSSample <<- NULL
        CoreLineList <<- list()   #define a list for the XPSSample corelines
        BLDone <<- FALSE
        BaseLine <<-  NULL        #by default baseline selected for BKGsubtraction
        splinePoints <<- list(x=NULL, y=NULL)
        BL.Ends <<- list(x=NULL, y=NULL)
        QntMat <<- NULL
        DPrflType <<- NULL
        TkoffAngles <<- NULL
        plot.new()
     }

#--- Variables
     plot.new()
     activeFName <- get("activeFName", envir = .GlobalEnv)
     if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
         tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
         return()
     }
     options(warn = -1)
     XPSSample <- NULL
     CoreLineList <- list()   #define a list for the XPSSample corelines
     CL_RSF <- NULL
     N.XS <- NULL
     N.CL <- NULL
     N.Cycles <- NULL
     CL <- NULL
     CL.Sym <- NULL
     Matched <- NULL         #CoreLines present in all the XPSSample
     CLnames <- NULL          #named of the profiled elements
     SelectedCL <- NULL
     SelectedXPSSamp <- NULL
     DepthPrflCK <- list()      #pointer to the XPSSamp type checkbox
     CoreLineCK <- list()       #pointer to the profiled checkbox corelines
     BLRadio <- list()       #pointer to the Baseline radiobutton
     BLDone <- FALSE
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

     graph0 <- NULL
     graph1 <- NULL
     xrange <- list()
     yrange <- list()
     CX <- NULL
     LevX <- NULL

     Plot_Args <- list( x=NULL, data=NULL, PanelTitles=list(), groups=NULL,
                    layout=NULL, xlim=NULL, ylim=NULL,
                    pch=10, cex=1.3, lty="solid", lwd=1, type="l",
                    background = "transparent", col=MatCol,
                    main=list(label=NULL,cex=1.4),
                    xlab=list(label=NULL, rot=0, cex=1.2),
                    ylab=list(label=NULL, rot=90, cex=1.2),
                    zlab=NULL,
                    scales=list(cex=1.1, tck=c(1,0), alternating=c(1), tick.number=5, relation="free",
                                x=list(log=FALSE), y=list(log=FALSE), axs="i"),
                    xscale.components = xscale.components.subticks,
                    yscale.components = yscale.components.subticks,
                    las=0,
                    par.settings = list(superpose.symbol=list(pch=20,fill=MatCol), #set symbol fill colore
                                        superpose.line=list(lty=1, col=MatCol), #needed to set legend colors
                                        strip=TRUE, par.strip.text=list(cex=1.2),
                                        strip.background=list(col="grey90") ),          #lightskyblue1
                    auto.key = FALSE,
                    grid = FALSE
                  )


#---
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

     Warn <- 0
     NCol <- ceiling(LL/5) #ii runs on the number of columns
     for(ii in 1:NCol) {   #5 elements per column
         NN <- (ii-1)*5    #jj runs on the number of column_rows
         for(jj in 1:5) {
             if((jj+NN) > LL) {break}
             SourceXS <- tkcheckbutton(T1frameFName, text=FNameList[(jj+NN)], variable=FNameList[(jj+NN)], onvalue = FNameList[(jj+NN)], offvalue = 0,
                           command=function(){
                               if (Warn == 0){
                                   Warn <<- 1
#%%%                                   tkmessageBox(message="Attention: Selected XPS-Samples MUST \nhave the SAME Number of Core.Lines", title="WARNING", icon="warning")
                               }
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
                           })
          tclvalue(DP) <- FALSE
          tkgrid(DepthPrflCK[[ii]], row = 1, column = ii, padx = 10, pady = 2, sticky="w")
     }
     
     DProfHlp <- tkbutton(T1frameDPType, text="  ?  ", width=5, command=function(){
                                txt <- paste("Depth Profiles can be generated by ARXPS or etching the sample by ion gun sputtering.\n",
                                             "To start the analysis you have to define which kind of experiment was performed\n",
                                             "because selecting ARXPS take-off angles must be provided", sep="")
                                tkmessageBox(message=txt, title="HELP INFO", icon="info")
                           })
     tkgrid(DProfHlp, row = 1, column = 3, padx = 5, pady = 5,  sticky="e")


     T1frameCoreLines <- ttklabelframe(DPGroup1, text = "Select the CORE LINES to analyze", borderwidth=5)
     tkgrid(T1frameCoreLines, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
     CoreLineCK <- ttklabel(T1frameCoreLines, text="            ")
     tkgrid(CoreLineCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     T1frameAnalysis <- ttklabelframe(DPGroup1, text = "Fit done on ALL Core Lines?", borderwidth=5)
     tkgrid(T1frameAnalysis, row = 4, column = 1, padx = 5, pady = 5, sticky="we")
     BLDONE <- tclVar(FALSE)
     AnalCK <- tkcheckbutton(T1frameAnalysis, text="Baseline Defined", variable=BLDONE, onvalue=1, offvalue=0,
                                command=function(){
                                   if (tclvalue(BLDONE) == 1){
                                       BLDone <<- TRUE
                                       WidgetState(T1frameBaseline, "disabled")
                                       WidgetState(BLGroup, "disabled")
                                   } else {
                                       BLDone <<- FALSE
                                       WidgetState(T1frameBaseline, "normal")
                                       WidgetState(BLGroup, "normal")
                                   }
                                })
     tkgrid(AnalCK, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

     BL_DefHlp <- tkbutton(T1frameAnalysis, text="  ?  ", width=5, command=function(){
                                txt <- paste("The program can work on pre-analyzed samples where Baseline and Core.Line Fit are already defined.\n",
                                             "In this case select the checkbox 'BASELINE DEFINED' to avoid adding a second Baseline.\n", sep="")
                                tkmessageBox(message=txt, title="HELP INFO", icon="info")
                           })
     tkgrid(BL_DefHlp, row = 1, column = 2, padx = 5, pady = 5,  sticky="e")


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
                                if (tclvalue(BL) == "0") {
                                    tkmessageBox(message="Please Select the BaseLine for Analysis!", title="WARNING", icon="warning")
                                    return()
                                }

                                LL <- length(XPSSample)
                                CL_RSF <<- rep(0, LL) #RSF = 0 for all the Core.Line forces the call to SetRSF()
                                Matched <<- match(SelectedCL, CL.Sym) #index of the selected CL among the acquired CL
                                N.CL <<- length(Matched)
                                if (N.CL == 0) {
                                    tkmessageBox(message="Please Select the Core-Lines to Analyze First!", title="WARNING", icon="warning")
                                    return()
                                }
                                for(ii in Matched){
                                    if (hasBaseline(XPSSample[[CL.Sym[ii] ]])){
                                       txt <- paste("BaseLine already defined for ", CL.Sym[ii], sep="")
                                       tkmessageBox(message=txt, title="WARNING", icon="warning")
                                    } else {
                                       idx <- CoreLineList[[ii]][1]
                                       if (XPSSample[[idx]]@RSF == 0 || length(XPSSample[[idx]]@RSF == 0) ){
                                           XPSSample[[idx]] <<- XPSsetRSF(XPSSample[[idx]], XPSSample[[idx]]@RSF)
                                           CL_RSF[CoreLineList[[ii]] ] <<- rep(XPSSample[[idx]]@RSF, length(CoreLineList[[ii]]))
                                       }
                                       xrange <- range(XPSSample[[idx]]@.Data[[1]]) #initialize xrange
                                       answ <- "no"
                                       while(answ == "no"){
                                          #---- Graphics: generate the data matrix (spectra only) for plotting
                                          X <- Y <- list()
                                          LL <- 0
                                          xrange[1] <- -10^4
                                          xrange[2] <- 10^4
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[[ii]][jj]
                                              X[[jj]] <- XPSSample[[idx]]@.Data[[1]]
                                              Y[[jj]] <- XPSSample[[idx]]@.Data[[2]]
                                              xrange[1] <- max(xrange[1], range(XPSSample[[idx]]@.Data[[1]])[1])
                                              xrange[2] <- min(xrange[2], range(XPSSample[[idx]]@.Data[[1]])[2])
                                          }
                                          LL <- max(sapply(X, function(z) length(z))) #max length of X column

                                          X <- sapply(X, "length<-", LL)  #introduces NA if length XSamp@.Data[[1]] < LL
                                          Y <- sapply(Y, "length<-", LL)  #introduces NA if length XSamp@.Data[[2]] < LL
                                          idx <- CoreLineList[[ii]][1]
                                          #Flags controlled on the first CoreLineList[[ii]][1] holds for all others CoreLineList[[ii]][nn]
                                          if (XPSSample[[idx]]@Flags[1]) xrange <- sort(xrange, decreasing=TRUE)
                                          matplot(X, Y, xlim=xrange, type="l", lty=1, col=MatCol[1:N.Cycles], main=CL.Sym[ii], cex.axis=1.25,
                                                  cex.lab=1.3, xlab=XPSSample[[idx]]@units[1], ylab=XPSSample[[idx]]@units[2])
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
                                          LL <- length(BL.Ends$x)
                                          decr <- FALSE   #order edges on a KE scale
                                          if (XPSSample[[idx]]@Flags[1]) { decr <- TRUE }  #order edges on a BE scale
                                          idx <- order(BL.Ends$x, decreasing=FALSE)
                                          if (decr == TRUE && max(BL.Ends$x) > max(xrange)) {
                                              BL.Ends$x[1] <- max(xrange)  #set maxn BE value
                                          } else if (decr == FALSE && max(BL.Ends$x) > max(xrange)) {
                                              BL.Ends$x[LL] <- max(xrange) #set max KE value
                                          }
                                          if (decr == TRUE && min(BL.Ends$x) < min(xrange)) {
                                              BL.Ends$x[LL] <- min(xrange)  #set min BE value
                                          } else if (decr == FALSE && min(BL.Ends$x) < min(xrange)) {
                                              BL.Ends$x[1] <- min(xrange)   #set min KE value
                                          }
                                          cat("\n ==> Control Core.Lines Edges")
                                          Ctrl.Edges(ii, N.Cycles, BL.Ends)
                                          cat("\n ==> Perform Background subtraction")
                                          for(jj in 1:N.Cycles){    #given the BL.End$x limits finds the corres[pondent ordinates
                                              idx <- CoreLineList[[ii]][jj]
                                              XPSSample[[idx]]@Boundaries$x <<- BL.Ends$x
                                              if (max(XPSSample[[idx]]@.Data[[1]]) < BL.Ends$x[1] ||
                                                  min(XPSSample[[idx]]@.Data[[1]]) > BL.Ends$x[2]){
                                                  cat("\n", paste(XPSSample[[idx]]@Symbol, " of XPS-Sample ", idx, " :\n",
                                                                  "Spectral range inconsistency! Process stopped...", sep=""))
                                                  print(range(XPSSample[[idx]]@.Data[[1]]))
                                                  print(BL.Ends$x)
                                                  break()

                                              }

                                              kk <- findXIndex(XPSSample[[idx]]@.Data[[1]], BL.Ends$x[1])
                                              nn <- 4
                                              while( (kk-nn) <= 0){
                                                 nn <- nn-1
                                              }
                                              XPSSample[[idx]]@Boundaries$y[1] <<- mean(XPSSample[[idx]]@.Data[[2]][(kk-nn):(kk+nn)])
                                              kk <- findXIndex(XPSSample[[idx]]@.Data[[1]], BL.Ends$x[2])
                                              LL <- length(XPSSample[[idx]]@.Data[[1]])
                                              nn <- 4
                                              while( (kk+nn) > LL){
                                                 nn <- nn-1
                                              }
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
                                              Y <- cbind(Y, XPSSample[[idx]]@Baseline$y)
                                          }
                                          idx <- CoreLineList[[ii]][1]
                                          X <- cbind(X, XPSSample[[idx]]@Baseline$x)
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[[ii]][jj]
                                              Y <- cbind(Y, XPSSample[[idx]]@RegionToFit$y)
                                              Colr <- c(Colr, 584)  #Sienna color for the Baselines
                                          }
                                          Colr <- c(Colr, MatCol[1:N.Cycles])
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
                                WidgetState(ConcButt, "normal")
                                WidgetState(PlotButt, "normal")
                            })

     tkgrid(SelectButt, row = 2, column = 1, padx=5, pady=2, sticky="w")
     WidgetState(SelectButt, "disabled")
     
     SelBaseHlp <- tkbutton(T1frameBaseline, text="  ?  ", width=5, command=function(){
                                txt <- paste("(1) After selection of the BaseLine function, press SELECT/ADD BASELINE to automatically add the selected BaseLine\n",
                                             "    to all the Core.Lines selected for the analysis.\n",
                                             "(2) For each Core.Line the program will ask to define the BaseLine End Points clicking with the mouse\n",
                                             "    on the plotted Core.Line\n",
                                             "(3) if you are satisfied with the added BaseLines press YES otherwise NO to re-define the BaseLine End Points\n",
                                             "(4) estimation of CONCENTRATION PROFILE and SELECTION OF SPECTRA TO EXPORT need definition of the BaseLines\n", sep="")
                                tkmessageBox(message=txt, title="HELP INFO", icon="info")
                           })
     tkgrid(SelBaseHlp, row = 2, column = 2, padx = 5, pady = 5,  sticky="e")


     T1frameProf <- ttklabelframe(DPGroup1, text = "Compute the Concentration Profile", borderwidth=5)
     tkgrid(T1frameProf, row = 6, column = 1, padx = 5, pady = 5, sticky="we")
     ConcButt <- tkbutton(T1frameProf, text=" CONCENTRATION PROFILE ", command=function(){
#among the elements determine the max number of acquired CL. Likely same number of CL for all elements = N Cycles etching
                                QntMat <- matrix(data=NA, ncol=N.CL, nrow=N.Cycles)
                                if (length(SelectedCL) == 0){
                                    tkmessageBox(message="Select Elements to Profile first!", title="WARNING", icon="warning")
                                    return()
                                }

                                idx <- CoreLineList[[1]][1] #it is supposed all the Core.Lines are acquired using the same PE
                                ReferencePE <- Get_PE(XPSSample[[idx]])
                                for(jj in 1:N.Cycles){ #N.Cycles runs on the DpthProf cycles or on the ARXPS tilt angles
                                    XSampTmp <- NULL
                                    XSampTmp <- new("XPSSample")  #generate a temporary XPSSample
                                    kk <- 0
                                    Matched <<- match(SelectedCL, CL.Sym) #index of the selected CL among the acquired CL
                                    N.CL <<- length(Matched)
                                    for(ii in Matched){         #for now runs only on the selected CL
                                        idx <- CoreLineList[[ii]][jj]
                                        if (BLDone == TRUE && N.CL == 1 && hasComponents(XPSSample[[idx]]) == FALSE){
                                               txt <- paste(" Attention: Fit Done on ALL CORE LINES Selected but NO Fit Found on ", XPSSample[[idx]],
                                                            "\n Cannot Proceed with Depth Profiling", sep="")
                                               tkmessageBox(message=txt, title="ERROR", icon="error")
                                               return()
                                        }
                                        if(!is.na(CoreLineList[[ii]][jj])){
                                           kk <- kk+1
                                           PE <- Get_PE(XPSSample[[idx]])
                                           if (PE != ReferencePE){
                                               txt <- paste(" Attention: Pass Energy of CoreLine ", XPSSample[[idx]]@Symbol, " = ", PE, " different from Reference PE = ", ReferencePE,
                                                            "\n Cannot compute the DepthProfile. Run the Quantification and Normalize for the different PE", sep="")
                                               tkmessageBox(message=txt, title="WARNING", icon="warning")
                                               cat("\n Spectrum N.",idx, SelectedCL[ii])
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
                                X <- matrix(X, nrow=N.Cycles, ncol=1)
                                Xlab <- "Etch Cycles"
                                if(DPrflType == "ARXPS"){
                                   Xlab <- "Take-off Angle (deg.)"
                                   X <- unlist(TkoffAngles)
                                   X <- matrix(X, nrow=N.Cycles, ncol=1)
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
     tkgrid(ConcButt, row = 1, column = 1, padx=5, pady=5, sticky="we")
     WidgetState(ConcButt, "disabled")
     
     ConcHlp <- tkbutton(T1frameProf, text="  ?  ", width=5, command=function(){
                                txt <- paste("(1) Press CONCENTRATION PROFILE to generate the element concentration vs. sputtering cycles\n",
                                             "(2) on the top-left corner of the graphic window you can copy the plot to paste it in a document\n",
                                             "(3) concentration values are reported in the window 'CONCENTRATION PROFILE'\n", 
                                             "(4) you can copy the text using the mouse left button and paste in a document.", sep="")
                                tkmessageBox(message=txt, title="HELP INFO", icon="info")
                           })
     tkgrid(ConcHlp, row = 1, column = 2, padx = 5, pady = 5,  sticky="e")


     T1frameSpect <- ttklabelframe(DPGroup1, text = "Select Spectra to Export for Analysis", borderwidth=5)
     tkgrid(T1frameSpect, row = 7, column = 1, padx = 5, pady = 5, sticky="we")

     PlotGroup <- ttkframe(T1frameSpect, borderwidth=0, padding=c(0,0,0,0) )
     tkgrid(PlotGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

     PlotButt <- tkbutton(PlotGroup, text=" PLOT SPECTRA ", command=function(){

                                MakeXY0 <- function(ii){
                                          YY <- NULL #needed to calculate ylim of the ii Corelines
                                          for(jj in 1:N.Cycles){
                                              idx <- CoreLineList[[ii]][jj]
                                              X <<- c(X, XPSSample[[idx]]@RegionToFit$x)
                                              YY <- c(YY, XPSSample[[idx]]@RegionToFit$y)
                                              Y <<- c(Y, XPSSample[[idx]]@RegionToFit$y)
                                              LevX <<- c(LevX, rep(ii, length(XPSSample[[idx]]@RegionToFit$x))) #LevX distingish among the Core-Lines
                                              CX <<- c(CX, rep(jj, length(XPSSample[[idx]]@RegionToFit$x))) #CX distinguish among the Cycles
                                          }
                                          Plot_Args$data <<- data.frame(x = X, y = Y)
                                          LL <- length(xrange)+1  #to avoid an additional incremental index kk: MakeXY(ii,kk)
                                          xrange[[LL]] <<- range(XPSSample[[idx]]@RegionToFit$x)
                                          if (XPSSample[[idx]]@Flags[1]) xrange[[LL]] <<- sort(xrange[[LL]], decreasing=TRUE)
                                          yrange[[LL]] <<- range(sapply(YY, sapply, range))
                                }

                                X <- Y <- NULL
                                CX <<- LevX <<- NULL
                                #set Graphical parameters
                                rowXcol <- list(product= c(1, 2, 4, 6, 9, 12),
                                                nrow=c(1,1,2,2,3,3),
                                                ncol=c(1,2,2,3,3,4))
                                idx <- min(which (rowXcol$product >= min(length(SelectedCL),12)))
                                Plot_Args$layout <<- c(rowXcol$nrow[idx], rowXcol$ncol[idx])   #set the plot with N panels orgaized in $nrow rows and $ncol columns


                                Matched <<- match(SelectedCL, CL.Sym) #index of the selected CL among the acquired CL
                                N.CL <<- length(Matched)
                                if (N.CL == 0) {
                                    tkmessageBox(message="Please Select the Core-Lines to Analyze First!", title="WARNING", icon="warning")
                                    return()
                                }
                                sapply(Matched, function(ii) { MakeXY0(ii) })

                                Plot_Args$x <<- formula("y ~ x| factor(LevX, labels=SelectedCL)")
                                idx <- CoreLineList[[1]][1]   #all spectra have the same x, y units
                                Plot_Args$xlab$label <<- XPSSample[[idx]]@units[1]
                                Plot_Args$ylab$label <<- XPSSample[[idx]]@units[2]
                                Plot_Args$xlim <<- xrange
                                Plot_Args$ylim <<- yrange
                                Plot_Args$groups <<- CX

                                graph0 <<- do.call(xyplot, args = Plot_Args) #generate the plot Layer
                                plot(graph0)                                 #plot the Layer
                                WidgetState(SliderS, "normal")
                                tkfocus(SliderS)
                           })
     tkgrid(PlotButt, row = 1, column = 1, padx = 5, pady = 5,  sticky="we")
#     WidgetState(PlotButt, "disabled")
     
     SpectHlp <- tkbutton(PlotGroup, text="  ?  ", width=5, command=function(){
                                txt <- paste("(1) press PLOT SPECTRA to show all the analyzed Core.Lines along the sputtering cycles\n",
                                             "(2) with the slider bar select the cycles showing the Core.Lines to export\n",
                                             "    left mouse button and left/right keys are enabled for moving slider cursor\n",
                                             "(3) press SAVE SPECTRA to save the selected Core.Lines for further analysis", sep="")
                                tkmessageBox(message=txt, title="HELP INFO", icon="info")
                           })
     tkgrid(SpectHlp, row = 1, column = 2, padx = 5, pady = 5,  sticky="we")
     
     SSS <- N.Cycles
     if (is.null(SSS)) { SSS <- 10 }
     SLD <- tclVar()
     SliderS <- tkscale(T1frameSpect, from=1, to=SSS, tickinterval=3,
                        variable=SLD, showvalue=TRUE, orient="horizontal", length=250)
     tkbind(SliderS, "<ButtonRelease>", function(K){
          
                                MakeXY1 <- function(ii, Cycl){
                                          idx <- CoreLineList[[ii]][Cycl]
                                          X <<- c(X, XPSSample[[idx]]@RegionToFit$x)
                                          Y <<- c(Y, XPSSample[[idx]]@RegionToFit$y)
                                          LevX <<- c(LevX, rep(ii, length(XPSSample[[idx]]@RegionToFit$x))) #LevX distingish among the Core-Lines
                                          CX <<- c(CX, rep(1, length(XPSSample[[idx]]@RegionToFit$x))) #CX distinguish among the Cycles

                                          Plot_Args$data <<- data.frame(x = X, y = Y)
                                }


                                Cycl <- as.numeric(tclvalue(SLD))
                                X <- Y <- NULL
                                CX <<- LevX <<- NULL
                                #set Graphical parameters
                                rowXcol <- list(product= c(1, 2, 4, 6, 9, 12),
                                                nrow=c(1,1,2,2,3,3),
                                                ncol=c(1,2,2,3,3,4))
                                idx <- min(which (rowXcol$product >= min(length(SelectedCL),12)))
                                Plot_Args$layout <<- c(rowXcol$nrow[idx], rowXcol$ncol[idx])   #set the plot with N panels orgaized in $nrow rows and $ncol columns

                                Matched <<- match(SelectedCL, CL.Sym) #index of the selected CL among the acquired CL
                                N.CL <<- length(Matched)
                                if (N.CL == 0) {
                                    tkmessageBox(message="Please Select the Core-Lines to Analyze First!", title="WARNING", icon="warning")
                                    return()
                                }
                                sapply(Matched, function(ii) { MakeXY1(ii, Cycl) })

                                Plot_Args$x <<- formula("y ~ x| factor(LevX, labels=SelectedCL)")
                                idx <- CoreLineList[[1]][1]   #all spectra have the same x, y units
                                Plot_Args$xlab$label <<- XPSSample[[idx]]@units[1]
                                Plot_Args$ylab$label <<- XPSSample[[idx]]@units[2]
                                Plot_Args$xlim <<- xrange  #xrange, yrange are those computed to generate graph0 Layer
                                Plot_Args$ylim <<- yrange
                                Plot_Args$lwd <<- 2
                                Plot_Args$col <<- rep("red", N.CL)
                                Plot_Args$groups <<- CX

                                graph1 <<- do.call(xyplot, args = Plot_Args) #generate the plot Layer
                                plot(graph0 + graph1)
                           })
     tkgrid(SliderS, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
     WidgetState(SliderS, "disabled")

     SaveSpectButt <- tkbutton(T1frameSpect, text="STORE SPECTRA", command=function(){
                                PathName <- getwd()
                                saveFName <- get("activeFName", envir=.GlobalEnv)
                                saveFName <- unlist(strsplit(saveFName, "\\."))     #not known if extension will be present
                                saveFName <- paste(saveFName[1], ".RData", sep="")  #Compose the new FileName, adding .RData extension
                                FNameList <<- XPSFNameList()
                                XSampToSave <- NULL

                                Cycl <- as.numeric(tclvalue(SLD))
                                XSampToSave <- new("XPSSample")
                                kk <- 1
                                for(ii in Matched){  #store the selected corelines in the temporary XSampToSave
                                    idx <- CoreLineList[[ii]][Cycl]
                                    XSampToSave[[kk]] <- XPSSample[[idx]]
                                    kk <- kk+1
                                }

                                SaveWindow <- tktoplevel()
                                tkwm.title(SaveWindow,"SAVE SPECTRUM")
                                tkwm.geometry(SaveWindow, "+200+150")   #position respect topleft screen corner

                                SaveGroup <- ttkframe(SaveWindow, borderwidth=0, padding=c(0,0,0,0) )
                                tkgrid(SaveGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="news")

                                XSframe <- ttklabelframe(SaveGroup, text = "Existing XPSSample", borderwidth=5)
                                tkgrid(XSframe, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

                                XS <- tclVar()
                                XPSSample <- ttkcombobox(XSframe, width = 30, textvariable = XS, values = FNameList)
                                tkgrid(XPSSample, row = 1, column = 1, padx = 5, pady = 3)
                                tkbind(XPSSample, "<<ComboboxSelected>>", function(){
                                              saveFName <<- tclvalue(XS)
                                              saveFName <<- unlist(strsplit(saveFName, "\\."))     #not known if extension will be present
                                              saveFName <<- paste(saveFName[1], ".RData", sep="")  #Compose the new FileName, adding .RData extension
                                              tclvalue(DFN) <<- saveFName
                                       })

                                DestFrame <- ttklabelframe(SaveGroup, text = " Destination File Name ", borderwidth=2)
                                tkgrid(DestFrame, row = 3, column = 1, padx = 5, pady = 5, sticky="we")

                                DFN <- tclVar("File Name")  #sets the initial msg
                                DestFName <- ttkentry(DestFrame, textvariable=DFN, width=30, foreground="grey")
                                tkbind(DestFName, "<FocusIn>", function(K){
                                                 tclvalue(DFN) <- ""
                                                 tkconfigure(DestFName, foreground="red")
                                       })
                                tkbind(DestFName, "<Key-Return>", function(K){
                                                 tkconfigure(DestFName, foreground="black")
                                                 saveFName <<- tclvalue(DFN)
                                       })
                                tkgrid(DestFName, row = 1, column = 1, padx = 5, pady = 3)

                                StoreDataBtn <- tkbutton(SaveGroup, text="STORE SPECTRA in MEMORY", width=31, command=function(){
                                                 saveFName <<- unlist(strsplit(saveFName, "\\."))
                                                 saveFName <<- paste(saveFName[1],".RData", sep="")  #Define the Filename to be used to save the XPSSample

                                                 if (exists(saveFName) == TRUE){
                                                     txt <- paste("Do you want to append new data to existing ", saveFName, " XPSSample?", sep="")
                                                     answ <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
                                                     if (tclvalue(answ) == "yes"){
                                                         ExistXSamp <- get(saveFName, envir=.GlobalEnv)
                                                         LL <- length(ExistXSamp)
                                                         LL1 <- length(XSampToSave)
                                                         for(ii in 1:LL1){
                                                             CLnames <- names(ExistXSamp)
                                                             ExistXSamp[[(LL+ii)]] <- XSampToSave[[ii]]
                                                             ExistXSamp[[(LL+ii)]]@Symbol <- SelectedCL[ii]
                                                             names(ExistXSamp) <- c(CLnames, SelectedCL[ii])
                                                         }
                                                         XSampToSave <- ExistXSamp
                                                         XSampToSave@Comments <- c(ExistXSamp@Comments, paste("Sputtering Cycle: ", Cycl, sep=""))

                                                     } else {
                                                         tkmessageBox(message="Please change File Name to save data", title="WARNING", icon="warning")
                                                         return()
                                                     }
                                                 } else {
                                                     names(XSampToSave) <- SelectedCL
                                                     XSampToSave@Comments[1] <- paste("Profiled Core.Lines: ", paste(SelectedCL, collapse=", "), sep="")
                                                     XSampToSave@Comments[2] <- paste("Sputtering Cycle: ", Cycl, sep="")
                                                 }

                                                 XSampToSave@Filename <- saveFName #save the new FileName in the relative XPSSample slot
                                                 assign(saveFName, XSampToSave, envir=.GlobalEnv)  #change the activeFName in the .GlobalEnv
                                                 remove(XSampToSave, envir=.GlobalEnv)  #needed to get a correct UpdateXS_Tbl
                                                 UpdateXS_Tbl()
                                                 XPSSaveRetrieveBkp("save")
                                       })
                                tkgrid(StoreDataBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

                                exitBtn <- tkbutton(SaveGroup, text="  EXIT  ", width=25, command=function(){
                                                 tkdestroy(SaveWindow)
                                                 return()
                                       })
                                tkgrid(exitBtn, row = 5, column = 1, padx = 5, pady = 5, sticky="w")
                           })
     tkgrid(SaveSpectButt, row = 2, column = 2, padx = 5, pady = 5, sticky="we")


     ResButt <- tkbutton(DPGroup1, text="  RESET ANALYSIS  ", command=function(){
                                ResetVars()
                                tclvalue(BL) <- FALSE
                                tclvalue(DP) <- FALSE
                                for(ii in 1:6){
                                    WidgetState(BLRadio[[ii]], "disabled")
                                }
                                WidgetState(SelectButt, "disabled")
                                WidgetState(ConcButt, "disabled")
                                WidgetState(PlotButt, "disabled")
                                WidgetState(SliderS, "disabled")
                           })
     tkgrid(ResButt, row = 8, column = 1, padx = 5, pady = 5,  sticky="we")

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
     tkgrid(SaveExitButt, row = 9, column = 1, padx = 5, pady = 5,  sticky="we")

     ExitButt <- tkbutton(DPGroup1, text=" EXIT ", command=function(){
     	                          tkdestroy(DPwin)
                                assign("activeFName", SelectedXPSSamp[1], envir = .GlobalEnv)
                                XPSSaveRetrieveBkp("save")
                                options(warn = 0)
                           })
     tkgrid(ExitButt, row = 10, column = 1, padx = 5, pady = 5,  sticky="we")

     ResFrame <- ttklabelframe(MainGroup, text = "CONCENTRATION PROFILE", borderwidth=2)
     tkgrid(ResFrame, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
     Results <- tktext(ResFrame, width=35, height=27)
     tkgrid(Results, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
     tkgrid.rowconfigure(ResFrame, 1, weight=4)
     addScrollbars(ResFrame, Results, type="x", Row = 1, Col = 1, Px=0, Py=0)

#     tkwait.window(DPwin)

}


