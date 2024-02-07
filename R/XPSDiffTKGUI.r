#function to differentiate XPS-Sample spectra

#' @title XPSDiff for spectra differentiation
#' @description XPSDiff function is used to compute the derivative of the XPSSpectra.
#'   The user can select the degree of differentiation. Tje result of the differentiation
#'   is oplotted together with the original data. It is possible to amplify the
#'   differentiated spectrum to be on the scale of the original spectrum.
#' @examples
#' \dontrun{
#' 	XPSDiff()
#' }
#' @export
#'


XPSDiff <- function(){

   Differ <- function(Object){
        LL <- length(Object)
        tmp <- NULL
        for(ii in 2:LL){
           tmp[ii] <- Object[ii] - Object[ii-1]
        }
        tmp[1] <- tmp[2]
        return(tmp)
   }

   BkgSubtraction <- function(data){   #linear BKG subtraction
      BackGnd <<- NULL
      LL <- length(data)
      rng <- floor(LL/15)
      if (rng<3) {rng==3}
      if (rng>30) {rng==30}
      bkg1 <- mean(data[1:rng])
      bkg2 <- mean(data[(LL-rng):LL])
      stp <- (bkg1-bkg2)/LL
      Dta_Bkg <- NULL
      for (ii in 1:LL){
          Dta_Bkg[ii] <- data[ii]-(bkg1-ii*stp)
          BackGnd[ii] <<- bkg1-ii*stp
      }
      return(Dta_Bkg)
   }

   FindNearest <- function(){
      D <- NULL
      Dmin <- ((LocPos$x-Corners$x[1])^2 + (LocPos$y-Corners$y[1])^2)^0.5  #init value
      for (ii in 1:4) {
          D[ii] <- ((LocPos$x-Corners$x[ii])^2 + (LocPos$y-Corners$y[ii])^2)^0.5  #dist P0 P1
          if(D[ii] <= Dmin){
             Dmin <- D[ii]
             idx=ii
          }
      }
      if (idx==1){
          Corners$x[1] <<- Corners$x[2] <<- LocPos$x
          Corners$y[1] <<- Corners$y[3] <<- LocPos$y
      } else if (idx==2){
          Corners$x[1] <<- Corners$x[2] <<- LocPos$x
          Corners$y[2] <<- Corners$y[4] <<- LocPos$y
      } else if (idx==3){
          Corners$x[3] <<- Corners$x[4] <<- LocPos$x
          Corners$y[1] <<- Corners$y[3] <<- LocPos$y
      } else if (idx==4){
          Corners$x[3] <<- Corners$x[4] <<- LocPos$x
          Corners$y[2] <<- Corners$y[4] <<- LocPos$y
      }
      return(Corners)
   }

   FindPattern <- function(TxtVect, Pattern){
      chrPos <- NULL
      LL <- length(TxtVect)
      for(ii in 1:LL){
          Pos <- gregexpr(pattern=Pattern,TxtVect[ii])  #the result of gregexpr is a list containing the character position of D.x x=differentiation degree
          if (Pos[[1]][1] > 0) {
              chrPos[1] <- ii
              chrPos[2] <- Pos[[1]][1]
              break
          }
      }
      return(chrPos)
   }

   MeasureMaxMinD <- function(){   # \U0394. = greek Delta used to indicate difference in Max-Min posiitions

      RefreshPlot <- function(){
           XXX <- cbind(unlist(XPSDiffted[[1]]@.Data[[1]]))  #transform the vector in a column of data
           YYY <- cbind(unlist(XPSDiffted[[1]]@.Data[[2]]))
           Xlim <- range(XXX)
           if (XPSSample[[SpectIndx]]@Flags[1]==TRUE) {
              Xlim <- rev(Xlim)  ## reverse x-axis
           }
           Ylim <- range(YYY)
           matplot(x=XXX, y=YYY, type="l", lty="solid", col="black",
                   xlim=Xlim, ylim=Ylim, xlab=XPSDiffted[[1]]@units[1], ylab="Differentiated Data [cps.]")
           return()
      }

      if(length(XPSDiffted[[1]]@.Data[[2]]) == 0) {
         tkmessageBox(message="NON-Differentiated data. Cannot compute MAX-MIN difference", title="WARNING", icon="warning")
         return()
      }
      if(length(tclvalue(CL)) == 0) {
         tkmessageBox(message="Please Select the Differentiated Core-Line", title="WARNING", icon="warning")
         return()
      }

      RefreshPlot()
      txt <- "LEFT button to set the MAX/MIN positions; RIGHT to exit \n Click near markers to modify the positions"
      tkmessageBox(message=txt , title = "WARNING",  icon = "warning")
      pos <- locator(n=2, type="p", pch=3, col="red", lwd=1.5) #first the two corners are drawn
      rect(pos$x[1], min(pos$y), pos$x[2], max(pos$y))  #marker-Corners are ordered with ymin on Left and ymax on Right
      Corners$x <<- c(pos$x[1],pos$x[1],pos$x[2],pos$x[2])
      Corners$y <<- c(pos$y[1],pos$y[2],pos$y[1],pos$y[2])
      points(Corners, type="p", pch=3, col="red", lwd=1.5)

      LocPos <<- list(x=0, y=0)
      while (length(LocPos) > 0) {  #if pos1 not NULL a mouse butto was pressed
         LocPos <<- locator(n=1, type="p", pch=3, col="red", lwd=2) #to modify the zoom limits
         if (length(LocPos$x) > 0) { #if the right mouse button NOT pressed
             FindNearest()
             if (XPSDiffted[[1]]@Flags[1]) { #Binding energy set
                 pos$x <- sort(c(Corners$x[1],Corners$x[3]), decreasing=TRUE) #pos$x in decrescent ordered => Corners$x[1]==Corners$x[2]
             } else {
                 pos$x <- sort(c(Corners$x[1],Corners$x[3]), decreasing=FALSE) #pos$x in ascending order
             }
             pos$y <- sort(c(Corners$y[1],Corners$y[2]), decreasing=FALSE)
             RefreshPlot()  #refresh graph

             rect(pos$x[1], pos$y[1], pos$x[2], pos$y[2])
             points(Corners, type="p", pch=3, col="red", lwd=1.5)
         }
      }

      MaxMinD <<- round(abs(pos$x[1]-pos$x[2]), 2)
      Info <- XPSDiffted[[1]]@Info
      S3 <<- "\U0394."  #delta symbol
      charPos <- FindPattern(XPSDiffted[[1]]@Info, "   ::: Max.Min.\U0394. = ")[1]  #charPos[1] = row index of Info where "::: Max.Min.D = " is found
      if(length(charPos) > 0){ #save the Max/Min difference in XPSSample Info
         XPSDiffted[[1]]@Info[charPos] <<- paste("   ::: Max.Min.", S3, " = ", MaxMinD, sep="") #overwrite previous MAx\Min Dist value
      } else {
         if (XPSDiffted[[1]]@Info == ""){
             nI <- 1
         } else {
             nI <- length(XPSDiffted[[1]]@Info)+1
         }
         txt <- paste("   ::: Max.Min.", S3, " = ", MaxMinD, sep="")
         XPSDiffted[[1]]@Info[nI] <<- txt
      }
      Symbol <- paste(S3, S2, DiffDeg, S1, sep="", collapse="") #compose the CoreLine Symbol  "\U0394." "D."  CLname
      #add a component to store derivative Max Min positions
      XPSDiffted[[1]] <<- XPSaddComponent(XPSDiffted[[1]], type = "Derivative")

      #Set the measured MaxMinD for the differeniated CoreLine
      XPSDiffted[[1]]@Symbol <<- Symbol # '\U0394' = Delta: the MaxMin diff was measured

      XPSDiffted[[1]]@Components[[1]]@param["mu", "start"] <<- (pos$x[1] +pos$x[2])/2
      XPSDiffted[[1]]@Components[[1]]@param["mu", "min"] <<- pos$x[1] #abscissa of Max/min position
      XPSDiffted[[1]]@Components[[1]]@param["mu", "max"] <<- pos$x[2]
      idx <- findXIndex(XPSDiffted[[1]]@.Data[[1]], pos$x[1])         #finds index corresponding to Max/min positions
      h1 <- XPSDiffted[[1]]@.Data[[2]][idx]
      idx <- findXIndex(XPSDiffted[[1]]@.Data[[1]], pos$x[2])         #finds index corresponding to Max/min positions
      h2 <- XPSDiffted[[1]]@.Data[[2]][idx]
      XPSDiffted[[1]]@Components[[1]]@param["h", "start"] <<- (h1+h2)/2
      XPSDiffted[[1]]@Components[[1]]@param["h", "min"] <<- h1 #ordinate of Max/min position
      XPSDiffted[[1]]@Components[[1]]@param["h", "max"] <<- h2
#      assign(activeFName, XPSSample, envir=.GlobalEnv)
#      assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
#      cat("\n ==> Data saved")

      plot(XPSDiffted[[1]])
      tkconfigure(DistLab, text=paste("Max Min Dist: ", MaxMinD, sep=""))
      WidgetState(SaveNewButt, "normal")
      WidgetState(OverWButt, "normal")
   }

   plotData <- function() {
      SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv) #set the activeSpectIndex to the actual CoreLine
      XXX <- cbind(unlist(XPSDiffted[[1]]@.Data[1]))
      LL <- length(XXX)
      YYY <- cbind(unlist(XPSSample[[SpectIndx]]@.Data[2]), unlist(XPSDiffted[[1]]@.Data[2]), rep(0, LL))
      Xlim <- range(XXX)
      if (XPSSample[[SpectIndx]]@Flags[1]==TRUE) {
         Xlim <- rev(Xlim)  ## reverse x-axis
      }
      Ylim <- range(YYY)
      matplot(x=XXX, y=YYY, type="l", lty=c("solid", "solid", "dashed"),lwd=c(1.75, 1, 1), col=c("black", "red3", "black"),
              xlim=Xlim, ylim=Ylim, xlab=XPSSample[[SpectIndx]]@units[1], ylab=XPSSample[[SpectIndx]]@units[2])
      return()
   }

   SaveFName <- function(saveMode){
      if(DiffDeg == 0 && length(tclvalue(DDeg))==0){
          tkmessageBox(message="Before saving data make differentiation please.", title="ERROR", icon="error")
          return()
      }

#save data in NEW CORELINE mode
      if (S2 != "" && saveMode=="NewCL") {  #"D." is present, one or more core-lines are differentiated
          Idx <- length(XPSSample)              #number of corelines +1
          Info <- XPSSample[[Idx]]@Info
          nI <- length(Info)
          N.spaces <-  gregexpr(" ", Info[nI])     #finds how many space characters are in info
          if (length(N.spaces[[1]]) != nchar(Info[nI])){  #if == FALSE if a blank raw is found
              nI <- nI +1                          #then add new Info line
          }
          Idx <- Idx+1
          XPSSample[[Idx]] <<- new("XPSCoreLine") #Add the new Coreline using the starting core-line

          if (length(tclvalue(DDeg)) > 0){         #working on previously differentiated data
              XPSSample[[Idx]] <<- XPSDiffted[[1]] #update New Coreline with dufferentiated data
#--- Cntrl if we are differentiating an already differentiated spectrum
              Info <- XPSSample[[SpectIndx]]@Info
              chrPos <- FindPattern(Info, "Diff. degree: ")
              if (length(chrPos[2]) > 0) {         #it is possible to start with a pre-differentiated CoreLine
                  Differentiated <<- XPSSample[[SpectIndx]]@.Data
                  CLname <- XPSSample[[SpectIndx]]@Symbol
                  #now set the correct S1, S2, S3 and DiffDeg values
                  S1S2S3 <- unlist(strsplit(CLname, ".", fixed=TRUE))
                  if("\U0394." %in% S1S2S3) { S3 <<- "\U0394." }
                  jj <- which(S1S2S3 == "D") # which search if "D" is present in S1S2S3. but gives FALSE when compares "D" with "DD", "DyMNN", "Cd3d"
                  if(length(jj) > 0){
                     S2 <<- "D."
                     LL <- length(S1S2S3)
                     S1 <<- paste(".", S1S2S3[(jj+2):LL], sep="", collapse="")
                     DiffDeg <<- DiffDeg + as.integer(S1S2S3[(jj+1)]) #DiffDeg set in 'Differentiation'
                     Symbol <- paste(S3, S2, DiffDeg, S1, sep="")
                  }
              } else {
                  Symbol <- paste(S3, S2, DiffDeg, ".", S1, sep="") #DiffDeg updated in 'Differentiation'
              }
              XPSSample[[Idx]]@Symbol <<- Symbol
              XPSSample@names[Idx] <<- Symbol
          }

      }

#save data in OVERWRITE mode
      if (S2 != "" && saveMode=="Overwrite") {  #"D." is present: working on previously differentiated data, and overwrite new data
#--- Cntrl if we are differentiating an already differentiated spectrum
          XPSSample[[SpectIndx]] <<- XPSDiffted[[1]] #change the original diff. data with the new differented data
          Info <- XPSSample[[SpectIndx]]@Info  #selecting the CL, XPSSample[[SpectIndx]] is loaded into the XPSDiffted[[1]]
          chrPos <- FindPattern(Info, "Diff. degree: ")
          if (length(chrPos[2]) > 0) {         #the CoreLine was pre-differentiated
              Differentiated <<- XPSSample[[SpectIndx]]@.Data
              CLname <- XPSSample[[SpectIndx]]@Symbol
              #now set the correct S1, S2, S3 and DiffDeg values
              S1S2S3 <- unlist(strsplit(CLname, ".", fixed=TRUE))
              if("\U0394." %in% S1S2S3) { S3 <<- "\U0394." }
              jj <- which(S1S2S3 == "D") # which search if "D" is present in S1S2S3. which gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
              if(length(jj) > 0){
                 S2 <<- "D."    # %in% search if "D" is present in S1S2S3. %in% gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
                 LL <- length(S1S2S3)
                 S1 <<- paste(".", S1S2S3[(jj+2):LL], sep="", collapse="")
                 DiffDeg <<- DiffDeg + as.integer(S1S2S3[(jj+1)])   #add precedent diff. degree
                 Symbol <- paste(S3, S2, DiffDeg, S1, sep="")
              }
          } else {
              Symbol <- paste(S3, S2, DiffDeg, ".", S1, sep="")  #DiffDeg updated in 'Differentiation'
          }
          Info <- XPSSample[[SpectIndx]]@Info
          chrPos <- FindPattern(Info, "   ::: Differentiated ")
          nI <- chrPos[1]                  #Overwrite previous Info
          XPSSample[[SpectIndx]]@Symbol <<- Symbol
          XPSSample@names[SpectIndx] <<- Symbol
          Idx <- SpectIndx
      }

      if (S2 == "" && saveMode=="Overwrite") { #"D." NOT present but we should work on already differentiated data
#--- Cntrl if we are differentiating an already differentiated spectrum
          XPSSample[[SpectIndx]] <<- XPSDiffted[[1]]
          Info <- XPSSample[[SpectIndx]]@Info
          chrPos <- FindPattern(Info, "Diff. degree: ")
          if (length(chrPos[2]) > 0) {         #it is possible to start with a pre-differentiated CoreLine
              Differentiated <<- XPSSample[[SpectIndx]]@.Data
              CLname <- XPSSample[[SpectIndx]]@Symbol
              #now set the correct S1, S2, S3 and DiffDeg values
              S1S2S3 <- unlist(strsplit(CLname, ".", fixed=TRUE))
              if("\U0394." %in% S1S2S3) { S3 <<- "\U0394." }
              jj <- which(S1S2S3 == "D") # which search if "D" is present in S1S2S3. which gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
              if(length(jj) > 0){
                 S2 <<- "D."    # %in% search if "D" is present in S1S2S3. %in% gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
                 LL <- length(S1S2S3)
                 S1 <<- paste(".", S1S2S3[(jj+2):LL], sep="", collapse="")
                 DiffDeg <<- DiffDeg + as.integer(S1S2S3[(jj+1)])   #add precedent diff. degree
              } else {
                 tkmessageBox(message="ERROR: Wrong Data Information", title="ERROR", icon="error")
                 return()
              }
              Symbol <- paste(S3, S2, DiffDeg, S1, sep="") # '\U0394.' = Delta representing the MaxMin diff
              XPSSample[[SpectIndx]]@Symbol <<- Symbol
              XPSSample@names[SpectIndx] <<- Symbol
              Idx <- SpectIndx
         }
      }

      if (DiffDeg == 1) Description <- "First Derivative"
      if (DiffDeg == 2) Description <- "Second Derivative"
      if (DiffDeg == 3) Description <- "Third Derivative"
      if (DiffDeg == 4) Description <- "Forth Derivative"
      if (DiffDeg == 5) Description <- "Fifth Derivative"

      if (length(XPSSample[[Idx]]@Components) > 0){  #Derivative Max/Min measured
          XPSSample[[Idx]]@Components[[1]]@description <<- Description
      }
      Info[nI] <- paste("   ::: Differentiated ", Symbol, ": Diff. degree: ", DiffDeg, sep="") #Update Differentiation information
      XPSSample[[Idx]]@Info <<- Info
      assign(activeFName, XPSSample,envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
      assign("activeSpectName", Symbol,.GlobalEnv) #set the activeSpectral name to the actual CoreLine name
      assign("activeSpectIndx", Idx,.GlobalEnv) #set the activeSpectral name to the actual CoreLine name
      cat("\n ==> Data Saved")
      WidgetState(SaveNewButt, "disabled")
      WidgetState(OverWButt, "disabled")
      SpectList <<- XPSSpectList(activeFName) #Update Core-Line combobox
      tkconfigure(D1CoreLine, values=SpectList)
      return(Idx)
   }


#--- Variables
    plot.new()
    if (length(activeFName)==0){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
    }

#    activeFName <- get("activeFName", envir=.GlobalEnv)  #cload the XPSSample name (string)
    XPSSample <- get(activeFName, envir=.GlobalEnv)  #load the active XPSSample (data)
    FNameList <- XPSFNameList()                  #list of loaded XPSSamples
    SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv) #load the index of the active CoreLine
    SpectName <- get("activeSpectName", envir=.GlobalEnv) #namer of the active CoreLine
    SpectList <- XPSSpectList(activeFName)       #list of CoreLine spectra belonging to XPSSample XPSSample
    XPSDiffted <- new("XPSSample")
    Differentiated <- NULL
    DiffBkp <- NULL
    CLBkp <- NULL
    BackGnd <- NULL
    Corners <- list(x=NULL, y=NULL)
    LocPos <- list()
    MaxMinD <- NULL
    DiffDeg <- 0
    S1 <- ""   #to store the 'XPSSample@Symbol'
    S2 <- ""   #to store 'D.x' if differentiation was performed
    S3 <- ""   #to store '/U0395' if Max/Min diff was done


#----- Main Panel

    DiffWindow <- tktoplevel()
    tkwm.title(DiffWindow,"DATA DIFFERENTIATION")
    tkwm.geometry(DiffWindow, "+100+50")

    mainFrame <- ttklabelframe(DiffWindow, text="Differentiate", borderwidth=2, padding=c(5,5,5,5) )
    tkgrid(mainFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="w")


# --- Line 1 ---
    D1Group1 <- ttkframe(mainFrame, borderwidth=0, padding=c(0,0,0,0) )
    tkgrid(D1Group1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

    D1frame1 <- ttklabelframe(D1Group1, text="Select XPS Sample", borderwidth=2, padding=c(5,5,5,5) )
    tkgrid(D1frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
    XS <- tclVar(activeFName)
    D1XpsSpect <- ttkcombobox(D1frame1, width = 20, textvariable = XS, values = FNameList)
    tkgrid(D1XpsSpect, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
    tkbind(D1XpsSpect, "<<ComboboxSelected>>", function(){
                        activeFName <<- tclvalue(XS)
                        XPSSample <<- get(activeFName,envir=.GlobalEnv)  #load the XPSSample
                        assign("activeFName", activeFName, envir=.GlobalEnv)
                        SpectList <<- XPSSpectList(activeFName)
                        SpectIndx <<- 1
                        tkconfigure(D1CoreLine, values=SpectList)
                   })
#    D1XpsSpect <- gcombobox(FNameList, selected=-1, editable=FALSE, handler=function(h,...){
#                           makeCombo()
#                           tkconfigure(DistLab, text="Max Min Dist: ")
#                 }, container=D1frame1)

    D1frame2 <- ttklabelframe(D1Group1, text="Select Coreline", borderwidth=2, padding=c(5,5,5,5) )
    tkgrid(D1frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
    CL <- tclVar()
    D1CoreLine <- ttkcombobox(D1frame2, width = 20, textvariable = CL, values = SpectList)
    tkgrid(D1CoreLine, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
    tkbind(D1CoreLine, "<<ComboboxSelected>>", function(){
                        XPSCLname <- tclvalue(CL)
                        tmp <- unlist(strsplit(XPSCLname, "\\."))   #skip the ".NUMBER" at beginning CoreLine name
                        SpectIndx <<- as.integer(tmp[1])
                        if (length(XPSSample[[SpectIndx]]@Components) > 0){
                            txt <- paste("Found Fit in Core-Line ", XPSCLname, "\n",
                                         "Analysis will be overwritten! Do you want to Proceed? \n",
                                         "Otherwise: \n",
                                         "Please Make a Copy of ", XPSCLname, "Using Processing Core-Line Option \n",
                                         "Eliminate the Analysis using the Reset Analysis Option \n",
                                         "Restart the Differentiation on the cleaned ", XPSCLname, " Core-Line", sep="")
                            answ <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
                            if (tclvalue(answ) == "no") { return() }
                        }

                        LL1 <- nchar(tmp[1])+2   # +2: substr(x,start,stop) start includes the character at 'start'
                        LL2 <- nchar(XPSCLname)
                        S1 <<- substr(XPSCLname, start=LL1, stop=LL2) #In case of XPSCLname="4.D1.C1s" SpectName must be "D1.C1s"
                        S3 <<- S2 <<- ""
                        Info <- XPSSample[[SpectIndx]]@Info
                        chrPos <- FindPattern(Info, "Diff. degree: ")
                        if (length(chrPos[2]) > 0) {         #it is possible to start with a pre-differentiated CoreLine
                            Differentiated <<- XPSSample[[SpectIndx]]@.Data
                            S1S2S3 <- unlist(strsplit(S1, ".", fixed=TRUE))
                            if("\U0394." %in% S1S2S3) { S3 <<- "\U0394." }
                            jj <- which(S1S2S3 == "D") # which search if "D" is present in S1S2S3. which gives FALSE when compares "D" with "DD", "DyMNN", "CdMNN"
                            if(length(jj) > 0){
                               S2 <<- "D."
                               LL <- length(S1S2S3)
                               S1 <<- paste(".", S1S2S3[(jj+2):LL], sep="", collapse="")
#                               DiffDeg <<- DiffDeg + as.integer(S1S2S3[(jj+1)]) #DiffDeg set in 'Differentiation'
                               DiffDeg <<- as.integer(S1S2S3[(jj+1)]) #DiffDeg set in 'Differentiation'
                            }
                        }
                        XPSDiffted[[1]] <<- XPSSample[[SpectIndx]]
                        assign("activeSpectName", S1, envir=.GlobalEnv)
                        assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
                        plot(XPSSample[[SpectIndx]])
                        WidgetState(DiffDegree, "normal")
                        CLBkp <<- XPSSample[[SpectIndx]]         #backUp original data
                   })

    D1frame3 <- ttklabelframe(D1Group1, text="Update File Lists", borderwidth=2, padding=c(5,5,5,5) )
    tkgrid(D1frame3, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
    UpdateButt <- tkbutton(D1frame3, text="  UPDATE  ", width=20, command=function(){
                        DiffBkp <<- NULL
                        BackGnd <<- NULL
                        XPSDiffted[[1]] <<- NULL
                        Differentiated <<- NULL
                        DiffDeg <<- 0
                        S1 <<- S2 <<- S3 <<- ""
                        tclvalue(DDeg) <- ""
                        tclvalue(AmpliDeg) <- ""
                        tclvalue(XS) <- ""
                        tclvalue(CL) <- ""
                        WidgetState(DiffButt, "disabled")
                        WidgetState(ResButt, "disabled")
                        WidgetState(SaveNewButt, "disabled")
                        WidgetState(OverWButt, "disabled")
                        FNameList <- XPSFNameList()                  #list of loaded XPSSamples
                        tkconfigure(D1XpsSpect, values=FNameList)
                        cat("\n ==> XPS Sample List Updated")
                   })
    tkgrid(UpdateButt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")


# --- Line 2 ---
    D2frame1 <- ttklabelframe(mainFrame, text="Select Differentiation Degree", borderwidth=2, padding=c(5,5,5,5) )
    tkgrid(D2frame1, row = 2, column = 1, padx = 5, pady = 5, sticky="we")
    DDeg <- tclVar()
    DiffDegree <- ttkcombobox(D2frame1, width = 10, textvariable = DDeg, values = c(1,2,3,4,5))
    tkgrid(DiffDegree, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
    tkbind(DiffDegree, "<<ComboboxSelected>>", function(){
                        WidgetState(DiffButt, "normal")
                        WidgetState(ResButt, "normal")
                 })

    DNeg <- tclVar(FALSE)
    Negative <- tkcheckbutton(D2frame1, text="Negative", variable=DNeg, onvalue = 1, offvalue = 0,
                           command=function(){
                              XX <- tclvalue(DNeg)
                              if(length(Differentiated[[2]]) > 0){
                                 if (XX=="1") { Differentiated[[2]] <<- -Differentiated[[2]] }
                                 if (XX=="0") {
                                     Differentiated[[2]] <<- DiffBkp[[2]]
                                     XX <- tclvalue(AmpliDeg)
                                     if (XX != "") { Differentiated[[2]] <<- as.numeric(XX)*Differentiated[[2]] }
                                 }
                                 XPSDiffted[[1]]@.Data[[2]] <<- Differentiated[[2]]
                                 plotData()
                              }

                 })
     tkgrid(Negative, row = 1, column = 2, padx = 5, pady=5, sticky="w")

     DiffButt <- tkbutton(D2frame1, text=" Differentiate ", width=20, command=function(){
                        Ndiff <- as.numeric(tclvalue(DDeg))
                        if (length(Ndiff) == 0){
                            tkmessageBox(message="Please Select the Differentiation Degree", title="Warning", icon="warning")
                            return()
                        }
                        SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv) #set the activeSpectIndex to the actual CoreLine
                        XPSDiffted[[1]] <<- XPSSample[[SpectIndx]]
                        Differentiated <<- XPSSample[[SpectIndx]]@.Data
                        for(ii in 1:Ndiff){
                            Differentiated[[2]] <<- Differ(Differentiated[[2]])
                        }
                        if (tclvalue(DNeg) == "1") { Differentiated[[2]] <- -Differentiated[[2]] }
                        XPSDiffted[[1]]@.Data[[2]] <<- Differentiated[[2]]
                        #add Region to Fit and Baseline to correctly plot data
                        XPSDiffted[[1]]@RegionToFit$x <<- XPSSample[[SpectIndx]]@.Data[[1]]
                        XPSDiffted[[1]]@RegionToFit$y <<- Differentiated[[2]]
                        XPSDiffted[[1]]@Baseline[["baseline"]] <<- new("baseline") #defines a new Baseline component of class baseline and package attribute .GlobalEnv
                        XPSDiffted[[1]]@Baseline$x  <<- XPSSample[[SpectIndx]]@.Data[[1]]
                        XPSDiffted[[1]]@Baseline$y  <<- rep(0, length(XPSSample[[SpectIndx]]@.Data[[1]])) #defines a zero-Baseline
                        XPSDiffted[[1]]@Baseline$type  <<- "linear"
                        S2 <<- "D."
                        DiffDeg <<- Ndiff
                        DiffBkp <<- Differentiated
#                        BkgSubtraction(XPSSample[[SpectIndx]]@.Data[[2]])  #plotData needs the backgrnd to be defined
                        plotData()
                        WidgetState(SaveNewButt, "normal")
                        WidgetState(OverWButt, "normal")

                 })
     tkgrid(DiffButt, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

     ResButt <- tkbutton(D2frame1, text=" RESET ", width=15, command=function(){
                        SpectIndx <<- grep(S1, names(XPSSample))  #restore original selected CL index
                        DiffBkp <<- NULL
                        BackGnd <<- NULL
                        XPSDiffted[[1]] <- NULL
                        Differentiated <<- NULL
                        DiffDeg <<- 0
                        S1 <<- S2 <<- S3 <<- ""
                        tclvalue(DDeg) <- ""
                        tclvalue(AmpliDeg) <- ""
                        tclvalue(XS) <- ""
                        tclvalue(CL) <- ""
                        tclvalue(DNeg) <- 0
                        XPSSample[[SpectIndx]] <<- CLBkp
                        plot(XPSSample[[SpectIndx]])
                        WidgetState(DiffButt, "disabled")
                        WidgetState(ResButt, "disabled")
                        WidgetState(SaveNewButt, "disabled")
                        WidgetState(OverWButt, "disabled")

                 })
     tkgrid(ResButt, row = 1, column = 4, padx = 4, pady = 5, sticky="w")


# --- Line 3 ---
    D3frame1 <- ttklabelframe(mainFrame, text="Amplify Diff. Data", borderwidth=2, padding=c(5,5,5,5) )
    tkgrid(D3frame1, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
    AmpliDeg <- tclVar("10,50, 100...")  #sets the initial msg
    AmpliDiff <- ttkentry(D3frame1, textvariable=AmpliDeg, foreground="grey")
    tkbind(AmpliDiff, "<FocusIn>", function(K){
                        tkconfigure(AmpliDiff, foreground="red")
                        tclvalue(AmpliDeg) <- ""
                 })
    tkbind(AmpliDiff, "<Key-Return>", function(K){
                           tkconfigure(AmpliDiff, foreground="black")
                        AA <- as.numeric(tclvalue(AmpliDeg))
                        Differentiated[[2]] <<- AA*DiffBkp[[2]]
                        plotData()
                 })
    tkgrid(AmpliDiff, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

    MaxMinBtn <- tkbutton(D3frame1, text=" Measure Max-Min Dist. ", command=function(){
                        MeasureMaxMinD()
                 })
    tkgrid(MaxMinBtn, row = 1, column = 2, padx = 10, pady = 5, sticky="w")

    DistLab <- ttklabel(D3frame1, text="Max Min Dist:                 ")
    tkgrid(DistLab, row = 1, column = 3, padx = 10, pady = 5, sticky="w")

#--- Common buttons
    D4Group1 <- ttkframe(mainFrame, borderwidth=0, padding=c(0,0,0,0) )
    tkgrid(D4Group1, row = 4, column = 1, padx = 0, pady = 0, sticky="w")

    SaveNewButt <- tkbutton(D4Group1, text=" SAVE AS A NEW CORE LINE ", command=function(){
                        SaveFName("NewCL")
                        BackGnd <<- NULL
                        tclvalue(DDeg) <- ""
                        XPSSaveRetrieveBkp("save")
                        WidgetState(SaveNewButt, "disabled")
                        WidgetState(OverWButt, "disabled")

                 })
    tkgrid(SaveNewButt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

    OverWButt <- tkbutton(D4Group1, text=" OVERWRITE PREVIOUS DIFFERENTIATION ", command=function(){
                        SaveFName("Overwrite")
                        BackGnd <<- NULL
                        tclvalue(DDeg) <- ""
                        XPSSaveRetrieveBkp("save")
                        WidgetState(SaveNewButt, "disabled")
                        WidgetState(OverWButt, "disabled")
                 })
    tkgrid(OverWButt, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

    exitBtn <- tkbutton(D4Group1, text=" EXIT ", width=15, command=function(){
                    tkdestroy(DiffWindow)
                    XPSSaveRetrieveBkp("save")
                 })
    tkgrid(exitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

    WidgetState(DiffDegree, "disabled")
    WidgetState(DiffButt, "disabled")
    WidgetState(ResButt, "disabled")
    WidgetState(SaveNewButt, "disabled")
    WidgetState(OverWButt, "disabled")

}
