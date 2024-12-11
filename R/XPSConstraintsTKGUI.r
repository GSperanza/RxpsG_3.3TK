#Function to edit/set/reset fit parameters:
# allowed function: FIX, Link fit parameters
#                   force the parameter to assume a given value respect to another (es. BE1 == 0.5+BE2)
#
#This function is based on the XPSConstrain() function

#' @title XPSConstraints is a user interface to add a fit constraints
#' @description  XPSConstraints function is a user interface to simplify
#    the setting of the fitting parameters to perform the best fit of a XPSCoreLine
#'   It adds constraints among fit components defined for a XPSCoreLine object.
#'   This function is called after the definition of the baseline using 
#'   (\code{XPSbaseline}) and fitting components with (\code{XPSAddFitComponent}).
#' @seealso \link{XPSConstrain}, \link{XPSAddFitComponent}, \link{XPSFitAlgorithms}
#' @examples
#' \dontrun{
#' 	XPSConstraints()
#' }
#' @export
#'


XPSConstraints <- function(){

#---------- setCommand ----------
   setCommand <- function(h,...){
           Nc1 <- as.integer(gsub("[^0-9]", "", component1))  #extract index from string component1: if component1 is a vector, a vector of indexes is generated
           Nc2 <- as.integer(gsub("[^0-9]", "", component2))  #extract index from string component2
           switch(operation,
           "fix" = {
              FName[[SpectIndx]] <<- XPSConstrain(FName[[SpectIndx]],Nc1,action=operation,variable=parameter,value=setValue, expr=NULL)
              if (parameter == "sigma") {
                 FName[[SpectIndx]] <<- FixCtrl(FName[[SpectIndx]])  #controls that no other links present for ComponentToLink otherwise errors are generated
              }
           },
           "link" = {
              FuncName2 <- FName[[SpectIndx]]@Components[[Nc2]]@funcName
              LL <- length(Nc1)
              for (ii in 1:LL){  #if a link on 'paramter' is aleady present, erase the old link to set the newone
                  #now control we are linking parameters of same kind of fit-function
                  FuncName1 <- FName[[SpectIndx]]@Components[[Nc1[ii]]]@funcName
                  if(grepl(FuncName2, FuncName1) == FALSE){
                      txt <- paste("Component ", Nc1[ii]," = ",FuncName1, "    Component ", Nc2," = ", FuncName2,"\n",
                                   "Linking a parameter among different fit functon is not allowed!", sep="")
                      tkmessageBox(message=txt, title="ERROR", icon="error")
                      return()
                  }
                  LnkdVar <- paste(parameter, Nc1[ii], sep="")
                  if (length(FName[[SpectIndx]]@Components[[ Nc1[ii] ]]@link) > 0){
                     if (LnkdVar == FName[[SpectIndx]]@Components[[ Nc1[ii] ]]@link[[1]]$variable){
                         FName[[SpectIndx]]@Components[[ Nc1[ii] ]]@link <<- list()
                     }
                  }
              }
              if (parameter == "sigma") {
#Attention: here we first save all links in SigmaCtrl
#then when pressing the button SAVE the setlinks() and LinkCtrl() are called to control all links on sigma
#and then XPSConstrain() is executed to set the links and save informarion in the XPSSample
                 for(ii in 1:LL){
                    SigmaCtrl$FitComp <<- c(1:NComp)
                    SigmaCtrl$CompLnkd[Nc1[ii]] <<- Nc1[ii] #save the linked component
                    SigmaCtrl$ToComp[Nc1[ii]] <<- Nc2       #save the linked-TO component
                    SigmaCtrl$Expression[Nc1[ii]] <<- linkExpression   #save the link expression
#in the case of sigma, here only SigmsCtrl is set. All links on sigma will be controlled
#by calling SetLinks(). This function activates linkCTRL() to control all links on sigma and
#calls also XPSConstrain() to set links
                 }
              } else {
                 value <- NULL
                 expression <- paste(parameter,Nc2,linkExpression,sep="") #save linked component and link expression in the XPSSample
                 if (LL == 0){
                    cat("\n Please specify the Fit component!\n")
                    return()
                 }
                 for(ii in 1:LL){
                    FName[[SpectIndx]] <<- XPSConstrain(FName[[SpectIndx]],Nc1[ii],action="link",variable=parameter,value=value,expr=expression)
                 }
              }
           },
           "remove" = {
              LL <- length(Nc1)
              if (LL == 0){
                 cat("\n Please specify the Fit component!\n")
                 return()
              }
              for(ii in 1:LL){
                 if (length(FName[[SpectIndx]]@Components[[ Nc1[ii] ]]@link) > 0) {  #FitComponents selected to set links can be reset although links are still not set
                    FName[[SpectIndx]] <<- XPSConstrain(FName[[SpectIndx]],Nc1[ii],action="remove",variable=NULL,value=NULL,expr=NULL)
                 }
              }
              assign(activeFName, FName, envir=.GlobalEnv)
           },
           "edit" = {
              # do nothing !
           })

           #replot FitComponent with changed parameters and the Fit
#           LL <- length(Nc1)
           for(ii in 1:NComp){
               FName[[SpectIndx]]@Components[[ii]] <<- Ycomponent(FName[[SpectIndx]]@Components[[ii]], x=FName[[SpectIndx]]@RegionToFit$x, y=FName[[SpectIndx]]@Baseline$y)
	              tmp <- sapply(FName[[SpectIndx]]@Components, function(z) matrix(data=z@ycoor))  #fit is the sum of fitting components
	              FName[[SpectIndx]]@Fit$y <<- ( colSums(t(tmp)) - length(FName[[SpectIndx]]@Components)*(FName[[SpectIndx]]@Baseline$y)) #substract NComp*Baseline
	              FName[[SpectIndx]] <<- sortComponents(FName[[SpectIndx]])
           }
           plot(FName[[SpectIndx]])
           Saved <<- FALSE
   }


#---------- SetLinks ----------

   SetLinks <- function() {    #SetLinks is executed when SAVE button is pressed
                               #SetLinks controls only the links on sigma!!!
             #reset previous links
             for (ii in 1:NComp){
                 LL <- length(FName[[SpectIndx]]@Components[[ii]]@link)
                 if (LL > 0) { #there is a link set on one of the fitting parameters
                    VarLnkd <- NULL
                    for (jj in 1:LL){ #scan the link slot: if the link is on sigma then the link will be reset
                        VarLnkd <- c(VarLnkd, FName[[SpectIndx]]@Components[[ii]]@link[[jj]]$variable) #vector containing the name of variable linked
                    }
                    jj <- grep("sigma", VarLnkd)
                    if (length(jj) > 0) {
                       FName[[SpectIndx]]@Components[[ii]]@link[[jj]] <- NULL #delete links on sigma
                    }
                 }
             }
             SigmaCtrl <- LinkCtrl(SigmaCtrl)  #controls on sigmas conditions
             SigmaCtrl <- na.omit(SigmaCtrl)   #eliminates NA from SigmaCtrl
             LL <- length(SigmaCtrl$CompLnkd)  #number of links on sigma
             for (ii in 1:LL) {
                  operation <- NULL
                  value <- NULL
                  Nc1 <- SigmaCtrl$CompLnkd[ii]
                  Nc2 <- SigmaCtrl$ToComp[ii]
                  if (! is.na(Nc1)){
                      linkExpr <- SigmaCtrl$Expression[ii]
                      linkExpr <- paste("sigma",Nc2,linkExpr,sep="") #there is also an operation on the linked component

                      FName[[SpectIndx]] <- XPSConstrain(FName[[SpectIndx]],Nc1,action="link",variable="sigma",value=value,expr=linkExpr)
                  }
             }
             cat("\n ==> Constraints saved!\n")
             FName[[SpectIndx]] <<- FName[[SpectIndx]]
   }

#---------- LinkCtrl ----------

   LinkCtrl <- function(SigmaCtrl) {    #LinkCtrl made when SAVE button pressed
      LinkedComp <- NULL
      LinkedComp <- cbind(SigmaCtrl$CompLnkd, SigmaCtrl$ToComp, SigmaCtrl$FitComp)  #the third column represents component indexes
      LinkedComp <- na.omit(LinkedComp)
      LL <- length(LinkedComp[,1])

      NComp <- length(SigmaCtrl$FitComp)
      LWrng <- NComp  #set length of WrongLinks to NComp
      NWrng <- NComp
      
      for(ii in 1:NComp){
          if (length(FName[[SpectIndx]]@Components[[ii]]@link) > 0) { #a link is set on this component
              LnkC1 <- FName[[SpectIndx]]@Components[[ii]]@link[[1]]$variable  #sigma4  or other linked variable
              Indx1 <- gsub("[^0-9]", "", LnkC1)   #if LnkC1=="sigma4" only the alphanumerical part "4" is taken: now 4
              ParName1 <- unlist(strsplit(LnkC1, Indx1))   #now split "sigma4" in "sigma" and "4" and return "sigma"
              Indx1 <- as.integer(Indx1)
              if (Indx1 != ii) { #the Indx1 MUST correspond to the parent FitComp_index == ii i.e. sigma4 to C4
                  txt <- paste(" Found Link Inconsistency of Parameter ", ParName1, " Fit_Component ", ii,
                               "\n Please Control/Reset Links!", sep="")
                  tkmessageBox(message=txt, title="ERROR", icon="error")
              }
          }
      }
      cat("\n ==> Link Consistency OK!")


      while(LWrng>0) {
         LinkedComp <- cbind(SigmaCtrl$CompLnkd, SigmaCtrl$ToComp, SigmaCtrl$FitComp)
         LinkedComp <- na.omit(LinkedComp)
         LL <- length(LinkedComp[,1])
#identification of Reference components: NO links present for them
         RefComp <- NULL   #RefComp is a vector of the indexes of Reference components
         jj=1
         for(ii in 1:NComp){
            if(is.na(SigmaCtrl$CompLnkd[ii])) {
               RefComp[jj] <- ii  #RefComp non linked components => SigmaCtrl$CompLnkd[ii]=NA
               jj=jj+1
            }
         }
         txt <- paste(paste("C", as.character(RefComp), sep=""), collapse=" ")
         cat("\n ==> Found Non-Linked Components: ",txt)

#drop correctly linked components from wrong links
         NRef <- length(RefComp) #runs on the NON-linked components
         NLnks <- length(LinkedComp[,1]) #Number of links
         indxOK <- NULL
         for(ii in 1:NRef){    #this for runs on all the NON-linked components
             for(jj in NLnks:1){
                if(LinkedComp[jj,2]==RefComp[ii]) {
                   indxOK <- c(indxOK,jj)   #this are the indexes of correctly linked components
                }
             }
         }
         NWrng <- NLnks-length(indxOK)     #NWrng = NLinks - Links OK
         WrongLnks <- matrix(LinkedComp[-indxOK,], nrow=NWrng, ncol=3)
         LWrng <- length(WrongLnks)
         if (LWrng==0) {
             SigmaCtrl$CompLnkd <- na.omit(SigmaCtrl$CompLnkd)
             SigmaCtrl$ToComp <- na.omit(SigmaCtrl$ToComp)
             SigmaCtrl$FitComp <- na.omit(SigmaCtrl$FitComp)
             SigmaCtrl$Expression <- na.omit(SigmaCtrl$Expression)
             cat("\n ==> Link Ctrl Done!")
             break    #break while loop
         }
#Now control elements of LinkedComp which are linked to a FitComp which is are in turn linked to anothed FitComponent
         for(ii in 1:NWrng){
             for(jj in 1:NLnks){
                if(WrongLnks[ii,2]==LinkedComp[jj,1]) {
                   idx1 <- WrongLnks[ii,3]                       #position of WrongLnk in SigmaCtrl
                   idx2 <- LinkedComp[jj,3]
                   SigmaCtrl$ToComp[idx1] <- LinkedComp[jj,2]    #change the link to the correct FitComponent
                   if (nchar(SigmaCtrl$Expression[idx2])>0 && is.na(SigmaCtrl$Expression[idx2])==FALSE){
                      SigmaCtrl$Expression[idx1] <- SigmaCtrl$Expression[idx2]  #copy the operation present on the reference FitComponent to the linked component
                   }
                }
             }
         }
         LinkedComp <- NULL
      }
      return(SigmaCtrl)
   }


#---------- FixCtrl ----------

   FixCtrl <- function(Object) {
#Let us suppose a FIX contraint on sigmaC1 = 1.9eV = sigma reference component 1 is set. FixCtrl checks that
#all the linked sigma to the reference Comp1 have the same amplitude
#Since it is unknown if first the sigmaC1 is set to 1.9eV and then the links are created,
#the control the apmplitude of sigma of the other components be == sigmaC2 is made either if FIX and LINK
#constraints are set

#      NComp <- length(Object@Components)
      CompIndx <- as.integer(gsub("[^0-9]", "", component1))   #index of the linked component
      SigC1 <- paste("sigma", as.character(CompIndx), sep="")  #a string made of "sigmaXX" where XX is the index of the ReferenceComp.
      SigC1Start <- Object@Components[[CompIndx]]@param$start[3]   #get the value of start, min, max to make them equal to those of ReferenceComp. C1
      SigC1Min <- Object@Components[[CompIndx]]@param$min[3]
      SigC1Max <- Object@Components[[CompIndx]]@param$max[3]
      for (ii in 1:NComp){
          if (length(Object@Components[[ii]]@link)>0) {
              if (Object@Components[[ii]]@link[[1]]$expr == SigC1) { #here control if the link is to ReferenceComp. == C1
                  Object@Components[[ii]]@param$start[3] <- SigC1Start #make values equal to those of ReferenceComp.
                  Object@Components[[ii]]@param$min[3] <- SigC1Min
                  Object@Components[[ii]]@param$max[3] <- SigC1Max
              }
          }
      }
      return(Object)
   }


#---------- editFitFrame ----------

   editFitFrame <- function(){
      selectedComp <- tclvalue(T1FitCompA)
      CompIndx <- as.integer(gsub("[^0-9]", "", selectedComp))
      fitParam <<- FName[[SpectIndx]]@Components[[CompIndx]]@param #load DataFrame relative to the selected component
      fitParam <<- round(fitParam, 4)
      TT <- paste("FIT PARAMETERS: component ", selectedComp,sep="")
      ParNames <- rownames(fitParam)
      idx <- grep("lg", ParNames)
      if(length(idx) > 0){ParNames[idx] <- "Mix.L.G"}
      idx <- grep("gv", ParNames)
      if(length(idx) > 0){ParNames[idx] <- "Mix.G.V"}
      CNames <- c("Start", "Min", "Max")
      fitParam <<- as.matrix(fitParam)
      fitParam <<- as.data.frame(fitParam, stringsAsFactors=FALSE) #in the dataframe add a column with variable names
      fitParam <<- DFrameTable(Data=fitParam, Title=TT, ColNames=CNames, RowNames=ParNames, 
                               Width=15, Modify=TRUE, Env=environment(), parent=NULL)
      FName[[SpectIndx]]@Components[[CompIndx]]@param <<- fitParam #save parameters in the slot of XPSSample
      operation <<- "edit"
      component1 <<- selectedComp
      parameter <<- NULL
      setCommand()    #only to replot the new fit
      XPSSaveRetrieveBkp("save")
      return()
   }


#===== variables =====
   activeFName <- get("activeFName",envir=.GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load XPS Spectra", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName,envir=.GlobalEnv)
   OldFName <- FName
   SpectIndx <- get("activeSpectIndx", envir=.GlobalEnv)
   SpectName <- NULL
   if (is.na(activeSpectIndx) || is.null(activeSpectIndx) || length(activeSpectIndx)==0){
       activeSpectIndx <<- 1
       activeSpectName <<- names(FName)[1]
   } else {
       activeSpectName <<- names(FName)[SpectIndx]
   }
   SpectList <- XPSSpectList(activeFName)
   NComp <- length(FName[[SpectIndx]]@Components)

   FitComp1 <- names(FName[[SpectIndx]]@Components)
   FitComp2 <- FitComp1
   fitParam <- NULL
   newFitParam <- NULL
   ParamList <- ""
   ParamLbl <- ""
   LinkFrame2 <- NULL
   LinkFrame3 <- NULL
   Linklayout <- list()
   T3LinkParam <- list()  #list containing the IP of the Param checkboxes
   RefLinkComp <- list()  #list containing the IP of the Reference corelines checkboxes
   LinkExpr <- list()  #list containing the IP of the Link_Expression Entry
   LinkGroup3 <- list()   #list containing the address of the group containing the Param checkboxes
   chckd <- NULL          #tclVar containing the selected checkboxes
   SelFitComp2 <- NULL    #tclVar containing the component2 selected as reference
   LinkIndx <- list(P=NULL, C=NULL)    #list containing the indexes identifying the selected param P and FitComp C
   EXPR <- NULL

   operation <- ""
   parameter <- ""
   linkExpression <- NULL
   setValue <- ""
   component1 <- NULL
   component2 <- NULL
   NewParam <- ""
   NewRSF <- NULL
   Saved <- FALSE
   SigmaCtrl <- list(FitComp=c(1:NComp), CompLnkd=NULL, ToComp=NULL, Expression=NULL)
   plot(FName[[SpectIndx]])


#--- Ctrl on the active coreline if fit is present or selection of a new core line.
     if (NComp ==0){
         NoFitWin <- tktoplevel()
         tkwm.title(NoFitWin," NO FITTED CORE-LINES ")
         tkwm.geometry(NoFitWin, "+100+50")   #SCREEN POSITION from top-left corner

         NFGroup <- ttkframe(NoFitWin,  borderwidth=2, padding=c(5,5,5,5) )
         tkgrid(NFGroup, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

         txt <- paste(" ATTENTION: no fit found in ", activeSpectName, "\n Change core line please!")
         tkgrid( ttklabel(NFGroup, text=txt, font="Sans 12 normal" ),
                 row = 2, column = 1, padx = 5, pady=5, pady = 5, sticky="w")
         tkgrid( ttkseparator(NFGroup, orient="horizontal"),
                 row = 3, column = 1, padx = 5, pady = 10, sticky="we")

         NoFitFrame <- ttklabelframe(NFGroup, text="SELECT CORE LINE", borderwidth=2, padding=c(5,5,5,5) )
         tkgrid(NoFitFrame, row = 4, column = 1, padx = 5, pady = 5, sticky="w")
         FittedCL <- tclVar()
         NoFitCombo <- ttkcombobox(NoFitFrame, width = 15, textvariable = FittedCL, values = SpectList)
         tkbind(NoFitCombo, "<<ComboboxSelected>>", function(){
                        SourceCoreline <- tclvalue(FittedCL)
                        SourceCoreline <- unlist(strsplit(SourceCoreline, "\\."))
                        SpectName <<- SourceCoreline[2]
                        SpectIndx <<- as.integer(SourceCoreline[1])
                        NComp <<- length(FName[[SpectIndx]]@Components)
                        FitComp1 <<- names(FName[[SpectIndx]]@Components)
                        plot(FName[[SpectIndx]])
                     })
         tkgrid(NoFitCombo, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

         OKButt <- tkbutton(NoFitFrame, text=" SET & EXIT ", width=12, command=function(){
                        SpectName <<- names(FName)[SpectIndx]
		                      if (NComp==0){
                            txt <- paste(" ATTENTION: no fit found in ", SpectName, "\n Change core line please!")
                            tkmessageBox(message=txt, title="ERROR", icon="error")
		                          tkdestroy(NoFitWin)
                        }
		                      assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
		                      assign("activeSpectName", SpectName, envir=.GlobalEnv)
		                      tkdestroy(NoFitWin)
                     })
         tkgrid(OKButt, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
         tkwait.window(NoFitWin)
      }
      if (NComp == 0) { return() }  #exit FitConstraints if none fit found

#--- Widget
      mainFCwin <- tktoplevel()
      tkwm.title(mainFCwin," FIT PARAMETER CONSTRAINTS ")
      tkwm.geometry(mainFCwin, "+100+50")   #SCREEN POSITION from top-left corner

      MainGroup <- ttkframe(mainFCwin,  borderwidth = 5, padding = c(5,5,5,5))
      tkgrid(MainGroup, row = 1, column=1, sticky="w")
      NBframe <- ttklabelframe(MainGroup, text = "CONTRAINTS", borderwidth=2)
      tkgrid(NBframe, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

#--- Notebook to set contraints -----
      NB <- ttknotebook(NBframe)
      tkgrid(NB, row = 1, column = 1, padx = 5, pady = 5)

#--- Tab1 - EDIT ---
      T1group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
      tkadd(NB, T1group1, text=" EDIT FIT PARAMETERS ")

      T1frame1 <- ttklabelframe(T1group1, text = "SELECT COMPONENT TO EDIT", borderwidth=2)
      tkgrid(T1frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
      T1FitCompA <- tclVar("")
      T1obj1 <- ttkcombobox(T1frame1, width = 20, textvariable = T1FitCompA, values = FitComp1)
      tkbind(T1obj1, "<<ComboboxSelected>>", function(){
                           editFitFrame()
                     })
      tkgrid(T1obj1, row = 1, column = 1, padx = 10, pady = 5, sticky="w")

      tkgrid( ttkseparator(T1group1, orient="horizontal"),
              row = 2, column = 1, pady = c(15, 10), sticky="we")
      tkgrid( ttkseparator(T1group1, orient="horizontal"),
              row = 2, column = 2, pady = c(15, 10), sticky="we")

      T1group2 <- ttkframe(T1group1, borderwidth=2, padding=c(0,0,0,0))
      tkgrid(T1group2, row = 3, column = 1, padx = 0, pady = 0, sticky="w")
      T1frame2 <- ttklabelframe(T1group2, text = "SELECT COMPONENT to CHANGE RSF", borderwidth=2)
      tkgrid(T1frame2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
      T1FitCompB <- tclVar("")
      T1obj2 <- ttkcombobox(T1frame2, width = 20, textvariable = T1FitCompB, values = FitComp1)
      tkbind(T1obj2, "<<ComboboxSelected>>", function(){
                           FitComp <- tclvalue(T1FitCompB)
                           CompIndx <- as.integer(gsub("[^0-9]", "", FitComp))
                           OldRSF <- FName[[SpectIndx]]@Components[[CompIndx]]@rsf
                           tclvalue(SetRelSensFact) <- OldRSF
                     })
      tkgrid(T1obj2, row = 1, column = 1, padx = 10, pady = 5, sticky="w")

      SetRelSensFact <- tclVar("")
      SetRSF <- ttkentry(T1frame2, textvariable=SetRelSensFact)
      tkgrid(SetRSF, row = 1, column = 2, padx = 10, pady = 5, sticky="w")
      tkbind(SetRSF, "<FocusIn>", function(K){
                           tkconfigure(SetRSF, foreground="red")
                     })
      tkbind(SetRSF, "<Key-Return>", function(K){
                           tkconfigure(SetRSF, foreground="black")
                           NewRSF <<- tclvalue(SetRelSensFact)
                           if (NewRSF != ""){
                               NewRSF <<- as.numeric(NewRSF)
                               tclvalue(SetRelSensFact) <- ""
                           }
                     })

      T1ButtRSF <- tkbutton(T1frame2, text=" SAVE  RSF ", width=20, command=function(){
                           FitComp <- tclvalue(T1FitCompB)
                           CompIndx <- as.integer(gsub("[^0-9]", "", FitComp))
                           slot(FName[[SpectIndx]]@Components[[CompIndx]], "rsf") <- NewRSF #load new parameter in the XPSSample slot
                           FName[[SpectIndx]] <<- FName[[SpectIndx]]
                     })
      tkgrid(T1ButtRSF, row = 2, column = 1, padx = 10, pady = 5, sticky="w")

      T1ButtRmvCnst <- tkbutton(T1group1, text="REMOVE COMPONENT CONTRAINTS", width=30, command=function(){
                           operation <<- "remove"
                           component1 <<- tclvalue(T1FitCompB)
                           setCommand()
                     })
      tkgrid(T1ButtRmvCnst, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

      T1ButtRmvAll <- tkbutton(T1group1, text="REMOVE ALL CONSTRAINTS", width=30, command=function(){
                           SigmaCtrl <<- list(FitComp=c(1:NComp), CompLnkd=NULL, ToComp=NULL, Expression=NULL)
                           operation <<- "remove"
                           for (ii in 1:NComp){
                               component1 <<- FitComp1[ii]
                               setCommand()
                           }
                     })
      tkgrid(T1ButtRmvAll, row = 6, column = 1, padx = 5, pady = 5, sticky="w")



# --- Tab2 - FIX ---
      T2group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
      tkadd(NB, T2group1, text=" FIX / SET ")

      T2frame1 <- ttklabelframe(T2group1, text = "SELECT COMPONENT", borderwidth=2)
      tkgrid(T2frame1, row = 1, column = 1, padx = 5, pady = 3, sticky="w")
      T2FitCompA <- tclVar("")
      T2obj1 <- ttkcombobox(T2frame1, width = 20, textvariable = T2FitCompA, values = FitComp1)
      tkbind(T2obj1, "<<ComboboxSelected>>", function(){
                           component1 <- tclvalue(T2FitCompA)   #componente scelta
                           CompIndx <- as.integer(gsub("[^0-9]", "", component1))  #indice della componente
                           ParamList <- rownames(FName[[SpectIndx]]@Components[[CompIndx]]@param) #carico il DataFrame relativo alla componente selezionata nel Radiobox
                           tkconfigure(T2obj2, values=ParamList)
                           WidgetState(T2obj2, "normal")
                     })
      tkgrid(T2obj1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

      T2frame2 <- ttklabelframe(T2group1, text = "PARAMETER TO FIX/SET", borderwidth=2)
      tkgrid(T2frame2, row = 2, column = 1, padx = 5, pady = 3, sticky="w")
      T2Param <- tclVar("")
      T2obj2 <- ttkcombobox(T2frame2, width = 20, textvariable = T2Param, values = " ")
      tkbind(T2obj2, "<<ComboboxSelected>>", function(){
                           component1 <<- tclvalue(T2FitCompA)   #componente scelta
                           CompIndx <- as.integer(gsub("[^0-9]", "", component1))  #indice della componente
                           parameter <- tclvalue(T2Param)
                           OldValue <- FName[[SpectIndx]]@Components[[CompIndx]]@param[parameter,"start"] # valore attuale del parametro
                           tclvalue(T2SetParamVal) <- OldValue
                           NewParam <<- round(as.numeric(OldValue), digits=2)
                     })
      tkgrid(T2obj2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
      WidgetState(T2obj2, "disabled")

      T2frame3 <- ttklabelframe(T2group1, text = "VALUE TO SET", borderwidth=2)
      tkgrid(T2frame3, row = 3, column = 1, padx = 5, pady = 3, sticky="w")
      T2SetParamVal <- tclVar("")
      T2obj3 <- ttkentry(T2frame3, textvariable=T2SetParamVal, width=23)
      tkbind(T2obj3, "<FocusIn>", function(K){
                         tkconfigure(T2obj3, foreground="red")
                     })
      #now ttkentry waits for a return to read the entry_value
      tkbind(T2obj3, "<Key-Return>", function(K){
                           tkconfigure(T2obj3, foreground="black")
                           NewParam <- tclvalue(T2SetParamVal)
                           if (NewParam != ""){
                               NewParam <<- as.numeric(NewParam)
                           }
                     })
      tkgrid(T2obj3, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

      T2ButtSetCnstr <- tkbutton(T2group1, text="SET CONSTRAINT", width=30, command=function(){
                           component1 <<- tclvalue(T2FitCompA)
                           component2 <<- "NULL"
                           CompIndx <- as.integer(CompIndx <- gsub("[^0-9]", "", component1))  #component index
                           parameter <<- tclvalue(T2Param)
                           operation <<- "fix"     #operation=set only when Gedit is used, otherwise it is forced to FIX when a parameter is selected (see handler T2obj1)
                           setValue <<- NewParam
                           linkExpression <<- ""
                           setCommand()
                     })
      tkgrid(T2ButtSetCnstr, row = 4, column = 1, padx = 5, pady = 3, sticky="w")

      T2ButtRmvCnst <- tkbutton(T2group1, text="REMOVE COMPONENT CONTRAINTS", width=30, command=function(){
                           operation <<- "remove"
                           component1 <<- tclvalue(T2FitCompA)
                           setCommand()
                     })
      tkgrid(T2ButtRmvCnst, row = 5, column = 1, padx = 5, pady = 3, sticky="w")

      T2ButtRmvAll <- tkbutton(T2group1, text="REMOVE ALL CONSTRAINTS", width=30, command=function(){
                           SigmaCtrl <<- list(FitComp=c(1:NComp), CompLnkd=NULL, ToComp=NULL, Expression=NULL)
                           operation <<- "remove"
                           for (ii in 1:NComp){
                               component1 <<- FitComp1[ii]
                               setCommand()
                           }
                     })
      tkgrid(T2ButtRmvAll, row = 6, column = 1, padx = 5, pady = 3, sticky="w")





# --- Tab3 - LINK ---
      T3group1 <- ttkframe(NB,  borderwidth=2, padding=c(5,5,5,5) )
      tkadd(NB, T3group1, text=" LINK ")

#---  FitFunctionName generates a Table containing the FitComp and relative FitFunction
      FitFunctionName <- function(){
          FitFwin <- tktoplevel()
          tkwm.title(FitFwin,"FIT FUNCTION NAME")
          tkwm.geometry(FitFwin, "+220+150")   #SCREEN POSITION from top-left corner
          FitFframe <- ttklabelframe(FitFwin, text = "Fit Function Names", borderwidth=3)
          tkgrid(FitFframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
          CName <- names(FName[[SpectIndx]]@Components)
          NfitC <- length(FitComp1)
          FFname <- NULL
          for(jj in 1:NfitC){
              FFname[jj] <- FName[[SpectIndx]]@Components[[jj]]@funcName  #retrieve FFname
          }
          FitFnInfo <- list(Components=CName, FitFunct=FFname)
          FitFTbl <- XPSTable(parent=FitFframe, items=FitFnInfo,
                              ColNames=c("Comp.", "Fit Funct."), Width=c(60, 150))
          ExitButt <- tkbutton(FitFframe, text=" EXIT ", width=10, command=function(){
                                     tkdestroy(FitFwin)
                               })
          tkgrid(ExitButt, row = 2, column = 1, padx = 5, pady = 5, sticky = "w")
      }



#---- Function ResetLinkes() clears all related variables
      ResetLinks <- function(){
          NfitP <- length(ParamLbl)
          NfitC <- length(FitComp1)
          for (ii in 1:NfitP){
              for (jj in 1:NfitC){ #reset Param Link Table checkbutton
                  tclvalue(chckd[ii,jj]) <<- FALSE
              }
          }
          operation <<- ""
          parameter <<- NULL
          linkExpression <<- ""
          setValue <<- ""
          component1 <<- NULL   #describes the selected FitComponents to link
          component2 <<- NULL   #describes the possible FitComp reference for the link: link FC1 to FC2
          LinkIndx[["P"]] <<- NULL   #reset the Param, FitComp indexes of the first selection
          LinkIndx[["C"]] <<- NULL
          FitComp2 <<- FitComp1 #initially all the fit components may be references
          #Reset FitComp2 Checknutton
          childID <- tclvalue(tkwinfo("children", LinkGroup3)) #Remove CL_checkbox
          if (length(childID) > 0){
              sapply(unlist(strsplit(childID, " ")), function(x) {
                   tcl("grid", "remove", x)
             })
          }
          RefLinkComp <<- list()
          tclvalue(SelFitComp2) <<- NULL
          for(jj in 1:length(FitComp2)){  #this checkbutton behaves as a radiob. Only 1 element can be checked
              RefLinkComp[[jj]] <<- tkcheckbutton(LinkGroup3, text=FitComp2[jj], variable=SelFitComp2, onvalue=FitComp2[jj], offvalue = 0)
              tkgrid(RefLinkComp[[jj]], row = 1, column = jj, padx=5, pady=5, sticky="w")
              tclvalue(SelFitComp2) <<- FALSE
          }
      }

#---  handler to control consistency of parameter selection
      SetParamLbl <- function(){
          ParamList <- rownames(FName[[SpectIndx]]@Components[[1]]@param) #set ParamList == FitParam of C1
          NfitC <- length(FitComp1)
          for (jj in 2:NfitC){        #cycle to Fit components
              PList <- rownames(FName[[SpectIndx]]@Components[[jj]]@param) #load the parameter sequence of the FitComponent[ii]
              LL <- length(PList)
              for (ii in 1:LL){
                  PP <- match(PList[ii], ParamList, nomatch=-1)  #compare parameters of FitComp[ii] with ParamLbl
                  if (PP < 0) { ParamList <- c(ParamList, PList[ii]) }  #add FitFaram[ii] of FitComp[jj] if it does not exist in ParamLbl
              }
          }
          return(ParamList)
      }

#---  Control on the checked boxes, consistency of the selection and save the selected parameters
      ParamCkCtrl <- function(){
          NfitP <- length(ParamLbl)
          NfitC <- length(FitComp1)
          SelComp <- NULL
          paramIdx <- NULL
          for (jj in 1:NfitC){
              for (ii in 1:NfitP){  #check if one parameter was selected for the jj_FitComponent          
                   PList <- rownames(FName[[SpectIndx]]@Components[[jj]]@param) #load the parameter dataframe of the selected component
                   if (! is.na(T3LinkParam[[ii]][[jj]][1])) { #consider only the element of the LiknTable for which a checkbox is defined
                      if (tclvalue(chckd[ii,jj]) == "1" && is.null(LinkIndx[["P"]])){
                         LinkIndx[["P"]] <<- ii   #save the Param index of the first selection
                         LinkIndx[["C"]] <<- jj   #save the FitComp index of the first selection
                      }
                      if (tclvalue(chckd[ii,jj]) == "1"){
                         FitFunct1 <- FName[[SpectIndx]]@Components[[ LinkIndx[["C"]] ]]@funcName #Component FitFunction name
                         FitFunct2 <- FName[[SpectIndx]]@Components[[jj]]@funcName
                         if (FitFunct1 != FitFunct2){
                            txt <- paste("Error: cannot link Parameters from different functions: \n==> Function1 = ", FitFunct1, "\n==> Function2 = ",FitFunct2, sep="")
                            tkmessageBox(message=txt, title="WRONG SELECTION", icon="error")
                            tclvalue(chckd[ii,jj]) <<- 0
                         }
                         if (ii != LinkIndx[["P"]]){
                            tkmessageBox(message="Error: cannot link different parameters!", title="WRONG SELECTION", icon="error")
                            tclvalue(chckd[ii,jj]) <<- 0
                         } else {
                            SelComp[jj] <- jj #save all the selected components
                            parameter <<- ParamLbl[ii]
                            paramIdx <- ii
                         }
                      }
                  }
              }
          }

          if(length(SelComp) == 0){  #if all parameter selections are cleared, reset all variables and return
             ResetLinks()
             return()
          }
          SelComp <- na.omit(SelComp)
          component1 <<- FitComp1[SelComp]    #select the checked FitComponents
          for (jj in 1:NfitC){
              PList <- rownames(FName[[SpectIndx]]@Components[[jj]]@param) #load the parameter dataframe of the selected component
              if (length(grep(parameter, PList)) == 0){
                  SelComp <- c(SelComp, jj)   #if the selected parameter is not present in the FitComp[jj]
              }                               #consider FitComp[jj] as selected component to erase it from FitComp2 list
          }
          LL <- length(SelComp)
          if (length(FitComp1[-SelComp]) == 0){   #when length(FitComp1)==0 FitComp2 contains only the last selection
             LastComp <- as.integer(gsub("[^0-9]", "", FitComp2))
             tkmessageBox(message="Error: too many components selected. No reference component available!", title="WRONG SELECTION", icon="error")
             tclvalue(chckd[paramIdx,LastComp]) <<- 0
             #let suppose selection is C1, C3, C2, C5, C4 last element of component1 is C4, but 4 is != length(component1)==LL we cannot use component1[-LL] to eliminate the last added component
             #let suppose FitParam asymm is present only in C2, C4. Let the selection C2 C4 == component1. To eliminate last component C4: component1[-2] and NOT component1[-4]
             #then we have to use grep to eliminate the last component added to component1
             idx <- grep(FitComp2, component1)    #LastComponent is not the last
             component1 <<- component1[-idx]
             return()
          }

          if ( LL > 0){           #FitComp2 = list of possible reference FitComp to which set the link
             FitComp2 <<- FitComp1[-SelComp]  #erase the selected components
          } else if (LL == 0){    #all selections cleared
             FitComp2 <<- FitComp1
             LinkIndx[["P"]] <<- NULL         #reset the Param, FitComp indexes of the first selection
             LinkIndx[["C"]] <<- NULL
          }

          #delete RefLinkComp checkbox
          childID <- tclvalue(tkwinfo("children", LinkGroup3)) #Remove CL_checkbox
          if (length(childID) > 0){
              sapply(unlist(strsplit(childID, " ")), function(x) {
                   tcl("grid", "remove", x)
             })
          }
          if (length(FitComp1[-SelComp]) == 0){   #when length(FitComp1)==0 FitComp2 contains only the last selection
             LastComp <- as.integer(gsub("[^0-9]", "", FitComp2))
             tkmessageBox(message="Error: too many components selected. No reference component available!", title="WRONG SELECTION", icon="error")
             tclvalue(chckd[paramIdx,LastComp]) <<- 0
             #let suppose selection is C1, C3, C2, C5, C4 last element of component1 is C4, but 4 is != length(component1)==LL we cannot use component1[-LL] to eliminate the last added component
             #let suppose FitParam asymm is present only in C2, C4. Let the selection C2 C4 == component1. To eliminate last component C4: component1[-2] and NOT component1[-4]
             #then we have to use grep to eliminate the last component added to component1
             idx <- grep(FitComp2, component1)    #LastComponent is not the last
             component1 <<- component1[-idx]
             return()
          }

          if ( LL > 0){           #FitComp2 = list of possible reference FitComp to which set the link
             FitComp2 <<- FitComp1[-SelComp]  #erase the selected components
          } else if (LL == 0){    #all selections cleared
             FitComp2 <<- FitComp1
             LinkIndx[["P"]] <<- NULL         #reset the Param, FitComp indexes of the first selection
             LinkIndx[["C"]] <<- NULL
          }

          #delete RefLinkComp checkbox
          childID <- tclvalue(tkwinfo("children", LinkGroup3)) #Remove CL_checkbox
          if (length(childID) > 0){
              sapply(unlist(strsplit(childID, " ")), function(x) {
                   tcl("grid", "remove", x)
             })
          }

          #Update FitComp2 reference checkbuttons
          RefLinkComp <<- list()
          SelFitComp2 <<- tclVar()
          for(jj in 1:length(FitComp2)){  #this checkbutton behaves as a radiobutt. Only 1 element can be checked
              RefLinkComp[[jj]] <<- tkcheckbutton(LinkGroup3, text=FitComp2[jj], variable=SelFitComp2,
                                                 onvalue=FitComp2[jj], offvalue = 0, command=function(){
                                                     selFC2 <- tclvalue(SelFitComp2)
                                                     if (FitFunct1 != FitFunct2){
                                                         FitFunct1 <- FName[[SpectIndx]]@Components[[ LinkIndx[["C"]] ]]@funcName #Ctrl on the FitFunctions relative
                                                         FitFunct1 <- FName[[SpectIndx]]@Components[[selFC2]]@funcName #Ctrl on the FitFunctions relative
                                                     }
                                                     if (FitFunct1 != FitFunct2){
                                                         txt <- paste("Error: cannot link Parameters from different functions: \n==> Function1 = ", FitFunct1, "\n==> Function2 = ",FitFunct2, sep="")
                                                         tkmessageBox(message=txt, title="WRONG SELECTION", icon="error")
                                                         tclvalue(SelFitComp2) <<- FALSE
                                                     }
                                                })
              tkgrid(RefLinkComp[[jj]], row = 1, column = jj, padx=5, pady=5, sticky="w")
              tclvalue(SelFitComp2) <<- FALSE
          }

      } #ParamCkCtrl() end


#---  HndlrSetLinks generates the checkbox table considering that parameters 
#     may be related to different fitting functions:
#     es: for Gauss parameters are  h, mu, sigma
#         for DoniachSunjicGauss parameters are  h, mu, sigmaDS, sigmaG, asym
#     controls are done to avoid linking parameters from different fitting functions
#     i.e.  link sigmaGauss to sigmaDS is forbidden.
      HndlrSetLinks <- function(){
                         FitComp2 <<- FitComp1
                         ParamLbl <<- ""
                         RefLinkComp <<- list()

                         LinkFCwin <- tktoplevel()
                         tkwm.title(LinkFCwin,"FIT PARAMETER TABLE")
                         tkwm.geometry(LinkFCwin, "+200+100")   #SCREEN POSITION from top-left corner

                         LinkGroup1 <- ttkframe(LinkFCwin,  borderwidth=2, padding=c(5,5,5,5))
                         tkgrid(LinkGroup1, row = 1, column=1, sticky="w")
                         LinkFrame1 <- ttklabelframe(LinkGroup1, text = "SELECT LINKS", borderwidth=2)
                         tkgrid(LinkFrame1, row = 1, column=1, padx = 5, pady = 5, sticky="w")
                         MyFont <- "Sans 12 normal"
                         msg1 <- ttklabel(LinkFrame1, text="Please select the parameter to link checking the correspondent component", font=MyFont)
                         tkgrid(msg1, row = 2, column = 1, padx = 5, pady = 2, sticky="w")

                         LinkGroup2 <- ttkframe(LinkFrame1, borderwidth=0, padding=c(0,0,0,0))
                         tkgrid(LinkGroup2, row = 3, column = 1, padx = 0, pady = 0, sticky="w")
                         #Build the vector of all non-equal FitParam: it may happen that Fit is made using different lineshapes
                         #In this case the number and the kind of FitParam associated to different FitComp may be different
                         #all the FitParam sequence of all FitComponents are compared with the ParamLbl initially set for C1 FitComp.
                         #All new parameters are added to ParamLbl. The final vector is a sequence of all non-equal parameters
                         ParamLbl <<- SetParamLbl()
                         NfitC <- length(FitComp1)
                         NfitP <- length(ParamLbl)
                         FitFuncHlp <- tkbutton(LinkGroup2, text="FitFun ?", command=function(){
                                    FitFunctionName()
                         })
                         tkgrid(FitFuncHlp, row = 1, column = 1, padx = 10, pady = 0, sticky="w")


                         #Build the parameter Checkbox Table
                         for (jj in 1:NfitC){  #in the first row of the Table are the FitComponent names
                              tkgrid( ttklabel(LinkGroup2, text=FitComp1[jj]),
                                      row = 1, column = jj+1, padx = 10, pady = 0, sticky="we")
                         }
                         for (ii in 1:NfitP){  #in the first column of the Table are the Fit Param of component1
                              tkgrid( ttklabel(LinkGroup2, text=ParamLbl[ii]),
                                      row = ii+1, column = 1, padx = 10, pady = 0, sticky="we")
                              T3LinkParam[[ii]] <<- list()
                         }
                         #now build the checkbox table for Param Linking: one checkbox for each FitParam
                         chckd <<- matrix(data=0, nrow=NfitP, ncol=NfitC)
                         for (jj in 1:NfitC){  #runs on Fit Components
                             PList <- rownames(FName[[SpectIndx]]@Components[[jj]]@param) #load the parameter sequence of the FitComponent[ii]
                             PP <- match(ParamLbl, PList, nomatch=0) #find elements of ParamLbl present in ParamList. Output is hh when ParamLbl[ii]==ParamList[hh] or zero otherwise.
                             #index ii with PP[ii]>0 defines the position of those elements in the Checkbox column jj. Here we are interested in the ii value not in the value of PP[ii]
                             for (ii in 1:NfitP){  #For the FitComp[jj] build the correspondent Param Column
                                 if (PP[ii] > 0){
                                     chckd[ii,jj] <<- paste(ii, ".", jj, sep="")
                                     #T3LinkParam[[kk]][jj] == checkbox of FitParam==ParamLbl[P[ii]] of FitComp1[jj]
                                     T3LinkParam[[ii]][[jj]] <<- tkcheckbutton(LinkGroup2, text="", variable=chckd[ii,jj], onvalue = 1, offvalue = 0,
                                                                                 command=function(){ ParamCkCtrl() }  #handler to select the paramenter and the components to link
                                                                              )
                                     tkgrid(T3LinkParam[[ii]][[jj]], row = ii+1, column = jj+1, padx=10, pady=0, sticky="w")
                                     tclvalue(chckd[ii,jj]) <<- FALSE
                                 } else {
                                     #table of T3LinkParam works if all pointers to checkbox are present. No NA, NULL elements can be used
                                     #However it may happen that, depending on the lineshape, the actual FitComp doe not contains all the FitParam
                                     #then some checkbox, those generated below, must not be present in the table. To make them invisible
                                     #they are generated in a window with visible=FALSE

                                     T3LinkParam[[ii]][[jj]] <<- NA    #the checkbox is not generated and the pointer is NULL
                                 }
                             }
                         }

                         LinkFrame2 <- ttklabelframe(LinkGroup1, text = "REFERENCE COMPONENT", borderwidth=2)
                         tkgrid(LinkFrame2, row = 2, column=1, padx = 5, pady = 5, sticky="we")
                         msg2 <- ttklabel(LinkFrame2, text="Please select the reference fit component for the link", font=MyFont)
                         tkgrid(msg2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

                         LinkGroup3 <<- ttkframe(LinkFrame2, borderwidth=0, padding=c(0,0,0,0))
                         tkgrid(LinkGroup3, row = 2, column = 1, padx = 0, pady = 0, sticky="w")
                         RefLinkComp <<- list()
                         SelFitComp2 <<- tclVar()
                         for(jj in 1:length(FitComp2)){  #this checkbutton behaves as a radiobutt. Only 1 element can be checked
                             RefLinkComp[[jj]] <<- tkcheckbutton(LinkGroup3, text=FitComp2[jj], variable=SelFitComp2,
                                                                onvalue=FitComp2[jj], offvalue = 0)
                             tkgrid(RefLinkComp[[jj]], row = 1, column = jj, padx=5, pady=5, sticky="w")
                             tclvalue(SelFitComp2) <<- FALSE
                         }

                         LinkFrame3 <<- ttklabelframe(LinkGroup1, text = "LINK OPERATIONS", borderwidth=2)
                         tkgrid(LinkFrame3, row = 3, column=1, padx = 5, pady = 5, sticky="we")
                         linkExpression <<- ""
                         EXPR <- tclVar("Link Expression?")  #sets the initial msg
                         LinkExpr <<- ttkentry(LinkFrame3, textvariable=EXPR, width=40, foreground="grey")
                         tkbind(LinkExpr, "<FocusIn>", function(K){
                                           tkconfigure(LinkExpr, foreground="red")
                                           tclvalue(EXPR) <- ""
                         })
                         tkbind(LinkExpr, "<Key-Return>", function(K){
                                           tkconfigure(LinkExpr, foreground="black")
                                           linkExpression <<- tclvalue(EXPR)
                         })
                         tkgrid(LinkExpr, row = 1, column=1, padx=5, pady=5, sticky="w")

                         tkgrid( ttklabel(LinkFrame3, text="Example1: link mu(C2) = mu(C1) +1.3 eV"),
                                          row = 2, column=1, padx = 5, pady = 3, sticky="w")
                         tkgrid( ttklabel(LinkFrame3, text="=>   check mu(C2)   check C1 as reference   Set Link Expression = +1.3"),
                                          row = 3, column=1, padx = 5, pady = 3, sticky="w")
                         tkgrid( ttklabel(LinkFrame3, text="Example2: link h(C4) = h(C3) * 0.5"),
                                          row = 4, column=1, padx = 5, pady = 3, sticky="w")
                         tkgrid( ttklabel(LinkFrame3, text="=>   check h(C4)   check C3 as reference   Set Link Expression = *0.5"),
                                          row = 5, column=1, padx = 5, pady = 3, sticky="w")


                         LinkGroup4 <- ttkframe(LinkGroup1, borderwidth=0, padding=c(0,0,0,0))
                         tkgrid(LinkGroup4, row = 9, column = 1, padx = 0, pady = 0, sticky="w")

                         SetBtn <- tkbutton(LinkGroup4, text="  SET LINKS  ", width=20, command=function(){
                                           #regenerate tkEntry to make it working properly
                                           tkdestroy(LinkExpr)
                                           tclvalue(EXPR) <- "Link Expression?"
                                           LinkExpr <<- ttkentry(LinkFrame3, textvariable=EXPR, width=40, foreground="grey")
                                           tkbind(LinkExpr, "<FocusIn>", function(K){
                                                     tkconfigure(LinkExpr, foreground="red")
                                                     tclvalue(EXPR) <- ""
                                                 })
                                           tkbind(LinkExpr, "<Key-Return>", function(K){
                                                     tkconfigure(LinkExpr, foreground="black")
                                                     linkExpression <<- tclvalue(EXPR)
                                                 })
                                           tkgrid(LinkExpr, row = 1, column=1, padx=5, pady=5, sticky="w")

                                           component2 <<- tclvalue(SelFitComp2)
#-------------------------------------------------------------------------------------------------------------------------------
#                                          #control on the linked sigma parameter:
#                                          #link sigma of different fitting functions (for example LINK Sigma(Gauss) to Sigma(Voigt)
#                                          #if link was set, the operation is blocked and an error message raised
#                                          FitFnct1 <- FName[[SpectIndx]]@Components[[component2]]@funcName
#                                          for(ii in 1:NfitP){
#                                              if (is.na(PList[ii])==FALSE && PList[ii] == "sigma"){
#                                                  for(jj in 1:NfitC){
#                                                      Chkd <- svalue(T3LinkParam[[ii]][[jj]])
#                                                      FitFnct2 <- FName[[SpectIndx]]@Components[[jj]]@funcName
#                                                      if (FitFnct2 != FitFnct1 && Chkd==TRUE){
#                                                          txt <- paste("Cannot link sigma of different fitting functions: \n", FitFnct1, "  ", FitFnct2, sep="")
#                                                          tkmessageBox(message=txt, title="ERROR", icon="error")
#                                                          ResetLinks()
#                                                          return()
#                                                      }
#                                                  }
#                                              }
#                                          }
#-------------------------------------------------------------------------------------------------------------------------------
                                           if (length(component1)==0 || length(component2)==0){
                                               tkmessageBox(message="Error: Component to link or Reference Component not set!", title ="WRONG COMPONENT SELECTION", icon="error")
                                               return()
                                           }
                                           operation <<- "link"
                                           setCommand()
                                           ResetLinks()  #after link selection, resets checks and prepare for further link setting

                         })
                         tkgrid(SetBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

                         ResetBtn <- tkbutton(LinkGroup4, text="  RESET LINKS  ", width=20, command=function(){
                                           ResetLinks()
                         })
                         tkgrid(ResetBtn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

                         ExitBtn <- tkbutton(LinkGroup4, text="  EXIT  ", width=20, command=function(){
                                            tkdestroy(LinkFCwin)
                         })
                         tkgrid(ExitBtn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")
      } #HndlrSetLinks() end


      T3frame1 <- ttklabelframe(T3group1, text = "SET LINKS", borderwidth=2)
      tkgrid(T3frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

      T3ButtLinkTbl <- tkbutton(T3frame1, text="OPEN PARAMETER TABLE", width=30, command=function(){
                           if(length(FName[[SpectIndx]]@Components) < 2){
                              tkmessageBox(message="Not enough Fitting Components to set Links", title="WARNING", icon="warning")
                              return()
                           }
                           HndlrSetLinks()
                     })
      tkgrid(T3ButtLinkTbl, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

      T3frame2 <- ttklabelframe(T3group1, text = "RESET LINKS", borderwidth=2)
      tkgrid(T3frame2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

      tkgrid(ttklabel(T3frame2, text="Select Fit Component"),
                     row = 1, column = 1, padx = 5, pady = 5, sticky="w")
      T3FitCompA <- tclVar()
      T3obj1 <- ttkcombobox(T3frame2, width = 15, textvariable = T3FitCompA, values = FitComp1)
      tkgrid(T3obj1, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

      T3ButtRmvCnst <- tkbutton(T3frame2, text="RESET COMPONENT CONSTRAINTS", width=30, command=function(){
                           component1 <<- tclvalue(T3FitCompA)
                           if (length(component1)==0){
                               tkmessageBox(message="Please select the fit component for resetting links!", title="ERROR", icon="error")
                               return()
                           }
                           operation <<- "remove"   #if present remove links on selected FitComponents
                           setCommand()
                           tclvalue(T3FitCompA) <- ""      #reset parameter to links
                     })
      tkgrid(T3ButtRmvCnst, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

      T3ButtRmvAll <- tkbutton(T3frame2, text="REMOVE ALL CONSTRAINTS", width=30, command=function(){
                           SigmaCtrl <<- list(FitComp=c(1:NComp), CompLnkd=NULL, ToComp=NULL, Expression=NULL)
                           component1 <<- names(FName[[SpectIndx]]@Components) #construct the list of linked components
                           operation <<- "remove" #all the FitComponents will be controlled and set links removed
                           LinkIndx[["P"]] <<- NULL   #reset the Param, FitComp indexes of the first selection
                           LinkIndx[["C"]] <<- NULL
                           FitComp2 <<- FitComp1
                           setCommand()
                     })
      tkgrid(T3ButtRmvAll, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

# --- End of notebook ---


# --- Page in common ---
      Optframe <- ttklabelframe(MainGroup, text = "OPTIONS", borderwidth=2)
      tkgrid(Optframe, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

      CLframe <- ttklabelframe(Optframe, text = "SELECT CORELINE", borderwidth=2)
      tkgrid(CLframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

      CL <- tclVar("")
      CLobj1 <- ttkcombobox(CLframe, width = 15, textvariable = CL, values = SpectList)
      tkbind(CLobj1, "<<ComboboxSelected>>", function(){
                           Saved <<- FALSE
                           XPSComponent <- tclvalue(CL)
                           XPSComponent <- unlist(strsplit(XPSComponent, "\\."))   #remove number at CoreLine beginning
                           SpectIndx <<- as.numeric(XPSComponent[1])
                           SpectName <<- XPSComponent[2]
                           assign("activeSpectName", SpectName,envir=.GlobalEnv) #set the active XPSSample be the lasr loaded file
                           assign("activeSpectIndx", SpectIndx,envir=.GlobalEnv) #set the active spectrum index
                           FName <<- get(activeFName,envir=.GlobalEnv)
#                           FitComp1A <<- FitComp1B <<- ""         #reset FitComp1A, FitComp1B
#                           FitComp1A <<- na.omit(FitComp1[1:8])   #splits a long series of components in two groups of 8 components each
#                           FitComp1B <<- na.omit(FitComp1[9:16])  #if FitComp1B is void gcheckbox is automatically not generated
                           FitComp1 <<- names(FName[[SpectIndx]]@Components)
                           NComp <<- length(FName[[SpectIndx]]@Components)
                           plot(FName[[SpectIndx]])

#--- Reset Notebook pages ---

#--- pag1
                           tkconfigure(T1obj1, values=FitComp1)
                           tkconfigure(T1obj2, values=FitComp1)
                           tclvalue(SetRelSensFact) <<- ""
#--- pag2
                           tkconfigure(T2obj1, values=FitComp1)
                           tclvalue(T2FitCompA) <<- ""
                           tclvalue(T2Param) <<- ""
                           tclvalue(T2SetParamVal) <<- ""
#--- pag3
                           tclvalue(T3FitCompA) <<- ""

#--- reset parameters
                           operation <<- ""
                           parameter <<- ""
                           linkExpression <<- ""
                           setValue <<- ""
                           component1 <<- ""
                           component2 <<- ""
                           NewParam <<- ""
                           SigmaCtrl <<- list(FitComp=NULL, CompLnkd=NULL, ToComp=NULL, Expression=NULL)
                           fitParam <<- NULL
                           ParamList <<- ""
                           OldFName <<- FName

                     })
      tkgrid(CLobj1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

      FitButtLM <- tkbutton(Optframe, text="FIT Lev.Marq.", width=20, command=function(){
                           if (Saved){
                              FName[[SpectIndx]] <<- XPSFitLM(FName[[SpectIndx]], plt=FALSE, verbose=TRUE)
                              plot(FName[[SpectIndx]])
                           } else {
                              tkmessageBox(message="Please save the Constraints before running the fit", title="WARNING", icon = "warning")
                           }
                           plot(FName[[SpectIndx]])
                     })
      tkgrid(FitButtLM, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

      FitButtMod <- tkbutton(Optframe, text="FIT ModFit", width=20, command=function(){
                           if(is.na(match("FME", Pkgs)) == TRUE ){   #check if the package 'FME' is installed
                              txt <- "Package 'FME' not installed. \nOption 'ModFit' not available"
                              tkmessageBox(message=txt, title="WARNING", icon="error")
                              return()
                           }
                           if (Saved){
                              FName[[SpectIndx]] <<- XPSModFit(FName[[SpectIndx]])#XPSMoveCompoonent GUI is active to beused in combination with XPSConstraintsGUI
                              plot(FName[[SpectIndx]])
                           } else {
                              tkmessageBox(message="Please save the Constraints before running the fit", title="WARNING!", icon = "warning")
                           }
                           plot(FName[[SpectIndx]])
                     })
      tkgrid(FitButtMod, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

      ButtUNdo <- tkbutton(Optframe, text="UNDO", width=20, command=function(){
                           FName[[SpectIndx]] <<- OldFName[[SpectIndx]]
                           plot(FName[[SpectIndx]])
                     })
      tkgrid(ButtUNdo, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

      ButtSave <- tkbutton(Optframe, text="SAVE", width=20, command=function(){
                           if(length(SigmaCtrl$CompLnkd) > 0 && Saved == FALSE) { #there are links on sigma still not controlled
                              SetLinks()                          #first all the links have to be set then they can be controlled and set!!!
                           }
                           SpectName <<- names(FName)[SpectIndx]  #name of the active CoreLine
                           assign(activeFName, FName, envir=.GlobalEnv)
                           assign("activeSpectName", SpectName, envir=.GlobalEnv)
                           assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
                           Saved <<- TRUE
                           plot(FName[[SpectIndx]])
                           XPSSaveRetrieveBkp("save")
                           OldFName[[SpectIndx]] <<- FName[[SpectIndx]]
                     })
      tkgrid(ButtSave, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

      ButtReload <- tkbutton(Optframe, text="RE-LOAD DATA", width=20, command=function(){
                           FName <<- get(activeFName, envir=.GlobalEnv)
                           Saved <<- FALSE
                           OldFName <<- FName
                           plot(FName[[SpectIndx]])
                     })
      tkgrid(ButtReload, row = 6, column = 1, padx = 5, pady = 5, sticky="w")

      ButtExit <- tkbutton(Optframe, text="EXIT", width=20, command=function(){
                           SpectName <<- names(FName)[SpectIndx]  #name of the active CoreLine
#                           assign(activeFName, FName, envir=.GlobalEnv)
#                           assign("activeSpectName", SpectName, envir=.GlobalEnv)
#                           assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
                           if (Saved){
                              tkdestroy(mainFCwin)
                           } else {
                              answ <- tkmessageBox(message="Data NOT saved! Do you want to exit?", type="yesno", title="WARNING!", icon="warning")
                              if (tclvalue(answ) == "yes"){
                                  tkdestroy(mainFCwin)
                              }
                           }
                           XPSSaveRetrieveBkp("save")
                           plot(FName[[SpectIndx]])
                           UpdateXS_Tbl()
                     })
      tkgrid(ButtExit, row = 7, column = 1, padx = 5, pady = 5, sticky="w")

      tclvalue(NB) <- 3

} #end of constraints
