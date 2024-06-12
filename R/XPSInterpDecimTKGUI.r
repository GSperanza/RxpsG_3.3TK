#Function to interpolate or decimate spectral data

#' @title XPSInterpDecim function to interpolate or decimate spectral data
#' @description function XPSInterpDecim acts on on objects of class 'XPSCoreLine'
#'   The original X, Y data are interpolated or downsampled (decimation) adding or eliminating
#'   N-data for each element of the original sequence
#'   No parameters are passed to this function
#' @examples
#' \dontrun{
#'	XPSInterpDecim()
#' }
#' @export
#'


XPSInterpDecim <- function() {

    CtrlDtaAnal <- function(){
       if (hasBaseline(XPSSample[[Indx]])){
           txt <- "Interpolation and Decimation act only on original non-analyzed data.\n
                   Interpolated decimated data will be saved in a new core-line.\n
                   Do you want to proceed?"
           answ <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
           if (tcl(answ) == 1) {
               WidgetState(ID.obj5, "normal")
               WidgetState(ID.obj6, "normal")
           } else {
               tkdestroy(ID.win)
               return()
           }
       }
    }

    Interpolate <- function(IdxFrom, IdxTo, Npti){
       XX2 <- NULL #interpolated X values
       YY2 <- NULL #interpolated Y values
       XX <- XPSSample[[IdxFrom]]@.Data[[1]]
       YY <- XPSSample[[IdxFrom]]@.Data[[2]]
       LL <- length(XX)
       cat("\n\n ==> Initial number of data:", LL)
       kk <- 1
       dx <- (XX[2] - XX[1]) / Npti
       for(ii in 1:(LL-1)){ # by interpolation of adjacent data
           dy <- (YY[ii + 1] - YY[ii]) / Npti
           for(jj in 1:Npti){
               XX2[kk] <- XX[ii] + (jj - 1)*dx
               YY2[kk] <- YY[ii] + (jj - 1)*dy
               kk <- kk+1
           }
       }
       XX2[kk] <- XX[LL]
       YY2[kk] <- YY[LL]
       cat("\n ==> Number of data interpolated core-line:", kk)
       Symbol <- XPSSample[[IdxFrom]]@Symbol
       Symbol <- paste("Intp.", Symbol, sep="")
       XPSSample[[IdxTo]]@Symbol <<- Symbol
       XPSSample@names[IdxTo] <<- Symbol
       XPSSample[[IdxTo]]@.Data[[1]] <<- XX2
       XPSSample[[IdxTo]]@.Data[[2]] <<- YY2
       cat("\n ==> New core-line Energy step:", abs(XPSSample[[IdxTo]]@.Data[[1]][2]-XPSSample[[IdxTo]]@.Data[[1]][1]))
    }

    Decimate <- function(IdxFrom, IdxTo, Npti){
       LL <- length(XPSSample[[IdxFrom]]@.Data[[1]])
       cat("\n\n ==> Initial number of data:", LL)
       XX2 <- NULL #decimated X values
       YY2 <- NULL #decimated Y values
       jj <- 1
       XX2[jj] <- XPSSample[[IdxFrom]]@.Data[[1]][1]
       YY2[jj] <- XPSSample[[IdxFrom]]@.Data[[2]][1]
       for(ii in seq(from=1, to=LL, by=Npti)){
           XX2[jj] <- XPSSample[[IdxFrom]]@.Data[[1]][ii]
           YY2[jj] <- XPSSample[[IdxFrom]]@.Data[[2]][ii]
           jj <- jj+1
       }
       cat("\n ==> Number of data decimated core-line:", (jj-1))
       Symbol <- XPSSample[[IdxFrom]]@Symbol
       Symbol <- paste("Dec.", Symbol, sep="")
       XPSSample[[IdxTo]]@Symbol <<- Symbol
       XPSSample@names[IdxTo] <<- Symbol
       XPSSample[[IdxTo]]@.Data[[1]] <<- XX2
       XPSSample[[IdxTo]]@.Data[[2]] <<- YY2
       cat("\n ==> New core-line Energy step:", abs(XPSSample[[IdxTo]]@.Data[[1]][2]-XPSSample[[IdxTo]]@.Data[[1]][1]))
    }

    ResetVars <- function(){
       XPSSample <<- NULL
       Indx <<- NULL
       SelectedFName <- NULL
       FNameList <<- XPSFNameList()
       SpectList <- " "
       Estep1 <<- NULL
       Estep2 <<- NULL
       Operation <<- ""
       tclvalue(XS) <<- ""
       tclvalue(CL) <<- ""
       tkconfigure(LabDE1, text="Orig. E-step:                 ")
       tclvalue(ES) <<- ""
       tclvalue(NPTD) <<- ""
       tclvalue(NPTI) <<- ""
       txt <- paste("Operation:                                            \n",
                "                                                        ", sep="")
       tkconfigure(LabOperation, text=txt, font="Serif 10 bold")
    }


#--- Variables
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   XPSSample <- NULL
   Indx <- NULL
   SelectedFName <- NULL
   FNameList <- XPSFNameList()
   SpectList <- " "
   Estep1 <- NULL
   Estep2 <- NULL
   Operation <- ""

#--- widget ---
   ID.win <- tktoplevel()
   tkwm.title(ID.win,"SPECTRAL DATA INTERPOLATION - DECIMATION")
   tkwm.geometry(ID.win, "+100+50")   #position respect topleft screen corner

   ID.group1 <- ttkframe(ID.win, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(ID.group1, row = 1, column = 1, padx = 0, pady = 0, sticky="w")
   ID.group2 <- ttkframe(ID.group1, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(ID.group2, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

#---
   ID.frame1 <- ttklabelframe(ID.group2, text="SELECT XPSsample", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(ID.frame1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
   XS <- tclVar()
   ID.obj1 <- ttkcombobox(ID.frame1, width = 20, textvariable = XS, values = FNameList)
   tkbind(ID.obj1, "<<ComboboxSelected>>", function(){
                         SelectedFName <<- tclvalue(XS)
                         XPSSample <<- get(SelectedFName, envir=.GlobalEnv)#load selected XPSSample
                         Gdev <- unlist(XPSSettings$General[6])            #retrieve the Graphic-Window type
                         Gdev <- strsplit(Gdev, "title")
                         Gdev <- paste(Gdev[[1]][1], " title='",SelectedFName,"')", sep="")     #add the correct window title
                         graphics.off() #switch off the graphic window
                         eval(parse(text=Gdev),envir=.GlobalEnv) #switches the new graphic window ON
                         plot(XPSSample)
                         SpectList <<- XPSSpectList(SelectedFName)
                         tkconfigure(ID.obj2, values=SpectList)
                  })
   tkgrid(ID.obj1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   CL <- tclVar()
   ID.frame2 <- ttklabelframe(ID.group2, text="SELECT CORELINE", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(ID.frame2, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
   ID.obj2 <- ttkcombobox(ID.frame2, width = 20, textvariable = CL, values = "  ")
   tkbind(ID.obj2, "<<ComboboxSelected>>", function(){
                         SpectName <- tclvalue(CL)
                         SpectName <- unlist(strsplit(SpectName, "\\."))   #tolgo il N. all'inizio del nome coreline
                         Indx <<- as.integer(SpectName[1])
                         CtrlDtaAnal()
                         Estep1 <<- abs(XPSSample[[Indx]]@.Data[[1]][2] - XPSSample[[Indx]]@.Data[[1]][1])
                         Estep1 <<- round(Estep1, 2)
                         txt <- paste("Orig. E-step: ", Estep1,"                 ", sep="")
                         txt <- substr(txt, start=1, stop=30)
                         tkconfigure(LabDE1, text=txt)
                         assign("activeSpectName", SpectName[2], envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                         assign("activeSpectIndx", Indx, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                         plot(XPSSample[[Indx]])
                  })
   tkgrid(ID.obj2, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#---
   ID.frame3 <- ttklabelframe(ID.group1, text="CHANGE THE ENERGY STEP", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(ID.frame3, row = 2, column = 1, padx = 5, pady = 3, sticky="w")

   LabDE1 <- ttklabel(ID.frame3, text="Orig. E-step:                 ", font="Serif 11 bold")
   tkgrid(LabDE1, row = 1, column = 1, padx = c(5, 20), pady = 3, sticky="w")
   HelpNdta <- tkbutton(ID.frame3, text=" HELP ", width=15, command=function(){
                         txt <- " After Core-Line selection the original Energy step is shown.\n Enter the new value of the energy step DE. RxpsG will INTERPOLATE\n or DECIMATE to provide spectrawith the chosen DE"
                         tkmessageBox(message=txt, title="HELP", icon="info")
                  })
   tkgrid(HelpNdta, row = 1, column = 2, padx = c(20, 5), pady = 3, sticky="e")

   tkgrid( ttklabel(ID.frame3, text=" New Energy Step: "),
           row = 2, column = 1, padx = 5, pady = 3, sticky="w")
   ES <- tclVar("E.step ?")  #sets the initial msg
   ID.obj4 <- ttkentry(ID.frame3, textvariable=ES, foreground="grey")
   tkbind(ID.obj4, "<FocusIn>", function(K){
                         tclvalue(ES) <- ""
                         tkconfigure(ID.obj4, foreground="red")
                  })
   tkbind(ID.obj4, "<Key-Return>", function(K){
                         tkconfigure(ID.obj4, foreground="black")
                         Estep2 <<- as.numeric(tclvalue(ES))
                         Operation <<- "NewEstep"
                  })
   tkgrid(ID.obj4, row = 3, column = 1, padx = 5, pady = 3, sticky="w")

#---
   ID.frame5 <- ttklabelframe(ID.group1, text="DECIMATE", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(ID.frame5, row = 4, column = 1, padx = 5, pady = 3, sticky="w")

   NPTD <- tclVar("N.pti ?")  #sets the initial msg
   ID.obj5 <- ttkentry(ID.frame5, textvariable=NPTD, foreground="grey")
   tkbind(ID.obj5, "<FocusIn>", function(K){
                         tclvalue(NPTD) <- ""
                         tkconfigure(ID.obj5, foreground="red")
                  })
   tkbind(ID.obj5, "<Key-Return>", function(K){
                         tkconfigure(ID.obj5, foreground="black")
                         Npti <- as.numeric(tclvalue(NPTD))
                         if((Npti-trunc(Npti)) > 0){
                             tkmessageBox(message="N. points to Decimate must be an integer", title="ERROR", icon="error")
                             return()
                         }
                         Operation <<- "Decimate"
                  })
   tkgrid(ID.obj5, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

#---
   ID.frame6 <- ttklabelframe(ID.group1, text="INTERPOLATE", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(ID.frame6, row = 5, column = 1, padx = 5, pady = 3, sticky="w")

   NPTI <- tclVar("N.pti ?")  #sets the initial msg
   ID.obj6 <- ttkentry(ID.frame6, textvariable=NPTI, foreground="grey")
   tkbind(ID.obj6, "<FocusIn>", function(K){
                         tclvalue(NPTI) <- ""
                         tkconfigure(ID.obj6, foreground="red")
                  })
   tkbind(ID.obj6, "<Key-Return>", function(K){
                         tkconfigure(ID.obj6, foreground="black")
                         Npti <- as.numeric(tclvalue(NPTI))
                         if((Npti-trunc(Npti)) > 0){
                             tkmessageBox(message="N. points to Decimate must be an integer", title="ERROR", icon="error")
                             return()
                         }
                         Operation <<- "Interpolate"
                  })
   tkgrid(ID.obj6, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

#---
   ID.frame7 <- ttklabelframe(ID.group1, text="OPERATION", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(ID.frame7, row = 6, column = 1, padx = 5, pady = 3, sticky="w")
   txt <- paste("Operation:                                                             \n",
                "                                                                         ", sep="")
   LabOperation <- ttklabel(ID.frame7, text=txt, font="Serif 10 bold")
   tkgrid(LabOperation, row = 1, column = 1, padx = 5, pady = 3, sticky="w")

#---
   ID.frame8 <- ttklabelframe(ID.group1, text="PROCESSING", borderwidth=3, padding=c(5,5,5,5) )
   tkgrid(ID.frame8, row = 7, column = 1, padx = 5, pady = 5, sticky="w")

   ID.obj8 <- tkbutton(ID.frame8, text="COMPUTE", width=20, command=function(){
                         IdxFrom <- Indx
                         IdxTo <- length(XPSSample)+1
                         txt <- ""
                         XPSSample[[IdxTo]] <<- new("XPSCoreLine",
						                                          	.Data = list(x = NULL, y = NULL, t=NULL),   #X, Y, T=Analizer, transfer function, and dummy column for future use
							                                          units = XPSSample[[IdxFrom]]@units,
							                                          Flags = XPSSample[[IdxFrom]]@Flags,
							                                          Info = "",
							                                          Symbol = "")
                         switch(Operation, "NewEstep" = {
                              if (Estep1 < Estep2){ #decimation
                                  RR <- Estep2/Estep1
                                  RR <- round(RR, 3)
                                  Dec <- (RR-trunc(RR)) #gives the decimals
                                  if (Dec == 0){
                                      Npti <- round(Estep2/Estep1, 0) #result is different from as.integer()
                                      txt <- paste(txt, "Operation: DECIMATION by ", Npti, " \n\n", sep="")
                                      tkconfigure(LabOperation, text=txt)
                                      Decimate(IdxFrom, IdxTo, Npti)
                                      plot(XPSSample[[IdxTo]])
                                  }
                                  if (Dec > 0){
                                      Dec <- (Estep1-trunc(Estep1))
                                      ii <- 1
                                      while(Dec > 0){
                                         Npti <- Estep1*10^ii   # ii corresponds to the number of decimals
                                         Dec <- (Npti-trunc(Npti))
                                         ii <- ii+1
                                      }
                                      txt <- paste(txt, "Operation: INTERPOLATION by ", Npti, "\n", sep="")
                                      tkconfigure(LabOperation, text=txt)
                                      Interpolate(IdxFrom, IdxTo, Npti)
                                      Estep1 <- Estep1/Npti  #Estep1 = 1.2 =>  Npti = 12  =>  Estep2/Npti = 0.1
                                      Npti <- Estep2/Estep1
                                      txt <- paste(txt, "Operation: DECIMATION by ", Npti, sep="")
                                      tkconfigure(LabOperation, text=txt)
                                      Decimate(IdxTo, IdxTo, Npti)
                                      plot(XPSSample[[IdxTo]])
                                  }
                              }
                              if (Estep1 > Estep2){ #interpolation
                                  RR <- Estep1/Estep2
                                  RR <- round(RR, 3)
                                  Dec <- (RR-trunc(RR)) #gives the decimals
                                  if (Dec == 0){
                                      Npti <- round(Estep1/Estep2, 0)
                                      txt <- paste(Operation, "Operation: INTERPOLATION by ", Npti, " \n````````````````\n", sep="")
                                      tkconfigure(LabOperation, text=txt)
                                      Interpolate(IdxFrom, IdxTo, Npti)
                                      plot(XPSSample[[IdxTo]])
                                  }
                                  if (Dec > 0){
                                      Dec <- (Estep1-trunc(Estep1))
                                      ii <- 1
                                      while(Dec > 0){
                                         Npti <- Estep1*10^ii   # ii corresponds to the number of decimals
                                         Dec <- (Npti-trunc(Npti))
                                         ii <- ii+1
                                      }
                                      txt <- paste(txt, "Operation: DECIMATION by ", Npti, "\n", sep="")
                                      tkconfigure(LabOperation, text=txt)
                                      Decimate(IdxFrom, IdxTo, Npti)
                                      Estep1 <- Estep1/Npti  #Estep2 = 1.2 =>  EEstep2 = 12  =>  Estep2/EEstep2 = 0.1
                                      Npti <- Estep2/Estep1
                                      txt <- paste(txt, "Operation: INTERPOLATION by ", Npti, sep="")
                                      tkconfigure(LabOperation, text=txt)
                                      Interpolate(IdxTo, IdxTo, Npti)
                                      plot(XPSSample[[IdxTo]])
                                  }
                              }
                           },
                           "Decimate" = {
                               Npti <- as.numeric(tclvalue(NPTD))
                               txt <- paste(txt, "Operation: DECIMATION by ", Npti, " \n\n", sep="")
                               tkconfigure(LabOperation, text=txt)
                               Decimate(IdxFrom, IdxTo, Npti)
                           },
                           "Interpolate" = {
                               Npti <- as.numeric(tclvalue(NPTI))
                               txt <- paste(txt, "Operation: INTERPOLATION by ", Npti, " \n\n", sep="")
                               tkconfigure(LabOperation, text=txt)
                               Interpolate(IdxFrom, IdxTo, Npti)
                           } )
                  })
   tkgrid(ID.obj8, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   ID.obj9 <- tkbutton(ID.frame8, text="RESET", width=20, command=function(){
                         ResetVars()
                         XPSSaveRetrieveBkp("save")
                  })
   tkgrid(ID.obj9, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   ID.obj10 <- tkbutton(ID.frame8, text="SAVE", width=20, command=function(){
                         assign(SelectedFName, XPSSample, envir=.GlobalEnv) #setto lo spettro attivo eguale ell'ultimo file caricato
                         XPSSaveRetrieveBkp("save")
                         plot(XPSSample)
                  })
   tkgrid(ID.obj10, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

   ID.obj11 <- tkbutton(ID.frame8, text="EXIT", width=20, command=function(){
                         XPSSaveRetrieveBkp("save")
                         tkdestroy(ID.win)
                  })
   tkgrid(ID.obj11, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
   
   WidgetState(ID.obj5, "disabled")
   WidgetState(ID.obj5, "disabled")

}
