#XPSImport.Ascii GUI to make reading/loading ascii file

#' @title XPSImport.Ascii function
#' @description Function allowing import of textual (ascii) file data
#'   Options are available to account for header and data separator
#'   Options are also provided to store data in an object of class 'XPSSample'
#'   XPSImport.Ascii saves the imported data in the .GlobalEnv
#' @examples
#' \dontrun{
#'  XPSImport.Ascii()
#'  activeFName <- get("activeFName", envir=.GlobalEnv)
#'  XPSSample <- get(activeFName, envir=.GlobalEnv)
#' }
#' @export
#'

XPSImport.Ascii <- function() {
    options(guiToolkit = "tcltk")

#--- warnings if required selections are lacking
    check_selection <- function(){
         if (nchar(tclvalue(XC)) * nchar(tclvalue(YC))==0) {  #X and Y columns must be indicated
              tkmessageBox(message="Please give ColX and ColY to be imported", title="WARNING: column lacking", icon="warning")
              return(FALSE)
         }
         if ( tclvalue(RevYN) != "1" && tclvalue(RevYN) != "0") {   #one of reverseX or NOreverseX should be SELECTED
              tkmessageBox(message="Reverse X axis? Plsease check!", title="WARNING X axis!", icon="warning")
              return(FALSE)
         }
         if ( nchar(tclvalue(CLNm)) == "0") {   #Column name must be indicated
              tkmessageBox(message="Core Line Name please!", title="WARNING Core Line Name", icon="warning")
              return(FALSE)
         }
         return(TRUE)
      }

#--- read data from file
      Read_Data <- function(...) {
          Nrws <- as.numeric(tclvalue(NR))
          if (tclvalue(SCN) == "0"){  #Scan file non selected
             Data <<- read.table(file=FNameIN, sep=Opt$Sep, dec=Opt$Dec,
                          skip=Nrws, colClasses="numeric", fill=TRUE )

             Ncol <- ncol(Data)
          } else {
             fp <- file(FNameIN, open="r")
             Ncol <- as.numeric(tclvalue(DNC))
             tmp <- " " #just to make length(tmp) > 0
             Data <<- NULL   #ora leggo i dati
             while (length(tmp) > 0) {
                 tmp <- scan(fp, what="character", n=Ncol, quiet=TRUE)
                 tmp <- sub(", ", "  ", tmp)   #changes separation "," with " ": for data  1, 2,143, 5,723  generates  1  2,143  5,723
                 tmp <- sub(",", ".", tmp)     #changes decimal "," with ".": for data  1  2,143  5,723  generates  1  2.143  5.723
                 if (is.na(as.numeric(tmp))) break #stop reading if there are characters which cannot translated in numbers
                 Data <<-  rbind(Data, as.numeric(tmp))
             }

          }
          Data <<- as.data.frame(Data, stringsAsFactors=FALSE)

          tkdestroy(DataIN) #remove previous table
          ColNames <- NULL
          CWidth <- NULL
          for(ii in 1:Ncol){
              ColNames[ii] <- paste("---C", ii,"---", sep="")
              CWidth[ii] <- 60
          }
          if ((Ncol*60) > WW) { 
              HH <<- HH+20   #create space for the X-scrollbar
              tkconfigure(LoadFrame, width=WW, height=HH)

          ## child needs to configure columns, displaycolumns, show
              xscr <- ttkscrollbar(LoadFrame, orient="horizontal",
                               command=function(...) tkxview(DataIN,...))
              yscr <- ttkscrollbar(LoadFrame, orient="vertical",
                               command=function(...) tkyview(DataIN,...))
          } else {
              xscr <- NULL #return()
              yscr <- NULL #return()
          }
          DataIN <- ttktreeview(LoadFrame,
                      show="headings",
                      columns = ColNames,
                      selectmode = "browse",
                      xscrollcommand=function(...) tkset(xscr,...),
                      yscrollcommand=function(...) tkset(yscr,...)
          )

          #add headings and set treeview column width
          for(ii in 1:Ncol){
              tcl(DataIN, "heading", (ii-1), text=ColNames[ii])
              tcl(DataIN, "column", (ii-1), width=CWidth[ii])
          }

          #populate treeview
          apply(Data, 1, function(x) {
                tcl(DataIN, "insert", "", "end", values = x )
               })

          #attention to the order of the various tkgrids in particular tkgrid.propagate()
          tkgrid(DataIN, row = 1, column = 1, sticky="news")
          tkgrid.propagate(LoadFrame, FALSE)  #maintains the frame dimensions fixed to WW x HH
          tkgrid.columnconfigure(LoadFrame, 1, weight=1)
          tkgrid.rowconfigure(LoadFrame, 1, weight=1)
          if ( ! is.null(xscr)){
              tkgrid(xscr, row=2, column=1, sticky="ew")
#              tkgrid(yscr, row=1, column=2, sticky="ns")
          }
          tkconfigure(LoadFrame, width = WW, height=HH)
      }

#--- Add a new XY data in a New CoreLine in an existing XPSSample
      addCoreLine <- function(){
              Xidx <- as.numeric(tclvalue(XC))
              Yidx <- as.numeric(tclvalue(YC))
              XX <- YY <- NULL
              XX <- na.omit(Data[[Xidx]])  #if present remove NA from read data
              YY <- na.omit(Data[[Yidx]])
              LL <- length(XX)
              LLy <- length(YY)
              if (tclvalue(RevYN) == "1" && XX[1] < XX[LL]) { #reverse X axis selected but X is ascending ordered
                 answ <- tkmessageBox(message="X is in ascending order. Do you want to reverse X axis? ", 
                                      type="yesno", title="CONFIRM REVERSE AXIS", AICON="WARNING")
                 if (tclvalue(answ) == "yes" ){
                    XX <- rev(XX) #reverse X in descending order
                    YY <- rev(YY) #reverse Y in descending order
                 } else {
                    tclvalue(RevYN) <- "0"
                 }
              }
              LL <- length(XPSSample)+1
              NewCL <- new("XPSCoreLine",
                    .Data = list(x = XX, y = YY, t=NULL, err=NULL),   #err is dedicated to standard errors on Y data
                    units = c(tclvalue(XSC), tclvalue(YSC)),
                    Flags = c(as.logical(as.numeric(tclvalue(RevYN))), TRUE, FALSE, FALSE),
                    Symbol= tclvalue(CLNm)
                   )
              CLnames <- names(XPSSample)
              XPSSample[[LL]] <<- NewCL
              names(XPSSample) <<- c(CLnames, as.character(tclvalue(CLNm)))
              assign("activeFName", activeFName, envir=.GlobalEnv)  #Set the activeSpectName to the last name of imported data
              tkconfigure(XColRead, foreground="grey")
              tkconfigure(YColRead, foreground="grey")
              tkconfigure(ErColRead, foreground="grey")
              tclvalue(ER) <- "?"
              plot(XPSSample)
       }

#--- Add a new XY data in a New CoreLine in an existing XPSSample
      addErrors <- function(){
              if (length(tclvalue(ER)) == 0) {
                  tkmessageBox(message="PLEASE SELECT THE ERR-Y COLUMN", title="Err-Y column Lacking", icon="warning")
                  return()
              }
              Xidx <- as.numeric(tclvalue(XC))
              Yidx <- as.numeric(tclvalue(YC))
              Erridx <- as.numeric(tclvalue(ER))
              ER <- XX <- YY <- NULL
              XX <- na.omit(Data[[Xidx]])
              YY <- na.omit(Data[[Yidx]])
              ER <- na.omit(Data[[Erridx]])
              LL <- length(XX)
              if (tclvalue(RevYN) == "1" && XX[1] < XX[LL]) { #reverse X axis selected but X is ascending ordered
                 answ <- tkmessageBox(message="X is in ascending order. Do you want to reverse X axis? ", type="yesno",  title="CONFIRM REVERSE AXIS", AICON="WARNING")
                 if (answ == TRUE ){
                    XX <- rev(XX) #reverse X in descending order
                    YY <- rev(YY) #reverse X in descending order
                 } else {
                    tclvalue(RevYN) <- "0"
                 }
              }
              LL <- length(XPSSample)
              XPSSample[[LL]]@.Data[[4]] <<- ER
              assign("activeFName", activeFName, envir=.GlobalEnv)  #Set the activeSpectName to the last name of imported data
              tkmessageBox(message="Y-ERRORS LOADED. USE CUSTOMPLOT TO DRAW DATA+ERRORS", title="Plot data", icon="warning")
              tkconfigure(XColRead, foreground="grey")
              tkconfigure(YColRead, foreground="grey")
              tkconfigure(ErColRead, foreground="grey")
              tclvalue(ER) <- "?"
              plot(XPSSample)
       }


#--- Variables ---
       FNameIN <- NULL
       FName <- NULL
       XPSSample <- new("XPSSample",
                         Project = " ",
                         Comments = " ",
                         User=Sys.getenv('USER'),
                         Filename="" )
       activeFName <- NULL
       WW <- NULL
       HH <- NULL
       Opt <- list(Sep=NULL, Dec=NULL, Qte=NULL)
       Data <- NULL

#--- Widget ---
       ImportWin <- tktoplevel()
       tkwm.title(ImportWin,"IMPORT ASCII")
       tkwm.geometry(ImportWin, "+100+50")

       MainGroup <- ttkframe(ImportWin, borderwidth=0, padding=c(0,0,0,0) )
       tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

       LeftGroup <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
       tkgrid(LeftGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

       RightGroup <- ttkframe(MainGroup, borderwidth=0, padding=c(0,0,0,0) )
       tkgrid(RightGroup, row = 1, column = 2, padx = 0, pady = 0, sticky="w")

##### LEFT #####

#--- Select File ---
       LoadButt <- tkbutton(LeftGroup, text="Open Data File", width=20, command=function(){
                           XPSSamplesList <- XPSFNameList(warn=FALSE)
                           idx <- grep(XPSSample@Filename, XPSSamplesList)
                           if (length(idx) == 0 && length(XPSSample) > 0) {
                               txt <- paste("Import Data in the Previous XPSSample ==> YES\n",
                                            "Save Previous Data ==> NO", collapse="")
                               answ <- tkmessageBox(message=txt, type="yesno", title="WARNING", icon="warning")
                               if (tclvalue(answ) == "no"){
                                   assign(activeFName, XPSSample, envir=.GlobalEnv) #save previous XPSSample
                                   XPSSample <<- new("XPSSample",
                                                      Project = " ",
                                                      Comments = " ",
                                                      User=Sys.getenv('USER'),
                                                      Filename="" )
                               }
                           }
                           Filters <- matrix(c("txt Files", ".txt", "Ascii Files", ".asc", "Data Files", ".dat", "Prn Files", ".prn"), ncol=2, nrow=2, byrow=TRUE)
                           FNameIN <<- tk_choose.files(default = "", caption = "Select files",
                                                       multi = FALSE, filters = Filters)
                           activeFName <<- basename(FNameIN)
                           pathFile <- dirname(FNameIN)
                           setwd(pathFile)
                           #Read Ascii file and show in InputData Window
                           tkinsert(Raw_Input, "0.0", paste(readLines(FNameIN), collapse="\n")) #write report in ShowParam Win
                           WidgetState(Import_btn, "normal")
#                           WidgetState(Save_btn, "disabled")
                           WidgetState(AddXS_btn, "disabled")
                           WidgetState(SaveExit_btn, "disabled")
                  })
       tkgrid(LoadButt, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#--- read options
       HeaderFrame <- ttklabelframe(LeftGroup, text = "File Header", borderwidth=2)
       tkgrid(HeaderFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       tkgrid( ttklabel(HeaderFrame, text="Header"),
               row=2, column = 1, padx = 5, pady = 2, sticky="w")

       HYN <- tclVar("0")
       HRadio <- ttkradiobutton(HeaderFrame, text="Yes", variable=HYN,  value=1,
                       command=function(){
                                tclvalue(NR) <- "1"
                                WidgetState(NRowHeader, "normal")
                  })
       tkgrid(HRadio, row = 2, column = 2, padx = 5, pady = 2, sticky="w")
       HRadio <- ttkradiobutton(HeaderFrame, text="No", variable=HYN,  value=0,
                       command=function(){
                                tclvalue(NR) <- "0"
                                WidgetState(NRowHeader, "disabled")
                  })
       tkgrid(HRadio, row = 2, column = 2, padx = 50, pady = 5, sticky="w")

       tkgrid( ttklabel(HeaderFrame, text="Header Rows"),
               row = 3, column = 1, padx = 5, pady = 5, sticky="w")
       NR <- tclVar("0")  #sets the initial msg
       NRowHeader <- ttkentry(HeaderFrame, textvariable=NR, foreground="grey")
       tkbind(NRowHeader, "<FocusIn>", function(K){
                         tkconfigure(NRowHeader, foreground="red")
                         tclvalue(NR) <- ""
                     })
       tkbind(NRowHeader, "<Key-Return>", function(K){
                         tkconfigure(NRowHeader, foreground="black")
                     })
       tkgrid(NRowHeader, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
       WidgetState(NRowHeader, "disabled")

#---
       DtaFrame <- ttklabelframe(LeftGroup, text = "Data Organization", borderwidth=2)
       tkgrid(DtaFrame, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(DtaFrame, text="Separator"),
               row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       SEP <- tclVar("Unspecified")
       SepType <- c("Tab", "Whitespace", "Comma", "Semicolon", "Unspecified")
       SepCombo <- ttkcombobox(DtaFrame, width = 15, textvariable = SEP, values = SepType)
       tkbind(SepCombo, "<<ComboboxSelected>>", function(){
                         idx <- grep(tclvalue(SEP), SepType)
                         Opt$Sep <<- c("\t", " ", ",", ";", "")[idx]
                    })
       tkgrid(SepCombo, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(DtaFrame, text="Decimal"),
               row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       DEC <- tclVar("Period")
       DecType <- c("Period", "Comma")
       DecCombo <- ttkcombobox(DtaFrame, width = 15, textvariable = DEC, values = DecType)
       tkbind(DecCombo, "<<ComboboxSelected>>", function(){
                         idx <- grep(tclvalue(DEC), DecType)
                         Opt$Dec <<- c(".", ",")[idx]
                    })
       tkgrid(DecCombo, row = 2, column = 2, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(DtaFrame, text="Quote"),
               row = 3, column = 1, padx = 5, pady = 5, sticky="w")
       QTE <- tclVar("No quote")
       QteType <- c("No quote", "Double quote", "Single quote")
       QteCombo <- ttkcombobox(DtaFrame, width = 15, textvariable = QTE, values = QteType)
       tkbind(QteCombo, "<<ComboboxSelected>>", function(){
                         idx <- grep(tclvalue(QTE), QteType)
                         Opt$Qte <<- c("", '"', "'")[idx]
                    })
       tkgrid(QteCombo, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
#---
       TryBtn <- tkbutton(LeftGroup, text="Try to Read Data", width=20, command=function(){
                         Read_Data()
                    })
       tkgrid(TryBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="w")

#---
       ScanFrame <- ttklabelframe(LeftGroup, text = "Scan DataFile", borderwidth=2)
       tkgrid(ScanFrame, row = 5, column = 1, padx = 5, pady = 5, sticky="w")

       SCN <- tclVar(FALSE)
       ScanFile <- tkcheckbutton(ScanFrame, text="Scan DataFile", variable=SCN, onvalue = 1, offvalue = 0,
                         command=function(){
                         scf <- tclvalue(SCN)
                         if (scf == "1") {
                             WidgetState(DataNcol, "normal")
                         } else {
                             WidgetState(DataNcol, "disabled")
                         }
                    })
       tkgrid(ScanFile, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       DNC <- tclVar("Data Ncol.")
       DataNcol <- ttkentry(ScanFrame, textvariable=DNC, foreground="grey")
       tkgrid(DataNcol, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
       tkbind(DataNcol, "<FocusIn>", function(K){
                         tkconfigure(DataNcol, foreground="red")
                         tclvalue(DNC) <- ""
                    })
       tkbind(DataNcol, "<Key-Return>", function(K){
                         tkconfigure(DataNcol, foreground="black")
                         DNcol <- as.numeric(tclvalue(DNC))
                    })
       WidgetState(DataNcol, "disabled")

#---
       CoreLineFrame <- ttklabelframe(LeftGroup, text = "XPS Core Line", borderwidth=2)
       tkgrid(CoreLineFrame, row = 6, column = 1, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(CoreLineFrame, text="Core-Line Name"),
               row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       CLNm <- tclVar("Core-Line Name")
       CLname <- ttkentry(CoreLineFrame, textvariable=CLNm, foreground="grey")
       tkbind(CLname, "<FocusIn>", function(K){
                         tkconfigure(CLname, foreground="red")
                         tclvalue(CLNm) <- ""
                    })
       tkbind(CLname, "<Key-Return>", function(K){
                         tkconfigure(CLname, foreground="black")
                         DNcol <- as.numeric(tclvalue(CLNm))
                    })
       tkgrid(CLname, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
#
       tkgrid( ttklabel(CoreLineFrame, text="X Scale"),
               row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       XSC <- tclVar("Binding Energy [eV]")
       X_Scale <- ttkentry(CoreLineFrame, textvariable=XSC, foreground="grey")
       tkbind(X_Scale, "<FocusIn>", function(K){
                         tkconfigure(X_Scale, foreground="red")
#                         tclvalue(XSC) <- ""
                    })
       tkbind(X_Scale, "<Key-Return>", function(K){
                         tkconfigure(X_Scale, foreground="black")
                         Xscale <- as.numeric(tclvalue(XSC))
                    })
       tkgrid(X_Scale, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
#
       tkgrid( ttklabel(CoreLineFrame, text="Y Scale"),
               row = 3, column = 1, padx = 5, pady = 5, sticky="w")
       YSC <- tclVar("Intensity [cps]")
       Y_Scale <- ttkentry(CoreLineFrame, textvariable=YSC, foreground="grey")
       tkbind(Y_Scale, "<FocusIn>", function(K){
                         tkconfigure(Y_Scale, foreground="red")
#                         tclvalue(YSC) <- ""
                    })
       tkbind(Y_Scale, "<Key-Return>", function(K){
                         tkconfigure(Y_Scale, foreground="black")
                         Yscale <- as.numeric(tclvalue(YSC))
                    })
       tkgrid(Y_Scale, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
#
       tkgrid( ttklabel(CoreLineFrame, text="Reverse X Axis?"),
               row = 4, column = 1, padx = 5, pady = 5, sticky="w")
       RevYN <- tclVar()
       RevRadio <- ttkradiobutton(CoreLineFrame, text="Yes", variable=RevYN,  value=1,
                       command=function(){
                           WidgetState(Import_btn, "normal")
                  })
       tkgrid(RevRadio, row = 4, column = 2, padx = 5, pady = 5, sticky="w")
       RevRadio <- ttkradiobutton(CoreLineFrame, text="No", variable=RevYN,  value=0,
                       command=function(){
                           WidgetState(Import_btn, "normal")
                  })
       tkgrid(RevRadio, row = 4, column = 2, padx = 50, pady = 5, sticky="w")
       tclvalue(RevYN) <- ""


##### RIGHT #####

#--- define INPUT Window
       InputFrame <- ttklabelframe(RightGroup, text = "File Data", borderwidth=2)
       tkgrid(InputFrame, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(InputFrame, text="Input Data:", font="Serif 12 bold"),
               row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       Raw_Input <- tktext(InputFrame, borderwidth=0, width=30, height=6)
       tkgrid(Raw_Input, row = 2, column = 1, padx = 0, pady = 0, sticky="wns")
       tkconfigure(Raw_Input, wrap="none")
#       addScrollbars(InputFrame, Raw_Input, type="x", Row = 2, Col = 1, Px=0, Py=0)
       WW <- as.numeric(tkwinfo("reqwidth", Raw_Input))
       HH <- as.numeric(tkwinfo("reqheight", Raw_Input))

#--- define LOAD Window
       tkgrid( ttklabel(InputFrame, text="Loaded data: ", font="Serif 12 bold"),
               row = 3, column = 1, padx = 5, pady = 5, sticky="w")
       LoadFrame <- ttkframe(InputFrame, borderwidth=0, padding=c(0,0,0,0))
       tkgrid(LoadFrame, row = 5, column = 1, padx = 0, pady = 0, sticky="w")
       Items <- list(C1=cbind(" ", " ", " ", " ", " ")) #initialize the Input Table with white spaces
       DataIN <- XPSTable(parent=LoadFrame, items=Items, NRows=5, ColNames="C1", Width=WW)
       tkgrid(DataIN, row = 1, column = 1, padx = 0, pady = 0, sticky="wns")

#--- Which column to read?
       ColToReadFrame <- ttklabelframe(RightGroup, text = "Column To Read", borderwidth=2)
       tkgrid(ColToReadFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

       tkgrid( ttklabel(ColToReadFrame, text="X-Col to Read: "),
               row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       XC <- tclVar("1")  #sets the initial msg
       XColRead <- ttkentry(ColToReadFrame, textvariable=XC, foreground="grey")
       tkgrid(XColRead, row = 1, column = 2, padx = 5, pady = 5, sticky="w")
       tkbind(XColRead, "<FocusIn>", function(K){
                         tkconfigure(XColRead, foreground="red")
                     })
       tkbind(XColRead, "<Key-Return>", function(K){
                         tkconfigure(XColRead, foreground="black")
                     })

       tkgrid( ttklabel(ColToReadFrame, text="Y-Col to Read: "),
               row = 2, column = 1, padx = 5, pady = 5, sticky="w")
       YC <- tclVar("2")  #sets the initial msg
       YColRead <- ttkentry(ColToReadFrame, textvariable=YC, foreground="grey")
       tkgrid(YColRead, row = 2, column = 2, padx = 5, pady = 5, sticky="w")
       tkbind(YColRead, "<FocusIn>", function(K){
                         tkconfigure(YColRead, foreground="red")
                     })
       tkbind(YColRead, "<Key-Return>", function(K){
                         tkconfigure(YColRead, foreground="black")
                     })

       tkgrid( ttklabel(ColToReadFrame, text="Err-Col to Read: "),
               row = 3, column = 1, padx = 5, pady = 5, sticky="w")
       ER <- tclVar("?")  #sets the initial msg
       ErColRead <- ttkentry(ColToReadFrame, textvariable=ER, foreground="grey")
       tkgrid(ErColRead, row = 3, column = 2, padx = 5, pady = 5, sticky="w")
       tkbind(ErColRead, "<FocusIn>", function(K){
                         tkconfigure(ErColRead, foreground="red")
                     })
       tkbind(ErColRead, "<Key-Return>", function(K){
                         tkconfigure(ErColRead, foreground="black")
                     })

       tkgrid( ttklabel(RightGroup, text="Import X, Y and Errors before Saving Data", font="Sans 10 bold"),
               row = 3, column = 1, padx = 5, pady = 5, sticky="w")

##--- BUTTONS
       BtnFrame <- ttkframe(ImportWin, borderwidth=0, padding=c(0,0,0,0))
       tkgrid(BtnFrame, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

       Import_btn <- tkbutton(BtnFrame, text="IMPORT", width=12, command=function(){
                         if (! check_selection()){return()}  #controls all the needed information are given
                         addCoreLine()    #add a new coreline
                         LL <- length(XPSSample)
                         cat("\n ----- Data Info -----")
                         cat("\n ===> Data File: ", activeFName, ", Core Line: ", XPSSample[[LL]]@Symbol)
                         cat("\n ===> Xmin= ", min(XPSSample[[LL]]@.Data[[1]]), "Xmax: ", max(XPSSample[[1]]@.Data[[1]]))
                         cat("\n ===> Ymin= ", min(XPSSample[[LL]]@.Data[[2]]), "Ymax: ", max(XPSSample[[1]]@.Data[[2]]))
                         cat("\n")
                         ErrCol <- tclvalue(ER)
                         if (ErrCol != "?"){
                             addErrors()    #add a new coreline
                             LL <- length(XPSSample)
                             cat("\n ----- Data Info -----")
                             cat("\n ===> Data File: ", activeFName, ", Core Line: ", XPSSample[[LL]]@Symbol)
                             cat("\n ===> Standard Deviation Error Added to Last Saved Data" )
                             cat("\n")
                         }
#                         WidgetState(Save_btn, "normal")
                         WidgetState(AddXS_btn, "normal")
                         WidgetState(SaveExit_btn, "normal")
                     })
       tkgrid(Import_btn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

#       Save_btn <- tkbutton(BtnFrame, text="SAVE", width=12, command=function(){
#                         LL <- length(XPSSample) #number of Corelines of the source XPSSample
#                         assign(activeFName, XPSSample, envir=.GlobalEnv)  #save the XPSSample in the .GlobalEnv
#                         assign("activeFName", activeFName, envir=.GlobalEnv)  #Set the activeSpectName to the last name of imported data
#                         assign("activeSpectName", XPSSample[[LL]]@Symbol, envir=.GlobalEnv)
#                         assign("activeSpectIndx", LL, envir=.GlobalEnv)   #set the activeSpectIndx to the last imported data
#                         XPSSaveRetrieveBkp("save")
#                         WidgetState(Save_btn, "disabled")
#                         WidgetState(AddXS_btn, "disabled")
#                     })
#       tkgrid(Save_btn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

       AddXS_btn <- tkbutton(BtnFrame, text="SAVE in an EXISTING XPS-Sample", width=30, command=function(){
                         XPSSamplesList <- XPSFNameList()
                         if (length(XPSSamplesList) > 0 ) {
                             GwinSave <- tktoplevel()
                             tkwm.title(GwinSave,"SELECT XPS-SAMPLE")
                             tkwm.geometry(GwinSave, "+200+200")
                             SaveGroup <- ttkframe(GwinSave, borderwidth=0, padding=c(0,0,0,0) )
                             tkgrid(SaveGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

                             Saveframe <- ttklabelframe(SaveGroup, text = "Select Destination", borderwidth=2)
                             tkgrid(Saveframe, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

                             Items <- list(x=XPSSamplesList)
                             SaveTbl <- XPSTable(parent=Saveframe, items=Items, NRows=7, ColNames="XPSSamples", Width=100)
                             addScrollbars(Saveframe, SaveTbl, type="y", Row=1, Col=1)
                             tkbind(SaveTbl, "<<TreeviewSelect>>", function() {  #bind the table elements to the LEFT mouse but doubleClick
                                         idx <- tclvalue(tcl(SaveTbl, "selection" ))  # Get the selected items
                                         idx <- as.numeric(gsub("\\D", "", idx))
                                         activeFName <<- Items[[1]][idx]
                                   })

                             SaveBtn <- tkbutton(SaveGroup, text="SELECT", width=16, command=function(){
                                                 if (length(activeFName) > 0) {
                                                     tkdestroy(GwinSave)
                                                     FName <<- get(activeFName, envir=.GlobalEnv)
                                                     LL <- length(FName)      #Number of corelines in the destination XPSSample
                                                     CLnames <- names(FName)
                                                     LLL <- length(XPSSample) #Number of CoreLines in the source XPSSample
                                                     FName[[LL+1]] <<- XPSSample[[LLL]] #save last imported Corelines in the destinaton XPSSample
                                                     FName@names <<- c(CLnames, tclvalue(CLNm)) #add names of new CoreLines
                                                     XPSSample <<- FName      #set the source XPSSample == destination file with all spectra
                                                     assign(activeFName, FName, envir=.GlobalEnv)  #Save the destination XPSSample in GlobalEnv
                                                     assign("activeFName", activeFName, envir=.GlobalEnv)
                                                     assign("activeSpectName", FName[[LL+1]]@Symbol, envir=.GlobalEnv)
                                                     assign("activeSpectIndx", 1, envir=.GlobalEnv)
                                                     plot(FName)
                                                     cat("\n Data saved in ", activeFName)
                                                     XPSSaveRetrieveBkp("save")
#                                                     WidgetState(Save_btn, "disabled")
                                                     WidgetState(AddXS_btn, "disabled")
                                                 }
                                        })
                             tkgrid(SaveBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")
                         }
                     })
       tkgrid(AddXS_btn, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

       SaveExit_btn <- tkbutton(BtnFrame, text="SAVE", width=14, command=function(){
                         LL <- length(XPSSample)
                         assign(activeFName, XPSSample, envir=.GlobalEnv)  #save XPSSample in GlobalEnv
                         assign("activeFName", activeFName, envir=.GlobalEnv)  #Set the activeSpectName to the last name of imported data
                         assign("activeSpectName", XPSSample[[LL]]@Symbol, envir=.GlobalEnv)
                         assign("activeSpectIndx", LL, envir=.GlobalEnv)   #set the activeSpectIndx to the last imported data
                         XPSSaveRetrieveBkp("save")
                         return(XPSSample)
                     })
       tkgrid(SaveExit_btn, row = 1, column = 3, padx = 5, pady = 5, sticky="w")

       Exit_btn <- tkbutton(BtnFrame, text="EXIT", width=10, command=function(){
                         tkdestroy(ImportWin)
                         XPSSaveRetrieveBkp("save")
print(str(XPSSample))
                         return(XPSSample)
                     })
       tkgrid(Exit_btn, row = 1, column = 4, padx = 5, pady = 5, sticky="w")

       WidgetState(Import_btn, "disabled")
#       WidgetState(Save_btn, "disabled")
       WidgetState(AddXS_btn, "disabled")
       WidgetState(SaveExit_btn, "disabled")

       tkwait.window(ImportWin)
       return(XPSSample)
}     




