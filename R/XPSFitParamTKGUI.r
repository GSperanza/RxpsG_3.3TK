#GUI to set fit param MaXiTERATION, TOLERANCE, MINFACTOR, when NLSfit error occurs

#'@title XPSFitParam
#'@description XPSFitParam GUI to set the fit param MaXiTERATION, TOLERANCE, MINFACTOR
#'  when NLSfit error occurs
#'@param ParamNames c("maxiteration", "tolerance", "minfactor") the set of parameter to work with
#'@param ParamValues the correspondent values
#'@examples
#'\dontrun{
#'  XPSFitParam(c("maxiteration", "tolerance", "minfactor"), c(1000, 1e-5, 1e-3))
#'}
#'@export
#'


XPSFitParam <- function(ParamNames, ParamValues){

  ParmIdx <- NULL

  mainNLSwin <- tktoplevel()
  tkwm.title(mainNLSwin,"SET NLS PARAMETERS")
  tkwm.geometry(mainNLSwin, "+100+50")

  NLSgroup <- ttkframe(mainNLSwin, borderwidth=0, padding=c(0,0,0,0) )
  tkgrid(NLSgroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

  NLSframe1 <- ttklabelframe(NLSgroup, text="Select NLS parameters", borderwidth=2)
  tkgrid(NLSframe1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  Parm1 <- tclVar()
  NLSparam <- ttkcombobox(NLSframe1, width = 15, textvariable = Parm1, values = ParamNames)
  tkbind(NLSparam, "<<ComboboxSelected>>", function(){
                           ParmIdx <<- grep(tclvalue(Parm1), ParamNames)
                           tclvalue(Parm2) <- ParamValues[ParmIdx]
                 })
  tkgrid(NLSparam, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

  NLSframe2 <- ttklabelframe(NLSgroup, text="Set NLS Parameter", borderwidth=2)
  tkgrid(NLSframe2, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

  Parm2 <- tclVar()  #sets the initial msg
  NLSobj1 <- ttkentry(NLSframe2, textvariable=Parm2, foreground="grey")
  tkgrid(NLSobj1, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
  tkbind(NLSobj1, "<FocusIn>", function(K){
                         tkconfigure(NLSobj1, foreground="red")
                         tclvalue(Parm2) <- ""
                 })
  tkbind(NLSobj1, "<Key-Return>", function(K){
                         tkconfigure(NLSobj1, foreground="black")
                         ParamValues[ParmIdx] <<- as.numeric(tclvalue(Parm2))
                 })

  NLSframe3 <- ttklabelframe(NLSgroup, text="Set Parameters", borderwidth=2)
  tkgrid(NLSframe3, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

  SaveBtn <- tkbutton(NLSframe3, text="SAVE Parameter", width=15, command=function(){
                    assign ("N.ParamValues", ParamValues, envir=.GlobalEnv)
                 })
  tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

  CloseBtn <- tkbutton(NLSframe3, text="Close and REFIT", width=15, command=function(){
                    fit<-NULL
                    tryAgain <- TRUE
                    assign("N.fit", fit, envir=.GlobalEnv)   # set fit==NULL to repeat the fit if loop active i.e. tryAgain==FALSE
                    assign("N.tryAgain", tryAgain , envir=.GlobalEnv)   # set tryAgain to loop
                    tkdestroy(mainNLSwin)
                    return()
                 })
  tkgrid(CloseBtn, row = 2, column = 1, padx = 5, pady = 5, sticky="w")

  ExitBtn <- tkbutton(NLSframe3, text="EXIT Fit Routine", width=15, command=function(){
                    tryAgain <- FALSE
                    assign("N.tryAgain", tryAgain , envir=.GlobalEnv)   # set tryAgain==FALSE to exit the loop
                    tkdestroy(mainNLSwin)
                    return()
                 })
  tkgrid(ExitBtn, row = 3, column = 1, padx = 5, pady = 5, sticky="w")

}
