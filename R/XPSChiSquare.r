#XPSChiSquare computes the ChiSquare of a fit, and provides the residual error
#Also the error on the fitting parameters are given

#' @title XPSChiSquare
#' @description XPSChiSquare calculates the Chi^2 for a core-line fit
#'
#' @examples
#' \dontrun{
#' 	XPSChiSquare()
#' }
#' @export
#'


XPSChiSquare <- function(){


#--- Global variables definition ---
   activeFName <- get("activeFName", envir = .GlobalEnv)
   if (length(activeFName)==0 || is.null(activeFName) || is.na(activeFName)){
       tkmessageBox(message="No data present: please load and XPS Sample", title="XPS SAMPLES MISSING", icon="error")
       return()
   }
   FName <- get(activeFName, envir=.GlobalEnv)  #load in FName the XPSSample data
   FNameList <- XPSFNameList() #list of XPSSamples
   SpectList <- XPSSpectList(activeFName) #CopreLine list in the active XPSSample
   SpectIndx <- NULL
   Chi2 <- NULL

#--- Widget
   ChiWin <- tktoplevel()
   tkwm.title(ChiWin,"FIT GOODNESS")
   tkwm.geometry(ChiWin, "+100+50")   #position respect topleft screen corner

   ChiGroup <- ttkframe(ChiWin, borderwidth=0, padding=c(0,0,0,0) )
   tkgrid(ChiGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

   ChiFrame1 <- ttklabelframe(ChiGroup, text = " Select XPS-Sample ", borderwidth=2)
   tkgrid(ChiFrame1, row = 1, column = 1, padx = 5, pady = 5, sticky="we")
   XS <- tclVar(activeFName)
   XPS.Sample <- ttkcombobox(ChiFrame1, width = 20, textvariable = XS, values = FNameList)
   tkbind(XPS.Sample, "<<ComboboxSelected>>", function(){
                     activeFName <<- tclvalue(XS)
                     FName <<- get(activeFName, envir=.GlobalEnv)
                     SpectList <<- XPSSpectList(activeFName)
                     tkconfigure(Core.Lines, values=SpectList)
                 })
   tkgrid(XPS.Sample, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   ChiFrame2 <- ttklabelframe(ChiGroup, text = " Select Core.Line ", borderwidth=2)
   tkgrid(ChiFrame2, row = 2, column = 1, padx = 5, pady = 5, sticky="we")

   CL <- tclVar()
   Core.Lines <- ttkcombobox(ChiFrame2, width = 20, textvariable = CL, values = SpectList)
   tkbind(Core.Lines, "<<ComboboxSelected>>", function(){
                     SpectName <- tclvalue(CL)
                     SpectName <- unlist(strsplit(SpectName, "\\."))   #drop the N. at beginning core-line name
                     SpectIndx <<- as.integer(SpectName[1])
                     if (length(FName[[SpectIndx]]@RegionToFit) == 0) {
                         tkmessageBox(message="No Fit Present! Please Select Another Core.Line", title="WARNING", icon="warning")
                         return()
                     }
                     plot(FName[[SpectIndx]])
                 })
   tkgrid(Core.Lines, row = 1, column = 2, padx = 5, pady = 5, sticky="w")

   ChiFrame3 <- ttklabelframe(ChiGroup, text = " Select Goodness Parameter ", borderwidth=2)
   tkgrid(ChiFrame3, row = 3, column = 1, padx = 5, pady = 5, sticky="we")
   MTD <- tclVar()
   GoodnessFctn <- c("Chi^2", "Reduced Chi^2", "Param_STD_ERR", "All", "Test")
   Compute.Gdnss <- ttkcombobox(ChiFrame3, width = 20, textvariable = MTD, values = GoodnessFctn)
   tkgrid(Compute.Gdnss, row = 1, column = 1, padx = 5, pady = 5, sticky="w")

   ChiBtn <- tkbutton(ChiGroup, text=" COMPUTE ", width=22, command=function(){
#Chi2 = SUM_i[(Y_i - Fit_i)^2 / Sigma^2_i ]
#We have the experimental data Y_i which is the Core Line
#We have the Core Line fit Fit_i
#What is the Sigma_i associated to the Y_i values?
#Following J. Wolberg, "Data Analysis Using the Method of Least Squares: Extracting the Most
#  Information from Experiments", Springer, 2006, pp 74:
#Let us use Y_i to represent the number of counts recorded in the time interval t_i.
#According to Poisson statistics the expected value of Sigma_i^2 = variance associated 
# with Y_i, is just Y_i
#Sigma_i = sqrt{Y_i}
#Then:    Chi2 = SUM.i[(Y_i - Fit_i)^2 / Y_i]
#
#Cfr. N. Fairley et al.: "Practical guide to understanding goodness-of-fit metrics used in chemical
#     state modeling of x-ray photoelectron spectroscopy data by synthetic line shapes
#     using nylon as anexample",(2023)J. Vac. Sci. Technol. A41 013203
#
#also following  J. Wolberg, "Data Analysis Using the Method of Least Squares: Extracting the Most
#  Information from Experiments", Springer, 2006, eqq. 2.2.3 where the weights wi are the elements 
#of the covarianvce matrix. Then we find that the Chi2 is
#  Chi2 = t(Y-Fit) inv(Cov) (Y-fit)    t(...) means transpose
#-----
#Reduced_Chi2 = Chi2/(n-p)
#where n-p = degree of freedom,   n = numer of overvables, p=number of fitting parameters
#-----
#Param_STD_ERR = Sigma_ak is the standard-deviation for the fit parameters ak
#following  J. Wolberg, "Data Analysis Using the Method of Least Squares: Extracting the Most
#  Information from Experiments", Springer, 2006, p.50
#  Param_STD_ERR = Chi2/(n-p) * Cov^-1
#where n-p = degree of freedom,   n = N. observables, p = N. fit parameters,
#Cov^-1 is the inverse of the covariance matrix
#
#  However, more easily:
#  ==> summary(fit) provides the standare error affecting each fitting parameter
                     txt <- paste("Likely the Core.Line Fit was Copied Using Process CoreLine \n",
                                  "Please Run the Option 'Fit Lev.Marq.' to Compute the Fit Information.",
                                  sep = "")
                     if (tclvalue(MTD) == "Chi^2"){
                         Y <- FName[[SpectIndx]]@RegionToFit$y
                         Fit <- FName[[SpectIndx]]@Fit$y + FName[[SpectIndx]]@Baseline$y
                         LL <- length(Y)
                         Chi2 <<- 0
                         for(ii in 1:LL){
                             Chi2 <<- Chi2 + (Y[ii] - Fit[ii])^2/abs(Y[ii]) #data-baseline can be negative...
                         }
                         cat("\n\n\n --- Chi Square ----------------------------------------------\n")                         
                         cat("\n ==> Chi^2:", Chi2)
                         cat("\n -------------------------------------------------------------")
                     } else if(tclvalue(MTD) == "Reduced Chi^2"){
                         if (length(FName[[SpectIndx]]@Fit$fit) == 0 ||
                             is.na(FName[[SpectIndx]]@Fit$fit[1]) ||
                             is.null(FName[[SpectIndx]]@Fit$fit)){
                             tkmessageBox(message=txt, title="WARNING", icon="warning")
                             return()
                         }
                         Y <- FName[[SpectIndx]]@RegionToFit$y
                         Fit <- FName[[SpectIndx]]@Fit$y + FName[[SpectIndx]]@Baseline$y
                         LL <- length(Y)
                         Chi2 <<- 0
                         CovMatrix <- vcov(FName[[SpectIndx]]@Fit$fit, complete=TRUE)
                         Npar <- dim(CovMatrix)[1]
                         for(ii in 1:LL){
                             Chi2 <<- Chi2 + (Y[ii] - Fit[ii])^2/abs(Y[ii]) #data-baseline can be negative...
                         }
                         cat("\n\n\n --- Chi^2 and Reduced Chi^2 ---------------------------------\n")
                         cat("\n ==> N. Fit parameters:", Npar)
                         Chi2Red <- Chi2/(LL-Npar)
                         cat("\n ==> Chi^2:", Chi2)
                         cat("\n ==> Reduced Chi^2:", Chi2Red)
                         cat("\n -------------------------------------------------------------")
                     } else if(tclvalue(MTD) == "Param_STD_ERR"){
                         if (length(FName[[SpectIndx]]@Fit$fit) == 0 ||
                             is.na(FName[[SpectIndx]]@Fit$fit[1]) ||
                             is.null(FName[[SpectIndx]]@Fit$fit)){
                             tkmessageBox(message=txt, title="WARNING", icon="warning")
                             return()
                         }
                         Y <- FName[[SpectIndx]]@RegionToFit$y
                         Fit <- FName[[SpectIndx]]@Fit$y + FName[[SpectIndx]]@Baseline$y
                         LL <- length(Y)
                         Chi2 <<- 0
                         for(ii in 1:LL){
                             Chi2 <<- Chi2 + (Y[ii] - Fit[ii])^2/abs(Y[ii]) #data-baseline can be negative...
                         }
                         FitParam <- coef(FName[[SpectIndx]]@Fit$fit)
                         CovMatrix <- vcov(FName[[SpectIndx]]@Fit$fit, complete=FALSE)
                         cat("\n\n\n --- Fit Parameter STD Errors --------------------------------\n")
                         cat("\n ==> Model Fit Summary:\n")
                         print(summary(FName[[SpectIndx]]@Fit$fit))
                         cat("\n -------------------------------------------------------------")
                     } else if(tclvalue(MTD) == "All"){
                         if (length(FName[[SpectIndx]]@Fit$fit) == 0 ||
                             is.na(FName[[SpectIndx]]@Fit$fit[1]) ||
                             is.null(FName[[SpectIndx]]@Fit$fit)){
                             tkmessageBox(message=txt, title="WARNING", icon="warning")
                             return()
                         }
                         Y <- FName[[SpectIndx]]@RegionToFit$y
                         Fit <- FName[[SpectIndx]]@Fit$y + FName[[SpectIndx]]@Baseline$y
                         LL <- length(Y)
                         Chi2 <<- 0
                         FitParam <- coef(FName[[SpectIndx]]@Fit$fit)
                         CovMatrix <- vcov(FName[[SpectIndx]]@Fit$fit, complete=TRUE)
                         ParamNames <- colnames(CovMatrix) #(CovMatrix) is n x n where n is the number of fitting parameters
                         Npar <- dim(CovMatrix)[1] # dim(CovMatrix) is n x n where n is the number of fitting parameters
                         Npar <- length(FitParam)
                         for(ii in 1:LL){
                             Chi2 <<- Chi2 + (Y[ii] - Fit[ii])^2/abs(Y[ii]) #data-baseline can be negative...
                         }
                         Chi2Red <- Chi2/(LL-Npar)

                         if (det(CovMatrix) == 0) {
                             tkmessageBox(message=" Cannot Compute the Inverse of Covariance Matrix.\n Computation stops!", title="WARNING", icon="warning")
                         }
                         Npar <- dim(CovMatrix)[1] # dim(CovMatrix) is n x n where n is the number of fitting parameters
                         I.CovMatrix <- MASS::ginv(CovMatrix)
                         SigmaPar <- sqrt(Chi2 * diag(I.CovMatrix) /(LL-Npar))
                         names(SigmaPar) <- ParamNames
                         cat("\n\n\n --- Summary of Fit Goodness ---------------------------------\n")
                         cat("\n ==> Chi^2:", Chi2, "\n")
                         cat("\n ==> N. Fit parameters:", Npar, "\n")
                         cat("\n ==> Reduced Chi^2:", Chi2Red, "\n")
                         cat("\n ==> Model Fit Summary:\n")
                         print(summary(FName[[SpectIndx]]@Fit$fit))
                         cat("\n -------------------------------------------------------------")
                     }
                 })
   tkgrid(ChiBtn, row = 4, column = 1, padx = 5, pady = 5, sticky="we")

   ExitBtn <- tkbutton(ChiGroup, text="  SAVE & EXIT  ", width=22, command=function(){
                          assign("activeFName", activeFName, envir=.GlobalEnv)
                          assign(activeFName, FName, envir=.GlobalEnv)
                          assign("activeSpectIndx", SpectIndx, envir=.GlobalEnv)
                          tkdestroy(ChiWin)
                      })
   tkgrid(ExitBtn, row = 5, column = 1, padx = 5, pady = 5, sticky="we")
}
