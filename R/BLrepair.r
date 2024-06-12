# macro per riparare BASELINE introdotta con VBTOP, VFFermi, MaxMinDIFF

#La macro agisce sull' Active XPSSample

  BLrepair <- function(){

     cat("\n ==> Active XPSSample: ", activeFName, "\n")
     FName <- get(activeFName, envir=.GlobalEnv)
     tmp <- list()
     for(ii in 1:length(FName)){
         if(hasBaseline(FName[[ii]])){
            cat("\n CoreLine", ii, "N. elements of Baseline list: ", length(FName[[ii]]@Baseline))
         }
     }
     cat("\n\n >>>> CoreLine index to correct: ")
     CL <- scan(n=1, quiet=TRUE)
     object <- FName[[CL]]
     txt <- paste("str{", activeFName,"[[", CL, "]]@Baseline:  ", sep="")
     cat("\n ==>", txt)
     LL <- length(object@Baseline)
     tmp <- as.character(object@Baseline)
     Names <- names(object@Baseline)
     for(ii in 1:LL){
         cat("\n",paste("$",Names[ii],": ",sep=""), substr(tmp[[ii]], start=1, stop=50))
     }
     if(hasBaseline(object)){
        ## define a class "baseline" for  the new baseline component
        if (LL > 2){ #save all the Baseline[[3:LL]] components i a temporary location
            tmp <- object@Baseline[3:LL]
            Names <- names(object@Baseline)[3:LL]
            object@Baseline[3:LL] <- NULL
        }
        LL <- LL+1
#        setClass("BsLn", representation(BsLn="list"), contains="list", prototype(BsLn=list()))
        object@Baseline[["baseline"]] <- new("baseline")
#        if(length(object@Baseline[["baseline"]]@baseline) == 0) {object@Baseline[["baseline"]]@baseline <- NA}
#        if(length(object@Baseline[["baseline"]]@corrected) == 0) {object@Baseline[["baseline"]]@corrected <- NA}
#        if(length(object@Baseline[["baseline"]]@spectra) == 0) {object@Baseline[["baseline"]]@spectra <- NA}
#        if(length(object@Baseline[["baseline"]]@call) == 0) {object@Baseline[["baseline"]]@call <- NA}
        cat("\n ==> Corrected Baseline:")
        attr(object@Baseline$baseline, "package") <- ".GlobalEnv"  #set package attribute of Baseline == .GlobalEnv
        object@Baseline <- c(object@Baseline, tmp)
        names(object@Baseline)[4:LL] <- Names
        idx <- grep("baseline", Names)
        if(idx > 0){
           object@Baseline[[(idx+3)]] <- NULL
        }
        if(length(object@Baseline$type)==0){
           cat("\n ==> Baseline type:")
           BLType <- scan(n=1, what="character", quiet=TRUE)
           object@Baseline$type <- BLType
        } else {
           cat("\n ==> Baseline type:", object@Baseline$type)
           cat("\n Is this Baseline type correct? [y/n]")
           answ <- scan(n=1, what="character", quiet=TRUE)
           if(answ == "n" || answ == "N" || answ == "no" || answ == "NO"){
              cat("\n Please give the Baseline type:")
              BLType <- scan(n=1, what="character", quiet=TRUE)
              object@Baseline$type <- BLType
           }
        }
        print(str(object@Baseline))
     } else {
        cat("\n ==> NO BASELINE")
     }
     FName[[CL]] <- object
     assign(activeFName, FName, envir=.GlobalEnv)
     cat("\n ==> XPSSample saved")
 }

