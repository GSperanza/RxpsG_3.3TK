#' Package RxpsG
#'
#' Processing tools for X ray Photoelectron Spectroscopy Data.
#' The package implements classes and methods to read, visualize and manipulate XPS spectra files.
#' The spectra can be obtained from .pxt and .vms data format from different instruments.'
#' More generally, any data that is recorded as a list of (x,y) is suitable.
#'
#' @import digest latticeExtra minpack.lm signal SparseM tcltk 
#' @suggests rootSolve
#'
#'
#' @importFrom graphics grconvertX grconvertY arrows axTicks axis box grid
#' @importFrom graphics layout legend lines locator matlines matplot mtext par plot.new
#' @importFrom graphics points rect segments text
#'
#' @importFrom grDevices dev.copy dev.cur dev.next dev.prev dev.print dev.set dev.size
#' @importFrom grDevices graphics.off bmp jpeg pdf png postscript tiff 
#' @importFrom grDevices recordGraphics recordPlot replayPlot x11 X11 
#'
#' @importFrom lattice cloud panel.arrows panel.points panel.segments panel.superpose panel.text
#' @importFrom lattice panel.xyplot xyplot xscale.components.default yscale.components.default
#' @importFrom lattice trellis.focus trellis.unfocus 
#'
#' @importFrom grid convertX convertY grid.locator unit
#'
#' @importFrom methods as formalArgs new show slot "slot<-"
#'
#' @importFrom minpack.lm nlsLM nls.lm nls.lm.control
#'
#' @importFrom signal filter freqz sgolay hamming fir1 filtfilt butter
#'
#' @importFrom stats as.formula coef convolve fft fitted formula getInitial lm
#' @importFrom stats model.weights na.omit nls.control numericDeriv predict residuals
#' @importFrom stats runif setNames spline update window
#'
#' @importFrom utils capture.output install.packages read.table str
#' @importFrom utils write.csv write.csv2 write.table
#'
#' @docType package
#' @name RxpsG   
#' @parameter activeSpectIndx
#' @parameter activeSpectName
#' @parameter XPSSettings 
#' @parameter Pkgs 
NULL


   




