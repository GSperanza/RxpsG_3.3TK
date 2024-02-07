#'XPSAgreement
#'
#'@description
#'Agreement to use the RxpsG package
#'
#'@examples
#'
#'\dontrun{
#'XPSAgreement()
#'}
#'
#'@docType methods
#'@export
#'


XPSAgreement<-function(){

       MainWin <- tktoplevel()
       tkwm.title(MainWin,"SOFTWARE AGREEMENT")
       tkwm.geometry(MainWin, "+100+50")   #position respect topleft screen corner

       MainGroup <- ttkframe(MainWin, borderwidth=0, padding=c(0,0,0,0) )
       tkgrid(MainGroup, row = 1, column = 1, padx = 0, pady = 0, sticky="w")

       msg<-"           Welcome to RXPSG!

       This program is free software. You can use it under the
       terms of the  GNU Affero General Public License.
       http://www.gnu.org/licenses/
       and licenses linked to the use of the R, Rstudio platforms.

       - Authors decline any responsibility deriving from
         the use of the software or from software bugs.

       - Users are kindly requested to cite the software
         authors in their publications:
       
         Giorgio Speranza, Roberto Canteri
         Fondazione Bruno Kessler, Sommarive str. 18
         38123 Trento Italy.  "

       ShowAgreement <- tktext(MainGroup, width=72, height=18)
       tkgrid(ShowAgreement, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       tkinsert(ShowAgreement, "0.0", msg) #write report in ShowParam Win

}
