## --------------------------------------------------------------------------
## Rxps V.1.0.5 - R package for processing X-ray Photoelectron Spectroscopy Data
## --------------------------------------------------------------------------
##  Copyright (c) 2012-2023 Roberto Canteri <canteri@fbk.eu>
##
##  This library is free software; you can redistribute it and/or
##  modify it under the terms of the GNU Lesser General Public
##  License as published by the Free Software Foundation; either
##  version 2.1 of the License, or (at your option) any later version.
##
##  This library is distributed in the hope that it will be useful,
##  but WITHOUT ANY WARRANTY; without even the implied warranty of
##  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
##  Lesser General Public License for more details.
##
##  You should have received a copy of the GNU Lesser General Public
##  License along with this library; if not, write to the Free Software
##  Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
##
## --------------------------------------------------------------------------
## --------------------------------------------------------------------------

## TK utility functions


#' @title WidgetState
#' @description WidgetState sets the state of all the widget childrens
#' @param widget the toplevel widget container
#' @param value is 'normal' or 'disabled'
#' @export
WidgetState <- function(widget, value) {
       childID <- tclvalue(tkwinfo("children", widget))
       if (childID == ""){
           Optn <- tkconfigure(widget)  #get child options
           if (length(grep("state", Optn)) > 0){ #controls if 'state' option is present
               tkconfigure(widget, state=value)  #set child 'disabled' or 'normal'
           }
       } else {
           sapply(unlist(strsplit(childID, " ")), function(x) {
                             Optn <- tkconfigure(x)  #get child options
                             if (length(grep("state", Optn)) > 0){ #controls if 'state' option is present
                                 tkconfigure(x, state=value)  #set child 'disabled' or 'normal'
                             }
                       })
       }
       tcl("update", "idletasks")   #conclude pending event before exiting function
}

#' @title ClearWidget
#' @description ClearWidget removes all childrens from a widget container
#' @param widget the widget container
#' @export
ClearWidget <- function(widget) {
       childID <- tclvalue(tkwinfo("children", widget))
       sapply(unlist(strsplit(childID, " ")), function(x) {
                  tcl("grid", "remove", x)
       })
}



#' @title Add scrollbars to some widgets GUI
#' @description Add scrollbars to some GUI widgets
#' @param parent frame containing the \code{widget}
#' @param widget widget where to add scrollbars
#' @param type which scrollbars to enable
#' @param Row the row position of the calling widget
#' @param Col the column position of the calling widget
#' @param Px padx param. of the calling widget
#' @param Py pady param. of the calling widget
#' @return add the selected scrollbar to the widget
#' @export
addScrollbars <- function(parent, widget, type = c("x", "y"), Row=1, Col=1, Px=0, Py=0) {
   tcl("update", "idletasks")
   if(any(type %in% c("x"))) {
      xscrl <- ttkscrollbar(parent, orient = "horizontal",
                            command = function(...) tkxview(widget, ...))
      tkgrid(xscrl, row = Row+1, column = Col, padx = Px, pady = Py, sticky = "nwe")
#      tkgrid.rowconfigure(parent, 0, weight=1)
      tkconfigure(widget, xscrollcommand = function(...) tkset(xscrl, ...))
      if (length(grep("wrap", tkconfigure(widget))) > 0 ){
          tkconfigure(widget, wrap="none")
      }
      tcl("autoscroll::autoscroll", xscrl)
   }
   if(any(type %in% c("y"))) {
      yscrl <- ttkscrollbar(parent, orient = "vertical",
                            command = function(...) tkyview(widget, ...))
      tkgrid(yscrl, row = Row, column = Col+1, padx = Px, pady = Py, sticky = "wns")
#      tkgrid.columnconfigure(parent, 0, weight=1)
      tkconfigure(widget, yscrollcommand = function(...) tkset(yscrl, ...))
      if (length(grep("wrap", tkconfigure(widget))) > 0 ){
          tkconfigure(widget, wrap="none")
      }
      tcl("autoscroll::autoscroll", yscrl)
   }
   tkgrid.propagate(parent, FALSE)
}

#' @title get_common_list Common XPS coreline for different XPS Samples
#' @description get_common_list gets the list of common XPSCoreline for different XPSSamples GUI
#' @param xps.sample_name list of xps samples name
#' @return the list of XPSCoreline common to the \code{xps.sample_name}
#' @author Roberto Canteri \email{canteri@fbk.eu}
#' @export
get_common_list <- function(xps.sample_name) {
   tmp <- names(get(as.character(xps.sample_name[1]), envir = .GlobalEnv))
   nlength <- length(xps.sample_name)
   if (nlength > 1) {
      for ( idx in c(2:nlength)) {
        tmp <- intersect(tmp, names(get(as.character(xps.sample_name[idx]), envir = .GlobalEnv)))
      }
   }
   commonList <- data.frame("CoreLine" = tmp,
                            "Info" = rep(sprintf("Common Core Line"), length(tmp))
   )
   return(commonList)
}

#' @title get_selected_treeview The selected row of the treeview
#' @description get_selected_treeview returns the selected row of the treeview GUI
#' @param widget treeview widget
#' @param column column of the treeview widget
#' @return values of selected rows and column
#' @author Roberto Canteri \email{canteri@fbk.eu}
#' @export
## get column 0 value from selected rows of ttk::treeview

get_selected_treeview <- function(widget, column = 0) {
    samplesID <- tclvalue(tcl(widget, "selection"))
    selected <- sapply(unlist(strsplit(samplesID, " ")), function(x) {
                       value <- tclvalue( tcl(widget, "set", x, column) )
                       return(value)
                })
}

#' @title get_column_treeview
#' @description get_column_treeview returns the indicated column of elements of the treeview GUI
#' @param widget treeview widget
#' @param column column of the treeview widget
#' @return values of selected column
#' @export
## get column 0 value from selected rows of ttk::treeview

get_column_treeview <- function(widget, column = 0) {
    childID <- tclvalue(tcl(widget, "children", ""))
    items <- sapply(unlist(strsplit(childID, " ")), function(x) {
                    value <- tclvalue( tcl(widget, "set", x, column) )
                })
    return(items)
}


#' @title populate_treeview Populate the treeview
#' @description populate_treeview populates the treeview with data saved in a \code{data.frame}
#'   The items MUST be a \code{data.frame}
#' @param widget treeview widget
#' @param where position to add the \code{items}, default to \code{end}
#' @param items items to add
#' @author Roberto Canteri \email{canteri@fbk.eu}
#' @export
populate_treeview <- function(widget, where = "end", items) {
   # where = 0 each item is inserted from top
   # where = "end" each item is attached
   clear_treeview(widget)
   idx <- apply(items, 1, function(x) {
             tcl(widget, "insert", "", where, values = x )
          })
   return(widget)
}




#' @title clear_treeview Clear the treeview
#' @description clear_treeview clears the treeview container
#' @param widget widget
#' @export
## clear treeview
clear_treeview <- function(widget) {
   tcl(widget, "delete", tcl(widget, "children", ""))
   return(widget)
}


#' @title clear_widget destroys the elements contained in a generic widget 
#'    like a tktoplevel(), ttkframe() or ttklabelframe()
#' @description clear_widget destroys all elements in the widget container
#' @param widget parent widget
#' @export
## clear widget
clear_widget <- function(widget) {
   children <- tclvalue(tcl("winfo", "children", widget))
   children <- unlist(strsplit(children, " "))
   sapply(children, function(x) { tcl("destroy", x) })
   return(widget)
}


#' @title updateTable
#' @description updateTable updates a table-like widget with the specified items
#' @param widget widget to update
#' @param items the items used to updade the Table
#' @export
updateTable <- function(widget, items) {
    #check if all elements items are NULL
    if (length(items) > 0 && sum(sapply(items, function(x) length(x))) == 0) {
        widget <- clear_treeview(widget)
        return(widget)
    }
    #check if items is NULL or items==list() or if its elements are all equal to ""
    if (length(items) == 0 || is.null(items[[1]][1]) == TRUE || items[[1]][1] == "") {
#        tkmessageBox(message="NO Items Found. Please Control Data!", title="WARNING", icon="warning")
        widget <- clear_treeview(widget)
        return(widget)
    }
    if(is.list(items)){
       LL <- length(items)
       LLmax <- max(sapply(items, function(x) length(x)))
       for(ii in 1:LL){
           for(jj in 1:LLmax){
               if(is.null(items[[ii]][jj]) || is.na(items[[ii]][jj]) || items[[ii]][jj]=="") { items[[ii]][jj] <- "   " }
           }
       }
    }
    items <- as.data.frame(items, stringsAsFactors=FALSE)
    widget <- populate_treeview(widget = widget, items = items)
    tcl("update", "idletasks")
    return(widget)
}

#' @title XPSTable
#' @description TableList generates a table-like widget with the specified items
#' @param parent the widget container
#' @param items the items used to generate the Table
#' @param NRows the number of Table rows
#' @param ColNames the name of the Table columns
#' @param Width the width of the Table columns
#' @export
XPSTable <- function(parent, items, NRows=0, ColNames, Width) {
    if (length(items) == 0) {
       tkmessageBox(message="ERROR: NO Items Found. Please Control Table Data!", title="ERROR", icon="error")
    }
    Ncol <- length(items)
    if(is.list(items)){
       Nitems <- max(sapply(items, function(x) length(x)))
       for(ii in 1:Ncol){
           for(jj in 1:Nitems){
               if(is.null(items[[ii]][jj]) || is.na(items[[ii]][jj]) || items[[ii]][jj]=="") {
                  items[[ii]][jj] <- "   "
               }
           }
       }
    } else {
       tkmessageBox(message="Items must be of class list", title="ERROR", icon="error")
       return()
    }
    if (NRows == 0) { NRows <- Nitems }
    if (length(Width) < Ncol) { Width <- rep(Width[1], Ncol) }
    items <- as.data.frame(items)
    Tbl <- ttktreeview(parent,
                       columns = ColNames,
                       displaycolumns=(seq(1:Ncol)-1),
                       show = "headings",
                       height = NRows,
                       selectmode = "browse"
           )
    for(ii in 1:Ncol){
        tcl(Tbl, "heading", (ii-1), text=ColNames[ii])
        tcl(Tbl, "column", (ii-1), width=Width[ii])
    }
    tkgrid(Tbl, row = 1, column = 1, padx = c(5, 0), pady = c(5, 0), sticky="w")
    updateTable(widget=Tbl, items=items)
    return(Tbl)
}


#Editable table_list
#' @title DFrameTable
#' @description DFrameTable makes a table to edit items of a data.frame object
#' @param Data == NAME of the object of class data.frame containing
#'          the items to be listed in DFrameTable if parent != NULL
#'        Data == DATA.FRAME containing the items to be listed in 
#'          DFrameTable if parent == NULL
#' @param Title, the name of the DFrameTable object
#' @param ColNames the names of the DFrameTable columns
#' @param RowNames the names of the DFrameTable rownames
#' @param Width the width of the DFrameTable columns
#' @param Modify logical TRUE (default) to modify the selected item FALSE to simply return it
#' @param Env the environment containing the object of class data.frame to print
#' @param parent the ID of the parent container
#' @param Row the row where to place the DFrameTable using tkgrid()
#' @param Column the column where to place the DFrameTable using tkgrid()
#' @param Border border width in pixels
#' @export
#ATTENTION: Data <- DFrameTable() can be inserted in an existing widget specifying the 'parent'
#           (ttkframe() or ttklabelframe() in the calling main program. In this case 'Data' 
#           is a string representing the data.frame  to be shown in the DFrameTable. 
#           'Data' is loaded from the environment Env() and assign() and return(Data) are made.
#   When 'parent' pointer is NULL, a tktoplevel() is automatically generated to contain the DFrameTable
#   In this case 'Data' is the data.frame to be shown in the DFrameTable(). Also in this case
#   Env must be specified to assign() and return(Data).


DFrameTable <- function(Data, Title, ColNames="", RowNames="", Width=10, Modify=TRUE, Env,
                        parent=NULL, Row=NULL, Column=NULL, Border=c(10,10,10,10)){

  EditItem <- function(Idx, width){  #Idx == index of the selected TableList column
       MWx <- as.numeric(tclvalue(tkwinfo("rootx", DFFrame)))  #coord X of DFrameWin
       MWy <- as.numeric(tclvalue(tkwinfo("rooty", DFFrame)))
       LBx <- as.numeric(tclvalue(tkwinfo("rootx", ListBox[[Idx]])))  #coord X of LBox
       LBy <- as.numeric(tclvalue(tkwinfo("rooty", ListBox[[Idx]])))

       ItemIdx <- tclvalue(tcl(ListBox[[Idx]], "curselection"))
       SelectedItem <- tclvalue(tcl(ListBox[[Idx]], "get", ItemIdx))
       #selected item X Y coords, height and width
       xy.W.H <- tclvalue(tcl(ListBox[[Idx]], "bbox", ItemIdx))
       xy.W.H <- unlist(strsplit(xy.W.H, " "))
       X <- as.numeric(xy.W.H[1]) + LBx-MWx-1 #add relative position of List
       Y <- as.numeric(xy.W.H[2]) + LBy-MWy-1
       H <- as.numeric(xy.W.H[3])
       W <- as.numeric(xy.W.H[4])
       DataIdx <- as.numeric(ItemIdx)+1  #+1 because items in listbox start from row=0
       EntryFrame <- ttkframe(DFrameWin, borderwidth=1, padding=c(0,0,0,0))
       tkplace(EntryFrame, x=X, y=Y)
       NWI <- tclVar(as.character(Data[[Idx]][DataIdx]))
       Entry <- tkentry(EntryFrame, textvariable=NWI, width=width, takefocus=1, foreground="red")
       tkgrid(Entry)
       tkbind(Entry, "<Map>", function() { tkfocus(Entry)} ) #forces focus and cursor in TKEntry DFrameWindow
       tkbind(Entry, "<Key-Return>", function(K){
                    tkfocus(Entry)
                    tkconfigure(Entry, foreground="black")
#                    NewItem <- tclvalue(tkget(Entry)) #gets the entered new-text
                    NewItem <- tclvalue(NWI) #gets the entered new-text
                    if (NewItem != ""){
                        tcl(ListBox[[Idx]], "delete", ItemIdx) #delete original item
                        tcl(ListBox[[Idx]], "insert", ItemIdx, as.character(NewItem)) #insert new edited item
                    }
                    #save the NewItem in the original DataFrame
                    if (is.numeric(Data[[Idx]][DataIdx])) {
                        Data[[Idx]][DataIdx] <<- as.numeric(NewItem)
                    } else if (is.character(Data[[Idx]][DataIdx])){
                        Data[[Idx]][DataIdx] <<- as.character(NewItem)
                    }
                    tkdestroy(EntryFrame)
             })
   }

#--- Initial Control ---
   if (exists("DFWinExists", envir=.GlobalEnv) == TRUE){   #If DFrameWin already opened cannot open another one
       tkmessageBox(message="DFrame Window already opened", title="WARNING", icon="warning")
       return(Data)
   }

#--- Variables ---
   NItems <- nrow(Data)
   NCol <- ncol(Data)
   ListBox <- list()  #set a list() to save tklistbox IDs
   CLidx <- 0
   DFrameWin <- list()
   VarName <- NULL
   SelectedItem <- NULL

   if (is.null(parent)) {
       DFrameWin <- tktoplevel()
       assign("DFWinExists", TRUE, envir=.GlobalEnv)
       tkwm.title(DFrameWin, Title)
       tkwm.geometry(DFrameWin, "+200+200")
       Row <- Column <- 1
   } else {
       DFrameWin <- parent
       VarName <- Data
       Data <- get(VarName, envir=Env)
       NItems <- nrow(Data)
       NCol <- ncol(Data)
   }

   if(is.data.frame(Data) == FALSE){
      tkmessageBox(message="ATTENTION: 'data.frame' is the required format for TableList",
                title="ERROR", icon="error")
                return(Data)
   }


#--- Widget ---

   # Create a frame to hold the TkListbox and scrollbars
   DFFrame <- ttkframe(DFrameWin, padding=Border)
   tkgrid(DFFrame, row = Row, column = Column)
   tkbind(DFrameWin, "<Destroy>", function(){
                       if (exists("DFWinExists", envir=.GlobalEnv) == TRUE){
                           remove("DFWinExists", envir=.GlobalEnv)
                       }
                    })

   RR <- CC <- 1
   if (RowNames[1] != ""){ #if param names absent RowNames[1] == RowNames
       if (length(RowNames) != NItems){
           tkmessageBox(message="Please control the number of Row Names", title="ERROR", icon="error")
           return()
       }
       RowFrame <- ttkframe(DFFrame, borderwidth=0, padding=c(0,0,0,0))
       tkgrid(RowFrame, row = 1, column = 1, padx=0, pady=0)
       CC <- 2
       RNW <- max(sapply(RowNames, function(x) nchar(x)))+1
       #insert a blank cells at the top of the column of RowNames
       if (ColNames[1] != ""){
           BlankCell <- tklistbox(RowFrame, selectmode = "single", font="Serif 10 bold",
                            height=1, width = RNW,
                            background="#E0E0E0", borderwidth=0)
           tkgrid(BlankCell, row=RR, column=1, padx=0, pady=0)
           tcl(BlankCell, "insert", "end", " ") #insert a blank cell before column names
           RR <- 2
       }
       #now insert the other cells = RowNames
       RWNames <- tklistbox(RowFrame, selectmode = "single", font="Serif 10 bold",
                            height=length(RowNames), width = RNW,
                            background="#E0E0E0", borderwidth=0)
       tkgrid(RWNames, row=RR, column=1, padx=0, pady=0)
       for(ii in 1:length(RowNames)){
           tcl(RWNames, "insert", "end", RowNames[ii])
       }
   }

   # Create a frame to hold the TkListbox
   ColFrame <- ttkframe(DFFrame, borderwidth=0, padding=c(0,0,0,0))
   tkgrid(ColFrame, row = 1, column = CC, padx=0, pady=0)

   if (length(Width) != NCol){  #All columns have the same width
       Width <- rep(Width, NCol)
   }
   for(ii in 1:NCol){
       if (ColNames[1] != ""){
          if (length(ColNames) != NCol){
              tkmessageBox(message="Please control the number of Column Names", title="ERROR", icon="error")
              return(Data)
          }
          #row containing column names
          if (ColNames[1] != ""){
              CLNames <- tklistbox(ColFrame, selectmode = "single",
                                  height=1, width = Width[ii], font="Serif 10 bold",
                                  background="#E0E0E0", borderwidth=0)
              tcl(CLNames, "insert", "end", ColNames[ii])
              tkgrid(CLNames, row=1, column=ii, padx=0, pady=0,  sticky="w") #sticky="news")
              CLidx <- 1
              RR <- 2
          }
       }

       # Create a TkListbox widget for each column of Data
       ListBox[[ii]] <- tklistbox(ColFrame, selectmode = "single",
                                  height=NItems, width = Width[ii], borderwidth=0)
       tkgrid(ListBox[[ii]], row = RR, column = ii, padx=0, pady=0,  sticky="w") #sticky = "news")

       # Populate the TkListbox widgets with data
       for (jj in 1:NItems) {
            if (is.numeric(Data[[ii]][jj])){
                tcl(ListBox[[ii]], "insert", "end", Data[[ii]][jj]) #insert data from column ii
            } else {
                tcl(ListBox[[ii]], "insert", "end", as.character(Data[[ii]][jj])) #insert data from column ii
            }
       }
       tkbind(ListBox[[ii]], "<Double-1>", function() {
              for(jj in 1:NCol){
                  if (tclvalue(tcl(ListBox[[jj]], "curselection")) != ""){
                      ii <- jj
                      break
                  }
              }
              if (Modify == TRUE ){
                  EditItem(ii, Width[ii])
              } else {
                  ItemIdx <- tclvalue(tcl(ListBox[[ii]], "curselection"))
#                  SelectedItem <- tclvalue(tcl(ListBox[[ii]], "get", ItemIdx))
                  ItemIdx <- as.numeric(ItemIdx)+1  #+1 because items in listbox start from row=0
                  SelectedItem <<- Data[[ii]][ItemIdx]
              }
       })
   }

   if (is.null(parent)){
       BtnFrame <- ttkframe(DFrameWin, padding=c(0,0,0,0))
       tkgrid(BtnFrame, row = 2, column = 1, sticky="w")

       SaveBtn <- tkbutton(BtnFrame, text="  SAVE & EXIT  ", width=12, command=function(){
                            tkdestroy(DFrameWin)
                            remove("DFWinExists", envir=.GlobalEnv)
                            XPSSaveRetrieveBkp("save")
                          })
       tkgrid(SaveBtn, row = 1, column = 1, padx = 5, pady = 5, sticky="w")
       xx <- as.numeric(tkwinfo("reqwidth", SaveBtn))+10

       CancelButt <- tkbutton(BtnFrame, text="  CANCEL  ", width=10, command=function(){
                            tkdestroy(DFrameWin)
                            remove("DFWinExists", envir=.GlobalEnv)
                            XPSSaveRetrieveBkp("save")
                          })
       tkgrid(CancelButt, row = 1, column = 1, padx = c(5+xx, 0), pady = 5, sticky="w")
       tkfocus(DFrameWin)
       tkwait.window(DFrameWin)
#THE RETURN HAS TO BE PLACED HERE!
#When using tkwait.window(DFwin), the function enters a waiting loop
#until the DFrameWin window is destroyed. When DFrameWin is
#killed we exit the tkwait.window(DFrameWin) loop  and the return is correctly executed.
#For this we place return(Data) here!
       return(Data)
   } else {
       if (Modify == TRUE){
           SetBtn <- tkbutton(DFFrame, text=" SET CHANGES ", command=function(){
                            remove("DFWinExists", envir=.GlobalEnv)
                            XPSSaveRetrieveBkp("save")
                            assign(VarName, Data, envir=Env) #modified data has to be assigned using the original name
                            return(Data)
                          })
           tkgrid(SetBtn, row = 2, column = 1, padx = 0, pady = 5, sticky="w")
       } else {
           SetBtn <- tkbutton(DFFrame, text=" SELECT THE RSF ", command=function(){
                            remove("DFWinExists", envir=.GlobalEnv)
                            assign(VarName, SelectedItem, envir=Env) #modified data has to be assigned using the original name
                            return(Data)
                          })
           tkgrid(SetBtn, row = 2, column = 1, padx = 0, pady = 5, sticky="w")
       }
   }
}



#' @title xps.info_GUI Info GUI for XPSSample
#' @description xps.info_GUI XPSSample info GUI
#' @param xps.sample_name name of XPSSample
#' @author Roberto Canteri \email{canteri@fbk.eu}
#' @export
xps.info_GUI <- function(xps.sample_name = NULL) {
   if (! is.null(xps.sample_name) )
   {
     winInfo <- tktoplevel() ; tktitle(winInfo) <- paste(" Info window for XPSSample :", xps.sample_name)
     block <- ttkframe(winInfo, borderwidth = 10, padding=c(5,5,5,5))
     tkgrid(block, sticky="news", padx = 10, pady = 5)
     widget <- tktext(block)
     addScrollbars(block, widget = widget, type = "both")
     out <- capture.output(show(get(xps.sample_name, envir = .GlobalEnv)))
     tcl(widget, "insert", "end", paste(out, collapse="\n") )
     tkgrid(ttkbutton(winInfo, text = "Exit", width = 10, command = function() { tkdestroy(winInfo) }))
   }
}


#' @title is.notempty
#' @description is.notempty  is value missing, null, 0-length or NA length 1
#' @param x object to test
#' @return logical
#' @export
is.notempty <- function(x) {
   y <- tclvalue(x)
   nchar(y) != 0
   # missing(x) ||
   #   is.null(x) ||
   #   length(x) == 0 ||
   #   (is.atomic(x) && length(x) == 1 && is.na(x))
}

#' @title switch_tcl_widget
#' @description switch_tcl_widget switches the widget ON/OFF
#' @param widget widget
#' @param state state corresponding \code{normal} == "enabled"or , \code{disabled}.
#' @author Roberto Canteri \email{canteri@fbk.eu}
#' @export
switch_tcl_widget <- function(widget, state = c("normal", "disabled")) {
   state <- match.arg(state)
   childID <- tclvalue(tkwinfo("children",widget))
   selected <- sapply(unlist(strsplit(childID, " ")), function(x) {
   tkconfigure(x, "-state", state)})
}



#' @title yscale.components.logpower10ticks
#' @description yscale.components.logpower10ticks plots log ticks.
#' @param lim lim.
#' @param logsc logsc.
#' @param at at.
#' @param ... other plot parameters
#' @export
#'
yscale.components.logpower10ticks <- function(lim, logsc = FALSE, at = NULL, ...) {
    ans <-
      yscale.components.log10ticks(lim = lim,
                                   logsc = logsc,
                                   at = at,
                                   ...)
    idx <- which(ans$left$ticks$tck > 0.5)
    ans$left$labels$labels[idx] <-
      parse(text = paste(logsc, "^", log(
        as.numeric(ans$left$labels$labels[idx]), logsc
      ), sep = ""))
    ans
  }

#' @title yscale.components.power10ticks
#' @description yscale.components.power10ticks function to label the ticks plot with exponential format.
#' @param lim lim.
#' @param packet.number packet.number.
#' @param packet.list packet.list.
#' @param right right.
#' @param ... other plot parameters
#' @export
#'
yscale.components.power10ticks <- function(lim,
           packet.number = 0,
           packet.list = NULL,
           right = TRUE,
           ...) 
  {
    ans <-
      yscale.components.default(
        lim,
        packet.number = packet.number,
        packet.list = packet.list,
        right = right,
        ...
      )
    at <- ans$left$labels$at
    eT <- floor(log10(abs(at)))# at == 0 case is dealt with below
    mT <- at / 10 ^ eT
    ss <- lapply(seq(along = at),
                 function(i) {
                   if (at[i] == 0)
                     quote(0)
                   else
                     substitute(A %*% 10 ^ E, list(A = mT[i], E = eT[i]))
                 })
    ans$left$labels$labels <- do.call("expression", ss)
    ans
  }





# ## 
# ##' update xps.coreline table
# ##' 
# ##' update xps.coreline table after any modification of any XPS Sample
# ##'
# ##' @param object XPSSample class item
# ##' @param widget widget to update
# ##' @export
# update_xps_corelines <- function(object, widget) { 
#   CorelineNames <- names(object)
#   if (nlevels(as.factor(CorelineNames)) != length(CorelineNames)) {
#     All_names <- paste(CorelineNames, seq_along(CorelineNames), sep="#")
#   }
#   else { All_names <- CorelineNames }
#   
#   df.corelines <- data.frame("CoreLine" = All_names, "Info" = sapply(object, function(x) {
#     if ( hasComponents(x) ) { sprintf("CoreLine with n.%d Fit Components", length(slot(x, "Components"))) }
#     else { sprintf("XPS Core Line") } })
#   )
#   return(df.corelines)
# }


########  NON FUNZIONA  ##############
#' @title SetWindowFont
#' @description SetFont sets the font of all the window childrens
#' @param widget the widget container
#' @param Font a list composed by elements $family=font_name, $size=font_size, weigth="normal" or "bold"
#' @param child the child where to set the font
#' @export

#  SetWindowFont <- function(widget, Font) {
#       #creates the UserFont personal font
#       UserFont <- tclVar()
#       UserStyle <- tclVar()
#       UserFont <- tcl("font", "create", UserFont, family=Font$family, size=Font$size, weight=Font$weight)
#       #obtain all the children of the given widget
#       childID <- tclvalue(tkwinfo("children", widget))
#       #set the desired Font to the children
#       selected <- sapply(unlist(strsplit(childID, " ")), function(x) {
##                             Optn <- tkconfigure(x)  #get child options
##                             if (length(grep("font", Optn)) > 0){ #controls if 'font' option is present
##                                 tcl("ttk::style", "configure", UserStyle, font=c(Font$family, Font$size, Font$weight), foreground="black")
#                                 tkconfigure(x, font=UserFont)
##                                 tkfont.configure(x, family=Font$family, size=Font$size, weight=Font$weight)  #set user Font to child
##                             }
#                          })
#       Done <- tcl("update", "idletasks")   #conclude pending event before exiting function
#  }
