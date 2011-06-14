# Some Rcmdr dialogs for the steepness package

.onAttach <- function(libname, pkgname){
  if (!interactive()) return()
  Rcmdr <- options()$Rcmdr
  plugins <- Rcmdr$plugins
  if (!pkgname %in% plugins) {
    Rcmdr$plugins <- c(plugins, pkgname)
    options(Rcmdr=Rcmdr)
    if("package:Rcmdr" %in% search()) {
      if(!getRcmdr("autoRestart")) {
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
      }
    }
    else {
      Commander()
    }
  }
}

if (getRversion() >= '2.15.1')globalVariables(c('top','namefile','datafile',
'X','temp','individuals','X1','names','replication','namesoptions.Variable','Rcmdr.steeptest',
'..values', 'name.optionsVariable', 'Rcmdr.steeptest','methodVariable', 'checkboxframe2', 
'methodFrame', 'buttonsFrame', 'DijVariable', 'DSVariable', 'NormDSVariable',
'ResultsVariable', 'checkBoxFrame', 'PijVariable'))

Rcmdr.steeptestDij <- function(){
require(steepness)
initializeDialog(title=gettextRcmdr("Steepness Test"))
RandVar <- tclVar("10000")
RandEntry <- tkentry(top, width="12", textvariable=RandVar)
checkBoxes(frame="checkBoxFrame", boxes=c("Dij", "DS", "NormDS","Results"), 
           initialValues=c("1", "1", "1","1"), 
           labels=gettextRcmdr(c("            Dyadic Dominance Indices", "            David's Scores", 
           "            Normalized David's Scores", "            Summary Statistics")))
checkBoxes(frame="checkboxframe2",boxes="name.options", initialValues="0", labels=gettextRcmdr
          ("            File Includes Row and Column Names"))
onOK <- function(){
	closeDialog()
	name.labels <- tclvalue(name.optionsVariable)  
	command <- "tclvalue(tkgetOpenFile(filetypes='{{Text files} {.txt}} 
                {{Data files} {.dat}} {{All files} *}'))"
	gassign("namefile",justDoIt(command))
	if (namefile == "") return() 
	if (name.labels == 0) {
	  gassign("temp",justDoIt("scan(namefile)"))
	  gassign("individuals",justDoIt("sqrt(length(temp))"))
	  gassign("X",justDoIt("matrix(temp,nrow=individuals,byrow=T)"))
	}
	if (name.labels == 1) {
	  gassign("X1",justDoIt("read.table(namefile,header=TRUE)"))
	  gassign("names",justDoIt("rownames(X1)"))
	  gassign("X",justDoIt("as.matrix(X1)"))
	}

	if (is.numeric(X) == FALSE){
	  errorCondition(recall=Rcmdr.steeptest, message="Invalid Data Type: Original sociomatrix must be numeric.")
	  return()
	}

command <- paste("as.numeric(",tclvalue(RandVar),")", sep="")
gassign("replication",justDoIt(command))
if ( (is.na(replication)) | (replication < 1) | (replication > 1000000) ) {
  errorCondition(recall=Rcmdr.steeptest, message="The number of randomizations must be between 1 and 1000000.")
  return()
}
	tkfocus(CommanderWindow())

        if (name.labels == 0) {doItAndPrint("test <- steeptest(X,replication,method='Dij')")}
        if (name.labels == 1) {doItAndPrint("test <- steeptest(X,replication,names,method='Dij')")}
        doItAndPrint("test$steepness")
        doItAndPrint("test$steep.right.pvalue")
	doItAndPrint("test$steep.left.pvalue")
        doItAndPrint("test$intercept")        

        if (tclvalue(DijVariable) == "1") {
          doItAndPrint("test$dyadic.dominance")
            }
        if (tclvalue(DSVariable) == "1") {
          doItAndPrint("test$david.scores")
            }
        if (tclvalue(NormDSVariable) == "1") {
          doItAndPrint("test$norm.david.scores")
            }
        if (tclvalue(ResultsVariable) == "1") {
          doItAndPrint("test$results")
            }
	if (name.labels == 1){
    justDoIt("remove(X1,namefile)")
	} else {justDoIt("remove(temp,individuals,namefile)")}
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject="steeptest")
tkgrid(labelRcmdr(top, text=""))
tkgrid(labelRcmdr(top, text=gettextRcmdr("            Original Sociomatrix will be loaded after OK"), fg="blue"), sticky="w")
tkgrid(checkboxframe2, columnspan=2, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(tklabel(top, text="            Number of Randomizations"), RandEntry, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(labelRcmdr(top, text=gettextRcmdr("            Results Options:"), fg="blue"), sticky="w")
tkgrid(checkBoxFrame, columnspan=2, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(buttonsFrame, sticky="w", columnspan=2)
tkgrid.configure(RandEntry, sticky="w")
dialogSuffix(rows=13, columns=2, focus=RandEntry)
}

Rcmdr.steeptestPij <- function(){
require(steepness)
initializeDialog(title=gettextRcmdr("Steepness Test"))
RandVar <- tclVar("10000")
RandEntry <- tkentry(top, width="12", textvariable=RandVar)
checkBoxes(frame="checkBoxFrame", boxes=c("Pij", "DS", "NormDS","Results"), 
           initialValues=c("1", "1", "1","1"), 
           labels=gettextRcmdr(c("            Matrix of Pij values", "            David's Scores", 
           "            Normalized David's Scores", "            Summary Statistics")))
checkBoxes(frame="checkboxframe2",boxes="name.options", initialValues="0", labels=gettextRcmdr
          ("            File Includes Row and Column Names"))
onOK <- function(){
	closeDialog()
	name.labels <- tclvalue(name.optionsVariable)  
	command <- "tclvalue(tkgetOpenFile(filetypes='{{Text files} {.txt}} 
                {{Data files} {.dat}} {{All files} *}'))"
	gassign("namefile",justDoIt(command))
	if (namefile == "") return() 
	if (name.labels == 0) {
	  gassign("temp",justDoIt("scan(namefile)"))
	  gassign("individuals",justDoIt("sqrt(length(temp))"))
	  gassign("X",justDoIt("matrix(temp,nrow=individuals,byrow=T)"))
	}
	if (name.labels == 1) {
	  gassign("X1",justDoIt("read.table(namefile,header=TRUE)"))
	  gassign("names",justDoIt("rownames(X1)"))
	  gassign("X",justDoIt("as.matrix(X1)"))
	}
	
	if (is.numeric(X) == FALSE){
	  errorCondition(recall=Rcmdr.steeptest, message="Invalid Data Type: Original sociomatrix must be numeric.")
	  return()
	}
	
	command <- paste("as.numeric(",tclvalue(RandVar),")", sep="")
	gassign("replication",justDoIt(command))
	if ( (is.na(replication)) | (replication < 1) | (replication > 1000000) ) {
	  errorCondition(recall=Rcmdr.steeptest, message="The number of randomizations must be between 1 and 1000000.")
	  return()
	}
	tkfocus(CommanderWindow())
	
	if (name.labels == 0) {doItAndPrint("test <- steeptest(X,replication,method='Pij')")}
	if (name.labels == 1) {doItAndPrint("test <- steeptest(X,replication,names,method='Pij')")}
	doItAndPrint("test$steepness")
	doItAndPrint("test$steep.right.pvalue")
	doItAndPrint("test$steep.left.pvalue")
	doItAndPrint("test$intercept")        
	
	if (tclvalue(PijVariable) == "1") {
	  doItAndPrint("test$dyadic.dominance")
	}
	if (tclvalue(DSVariable) == "1") {
	  doItAndPrint("test$david.scores")
	}
	if (tclvalue(NormDSVariable) == "1") {
	  doItAndPrint("test$norm.david.scores")
	}
	if (tclvalue(ResultsVariable) == "1") {
	  doItAndPrint("test$results")
	}
	if (name.labels == 1){
	  justDoIt("remove(X1,namefile)")
	} else {justDoIt("remove(temp,individuals,namefile)")}
	tkfocus(CommanderWindow())
}
OKCancelHelp(helpSubject="steeptest")
tkgrid(labelRcmdr(top, text=""))
tkgrid(labelRcmdr(top, text=gettextRcmdr("            Original Sociomatrix will be loaded after OK"), fg="blue"), sticky="w")
tkgrid(checkboxframe2, columnspan=2, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(tklabel(top, text="            Number of Randomizations"), RandEntry, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(labelRcmdr(top, text=gettextRcmdr("            Results Options:"), fg="blue"), sticky="w")
tkgrid(checkBoxFrame, columnspan=2, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(buttonsFrame, sticky="w", columnspan=2)
tkgrid.configure(RandEntry, sticky="w")
dialogSuffix(rows=13, columns=2, focus=RandEntry)
}

Rcmdr.steepplot <- function(){
require(steepness)
initializeDialog(title=gettextRcmdr("Steepness Plot"))
checkBoxes(frame="checkboxframe2",boxes="name.options", initialValues="0", labels=gettextRcmdr
          ("            File Includes Row and Column Names"))
radioButtons(window=top,name="method",initialValue=..values[1],
          buttons=c("Dij", "Pij"),
          values=c("dij", "pij"),
          labels=gettextRcmdr(c("            Steepness plot based on Dij values",
          "            Steepness plot based on Pij values")),
title=gettextRcmdr("            Choose a method for the steepness plot"))

onOK <- function(){
	closeDialog()
	name.labels <- tclvalue(name.optionsVariable)  
	command <- "tclvalue(tkgetOpenFile(filetypes='{{Text files} {.txt}} 
                {{Data files} {.dat}} {{All files} *}'))"
	gassign("namefile",justDoIt(command))
	if (namefile == "") return() 
	if (name.labels == 0) {
	  gassign("temp",justDoIt("scan(namefile)"))
	  gassign("individuals",justDoIt("sqrt(length(temp))"))
	  gassign("X",justDoIt("matrix(temp,nrow=individuals,byrow=T)"))
	}
	if (name.labels == 1) {
	  gassign("X1",justDoIt("read.table(namefile,header=TRUE)"))
	  gassign("names",justDoIt("rownames(X1)"))
	  gassign("X",justDoIt("as.matrix(X1)"))
	}
	
	if (is.numeric(X) == FALSE){
	  errorCondition(recall=Rcmdr.steeptest, message="Invalid Data Type: Original sociomatrix must be numeric.")
	  return()
	}
  
	method.option <- tclvalue(methodVariable)
        tkfocus(CommanderWindow())
	if (method.option == "dij"){
          if (name.labels == 0) {doItAndPrint("getplot(X,method='Dij')")}
          if (name.labels == 1) {doItAndPrint("getplot(X,names,method='Dij')")}
        }
	if (method.option == "pij"){
          if (name.labels == 0) {doItAndPrint("getplot(X,method='Pij')")}
          if (name.labels == 1) {doItAndPrint("getplot(X,names,method='Pij')")}
        }
  
	if (name.labels == 1){
	  justDoIt("remove(X1,namefile)")
	} else {justDoIt("remove(temp,individuals,namefile)")}
	tkfocus(CommanderWindow())
	}
OKCancelHelp(helpSubject="getplot")
tkgrid(labelRcmdr(top, text=""))
tkgrid(labelRcmdr(top, text=gettextRcmdr("            Original Sociomatrix will be loaded after OK"), fg="blue"), sticky="w")
tkgrid(checkboxframe2, columnspan=2, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(methodFrame, columnspan=2, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(buttonsFrame, sticky="w", columnspan=2)
dialogSuffix(rows=6, columns=2, focus=checkboxframe2)
}

Rcmdr.help.steepness <- function(){
   require(steepness)
   doItAndPrint("help(\"steepness\")")
   invisible(NULL)
}

Rcmdr.help.RcmdrPlugin.steepness <- function(){
   require(steepness)
   doItAndPrint("help(\"RcmdrPlugin.steepness\")")
   invisible(NULL)
}
