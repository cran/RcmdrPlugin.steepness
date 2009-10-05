# Some Rcmdr dialogs for the steepness package

.First.lib <- function(libname, pkgname){
    if (!interactive()) return()
    Rcmdr <- options()$Rcmdr
    plugins <- Rcmdr$plugins
    if ((!pkgname %in% plugins) && !getRcmdr("autoRestart")) {
        Rcmdr$plugins <- c(plugins, pkgname)
        options(Rcmdr=Rcmdr)
        closeCommander(ask=FALSE, ask.save=TRUE)
        Commander()
        }
    }

Rcmdr.steeptest <- function(){
require(steepness)
require(tcltk)
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
        assign("namefile", justDoIt(command), envir=.GlobalEnv)
        if (namefile == "") return();
        if (name.labels == 0) {
          assign("temp", justDoIt("scan(namefile)"), envir=.GlobalEnv)
          assign("individuals", justDoIt("sqrt(length(temp))"), envir=.GlobalEnv)
          assign("X",justDoIt("matrix(temp,nrow=individuals,byrow=T)"),envir=.GlobalEnv)
          }
        if (name.labels == 1) {
          assign("X1", justDoIt("read.table(namefile)"), envir=.GlobalEnv)
          assign("names", justDoIt("rownames(X1)"), envir=.GlobalEnv)
          justDoIt("rownames(X1) <- NULL")
          justDoIt("colnames(X1) <- NULL")
          assign("X",justDoIt("as.matrix(X1)"),envir=.GlobalEnv)
        }
        if (is.numeric(X) == FALSE){
           errorCondition(recall=Rcmdr.steeptest, message="Invalid Data Type: Original sociomatrix must be numeric.")
            return()
            }

        command <- paste("as.numeric(",tclvalue(RandVar),")", sep="")
        assign("replications", justDoIt(command), envir=.GlobalEnv)
        if ( (is.na(replications)) | (replications < 1) | (replications > 1000000) ) {
            errorCondition(recall=Rcmdr.steeptest, message="The number of randomizations must be between 1 and 1000000.")
            return()
            }
	tkfocus(CommanderWindow())

        if (name.labels == 0) {doItAndPrint("test <- steeptest(X,replications)")}
        if (name.labels == 1) {doItAndPrint("test <- steeptest(X,replications,names)")}
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
require(tcltk)
initializeDialog(title=gettextRcmdr("Steepness Plot"))
checkBoxes(frame="checkboxframe2",boxes="name.options", initialValues="0", labels=gettextRcmdr
          ("            File Includes Row and Column Names"))
onOK <- function(){
	closeDialog()
        name.labels <- tclvalue(name.optionsVariable)
        command <- "tclvalue(tkgetOpenFile(filetypes='{{Text files} {.txt}} 
                {{Data files} {.dat}} {{All files} *}'))"
        assign("namefile", justDoIt(command), envir=.GlobalEnv)
        if (namefile == "") return();
        if (name.labels == 0) {
          assign("temp", justDoIt("scan(namefile)"), envir=.GlobalEnv)
          assign("individuals", justDoIt("sqrt(length(temp))"), envir=.GlobalEnv)
          assign("X",justDoIt("matrix(temp,nrow=individuals,byrow=T)"), envir=.GlobalEnv)
          }
        if (name.labels == 1) {
          assign("X1", justDoIt("read.table(namefile)"), envir=.GlobalEnv)
          assign("names", justDoIt("rownames(X1)"), envir=.GlobalEnv)
          justDoIt("rownames(X1) <- NULL")
          justDoIt("colnames(X1) <- NULL")
          assign("X",justDoIt("as.matrix(X1)"), envir=.GlobalEnv)
        }
        if (is.numeric(X) == FALSE){
           errorCondition(recall=Rcmdr.steeptest, message="Invalid Data Type: Original sociomatrix must be numeric.")
            return()
            }
        tkfocus(CommanderWindow())
        if (name.labels == 0) {doItAndPrint("getplot(X)")}
        if (name.labels == 1) {doItAndPrint("getplot(X,names)")}
	}
OKCancelHelp(helpSubject="getplot")
tkgrid(labelRcmdr(top, text=""))
tkgrid(labelRcmdr(top, text=gettextRcmdr("            Original Sociomatrix will be loaded after OK"), fg="blue"), sticky="w")
tkgrid(checkboxframe2, columnspan=2, sticky="w")
tkgrid(labelRcmdr(top, text=""))
tkgrid(buttonsFrame, sticky="w", columnspan=2)
dialogSuffix(rows=5, columns=2, focus=checkboxframe2)
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
