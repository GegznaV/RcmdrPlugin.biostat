#' ===========================================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
command_list_datasets_in_pkgs <- function() {
    doItAndPrint("## List datasets in attached R packages\ndata()")
}



#' ===========================================================================
#' @rdname Menu-window-functions
#' @export
#' @keywords internal
# Based on function from "Rcmdr"

window_read_from_pkg <- function() {
  env <- environment()
  datasets <- NULL
  initializeDialog(title = gettextRcmdr("Read Data From R Package"))
  dsname <- tclVar("")
  package <- NULL
  enterFrame <- tkframe(top)
  entryDsname <- ttkentry(enterFrame, width = "20", textvariable = dsname)
  packages <- sort(.packages())
  packages <- packages[!packages %in% c("base", "stats")]
  packages <- packages[sapply(packages, function(package) {
    ds <- data(package = package)$results
    if (nrow(ds) == 0) {
      return(FALSE)
    }
    ds <- ds[, "Item"]
    valid <- sapply(ds, is.valid.name)
    length(ds[valid]) > 0
  })]
  packageDatasetFrame <- tkframe(top)
  packageFrame <- tkframe(packageDatasetFrame)
  max.height <- getRcmdr("variable.list.height")
  packageBox <- tklistbox(packageFrame,
    height = min(
      max.height,
      length(packages)
    ), exportselection = "FALSE", selectmode = "single",
    background = "white"
  )
  packageScroll <- ttkscrollbar(packageFrame, command = function(...) tkyview(
      packageBox,
      ...
    ))
  tkconfigure(packageBox, yscrollcommand = function(...) tkset(
      packageScroll,
      ...
    ))
  for (p in packages) tkinsert(packageBox, "end", p)
  datasetFrame <- tkframe(packageDatasetFrame)
  datasetBox <- tklistbox(datasetFrame,
    height = max.height,
    exportselection = "FALSE", selectmode = "single", background = "white"
  )
  datasetScroll <- ttkscrollbar(datasetFrame, command = function(...) tkyview(
      datasetBox,
      ...
    ))
  tkconfigure(datasetBox, yscrollcommand = function(...) tkset(
      datasetScroll,
      ...
    ))
  onPackageSelect <- function() {
    assign("package", packages[as.numeric(tkcurselection(packageBox)) +
      1], envir = env)
    datasets <<- data(package = package)$results[, 3]
    valid <- sapply(datasets, is.valid.name)
    datasets <<- datasets[valid]
    tkdelete(datasetBox, "0", "end")
    for (dataset in datasets) tkinsert(
        datasetBox, "end",
        dataset
      )
    tkconfigure(datasetBox, height = min(max.height, length(datasets)))
    firstChar <- tolower(substr(datasets, 1, 1))
    len <- length(datasets)
    onLetter <- function(letter) {
      letter <- tolower(letter)
      current <- 1 + round(as.numeric(unlist(strsplit(
        tclvalue(tkyview(datasetBox)),
        " "
      ))[1]) * len)
      mat <- match(letter, firstChar[-(1:current)])
      if (is.na(mat)) {
        return()
      }
      tkyview.scroll(datasetBox, mat, "units")
    }
    onA <- function() onLetter("a")
    onB <- function() onLetter("b")
    onC <- function() onLetter("c")
    onD <- function() onLetter("d")
    onE <- function() onLetter("e")
    onF <- function() onLetter("f")
    onG <- function() onLetter("g")
    onH <- function() onLetter("h")
    onI <- function() onLetter("i")
    onJ <- function() onLetter("j")
    onK <- function() onLetter("k")
    onL <- function() onLetter("l")
    onM <- function() onLetter("m")
    onN <- function() onLetter("n")
    onO <- function() onLetter("o")
    onP <- function() onLetter("p")
    onQ <- function() onLetter("q")
    onR <- function() onLetter("r")
    onS <- function() onLetter("s")
    onT <- function() onLetter("t")
    onU <- function() onLetter("u")
    onV <- function() onLetter("v")
    onW <- function() onLetter("w")
    onX <- function() onLetter("x")
    onY <- function() onLetter("y")
    onZ <- function() onLetter("z")
    for (letter in c(letters, LETTERS)) {
      tkbind(
        datasetBox, paste("<", letter, ">", sep = ""),
        get(paste("on", toupper(letter), sep = ""))
      )
    }
    onClick <- function() tkfocus(datasetBox)
    tkbind(datasetBox, "<ButtonPress-1>", onClick)
  }
  onDatasetSelect <- function() {
    tclvalue(dsname) <- datasets[as.numeric(tkcurselection(datasetBox)) + 1]
  }
  firstChar <- tolower(substr(packages, 1, 1))
  len <- length(packages)
  onLetter <- function(letter) {
    letter <- tolower(letter)
    current <- 1 + round(as.numeric(unlist(strsplit(
      tclvalue(tkyview(packageBox)),
      " "
    ))[1]) * len)
    mat <- match(letter, firstChar[-(1:current)])
    if (is.na(mat)) {
      return()
    }
    tkyview.scroll(packageBox, mat, "units")
  }
  onA <- function() onLetter("a")
  onB <- function() onLetter("b")
  onC <- function() onLetter("c")
  onD <- function() onLetter("d")
  onE <- function() onLetter("e")
  onF <- function() onLetter("f")
  onG <- function() onLetter("g")
  onH <- function() onLetter("h")
  onI <- function() onLetter("i")
  onJ <- function() onLetter("j")
  onK <- function() onLetter("k")
  onL <- function() onLetter("l")
  onM <- function() onLetter("m")
  onN <- function() onLetter("n")
  onO <- function() onLetter("o")
  onP <- function() onLetter("p")
  onQ <- function() onLetter("q")
  onR <- function() onLetter("r")
  onS <- function() onLetter("s")
  onT <- function() onLetter("t")
  onU <- function() onLetter("u")
  onV <- function() onLetter("v")
  onW <- function() onLetter("w")
  onX <- function() onLetter("x")
  onY <- function() onLetter("y")
  onZ <- function() onLetter("z")
  for (letter in c(letters, LETTERS)) {
    tkbind(
      packageBox, paste("<", letter, ">", sep = ""),
      get(paste("on", toupper(letter), sep = ""))
    )
  }
  onClick <- function() tkfocus(packageBox)
  tkbind(packageBox, "<ButtonPress-1>", onClick)
  onOK <- function() {
    datasetName <- datasets[as.numeric(tkcurselection(datasetBox)) +  1]
    dsnameValue <- tclvalue(dsname)
    if (dsnameValue != "" && is.null(package)) {
      closeDialog()
      if (is.element(dsnameValue, listDataSets())) {
        if ("no" == tclvalue(checkReplace(
          dsnameValue,
          gettextRcmdr("Data set")
        ))) {
          if (GrabFocus()) {
            tkgrab.release(top)
          }
          tkdestroy(top)
          readDataFromPackage()
          return()
        }
      }
      save.options <- options(warn = 2)
      check <- try(eval(parse(text = logger(paste("data(",
        dsnameValue, ")",
        sep = ""
      ))), envir = .GlobalEnv),
      silent = TRUE
      )
      options(save.options)
      if (class(check) == "try-error") {
        errorCondition(
          recall = readDataFromPackage,
          message = sprintf(
            gettextRcmdr("Data set %s does not exist"),
            dsnameValue
          )
        )
        return()
      }
      activeDataSet(dsnameValue)
      tkfocus(CommanderWindow())
    }
    else {
      if (is.null(package)) {
        errorCondition(
          recall = readDataFromPackage,
          message = gettextRcmdr("You must select a package.")
        )
        return()
      }
      if (length(datasetName) == 0) {
        errorCondition(
          recall = readDataFromPackage,
          message = gettextRcmdr("You must select a data set.")
        )
        return()
      }
      if (is.element(datasetName, listDataSets())) {
        if ("no" == tclvalue(checkReplace(
          datasetName,
          gettextRcmdr("Data set")
        ))) {
          if (GrabFocus()) {
            tkgrab.release(top)
          }
          tkdestroy(top)
          readDataFromPackage()
          return()
        }
      }
      closeDialog()
      command <- paste0("data(", datasetName, ", package=\"", package, "\")")
      result <- justDoIt(command)
      logger(command)
      if (class(result)[1] != "try-error") {
        activeDataSet(datasetName)
      }
      tkfocus(CommanderWindow())
    }
  }
  onDataHelp <- function() {
    datasetName <- datasets[as.numeric(tkcurselection(datasetBox)) +
      1]
    dsnameValue <- tclvalue(dsname)
    if (dsnameValue == "") {
      dsnameValue <- datasetName
    }
    if (length(dsnameValue) == 0) {
      Message(gettextRcmdr("No data set selected."), type = "warning")
    } else if (is.null(package)) {
      doItAndPrint(paste("help(\"", dsnameValue, "\")",
        sep = ""
      ))
    } else {
      doItAndPrint(paste("help(\"", dsnameValue, "\", package=\"",
        package, "\")",
        sep = ""
      ))
    }
  }
  OKCancelHelp(helpSubject = "data")
  dataHelpButton <- buttonRcmdr(top,
    text = gettextRcmdr("Help on selected data set"),
    command = onDataHelp
  )
  tkgrid(labelRcmdr(packageDatasetFrame,
    text = gettextRcmdr("Package (Double-click to select)"),
    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"
  ),
  labelRcmdr(packageDatasetFrame, text = "   "), labelRcmdr(packageDatasetFrame,
    text = gettextRcmdr("Data set (Double-click to select)"),
    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"
  ),
  sticky = "w"
  )
  tkgrid(packageBox, packageScroll, sticky = "nw")
  tkgrid(datasetBox, datasetScroll, sticky = "nw")
  tkgrid(packageFrame, labelRcmdr(packageDatasetFrame, text = "   "),
    datasetFrame,
    sticky = "nw"
  )
  tkgrid(packageDatasetFrame, sticky = "w")
  tkgrid(labelRcmdr(top, text = gettextRcmdr("OR"), fg = "red"),
    sticky = "w"
  )
  tkgrid(labelRcmdr(enterFrame,
    text = gettextRcmdr("Enter name of data set:  "),
    fg = getRcmdr("title.color"), font = "RcmdrTitleFont"
  ),
  entryDsname,
  sticky = "w"
  )
  tkgrid(enterFrame, sticky = "w")
  tkgrid(dataHelpButton, sticky = "w")
  tkgrid(buttonsFrame, sticky = "ew")
  tkgrid.configure(packageScroll, sticky = "ns")
  tkgrid.configure(datasetScroll, sticky = "ns")
  tkbind(packageBox, "<Double-ButtonPress-1>", onPackageSelect)
  tkbind(datasetBox, "<Double-ButtonPress-1>", onDatasetSelect)
  dialogSuffix(focus = entryDsname)
}
