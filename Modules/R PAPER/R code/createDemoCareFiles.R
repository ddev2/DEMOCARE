currPath <- dirname(rstudioapi::getActiveDocumentContext()$path)
setwd(currPath)
source ("democare_compute.R")

answer <- rstudioapi::showQuestion("Read a DEMOCARE file with extension *.dat",
                                   "Output directory same as input one?", ok = "Same", cancel = "Different")
filePath <- rstudioapi::selectFile(caption = "Select DEMOCARE File",
                                   filter = "DAT Files (*.dat)",
                                   path = currPath,
                                   existing = TRUE)

if (is.null(filePath)) stop("no file")

pathOut <- paste (dirname(filePath), getDirSep(), sep="")
if (answer==FALSE) {
  path <- rstudioapi::selectDirectory(
    caption = "Select Output Directory",
    label = "Select",
    path = pathOut
  )

  if (!is.null(path)) {
    pathOut <- paste (path, getDirSep(), sep="")
  }
}


createDemoCareFiles(filePath, pathOut=pathOut)
