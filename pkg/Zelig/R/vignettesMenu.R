## adds a vignette menu for zelig packages (hacked from Seth's code )

addVigs2WinMenu <- function(pkgName) {
   if ((.Platform$OS.type == "windows") && (.Platform$GUI == "Rgui")
       && interactive()) {
       vigFile <- system.file("Meta", "vignette.rds", package=pkgName)
       if (!file.exists(vigFile)) {
           warning(sprintf("%s contains no vignette, nothing is added to the menu bar", pkgName))
       } else {
           vigMtrx <- .readRDS(vigFile)
           vigs <- file.path(.find.package(pkgName), "doc", vigMtrx[,"PDF"])
           names(vigs) <- vigMtrx[,"Title"]

           if (!"Vignettes" %in% winMenuNames())
             winMenuAdd("Vignettes")
           pkgMenu <- paste("Vignettes", pkgName, sep="/")
           winMenuAdd(pkgMenu)
           for (i in vigs) {
               item <- sub(".pdf", "", basename(i))
               winMenuAddItem(pkgMenu, item, paste("shell.exec(\"", as.character(i), "\")", sep = ""))
           }
       } ## else
       ans <- TRUE
   } else {
       ans <- FALSE
   }
   ans
}
