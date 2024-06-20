
#' @exportClass BiocPackage
.BiocPackage <- setRefClass(
    "BiocPackage",
    fields = list(
        isValid = "logical",
        isTar = "logical",
        DESCRIPTION = "matrix",
        packageVersion = "character",
        sourceDir = "character",
        BiocCheckDir = "character",
        packageName = "character",
        metadata = "list"
    ),
    methods = list(
        initialize = function(packageDir, ...) {
            packageDir <- normalizePath(packageDir)
            .self[["isTar"]] <- grepl("\\.tar\\.[gx]z$", packageDir)
            .self$getPackageDir(packageDir)
            .self$readDESCRIPTION()
            callSuper(...)
        },
        getPackageDir = function(packageDir) {
            if (.self[["isTar"]]) {
                packageDir <- .self$untarTarball(packageDir)
            }
            if (identical(.Platform$OS.type, "windows"))
                packageDir <- gsub("\\\\", "/", packageDir)
            .self[["sourceDir"]] <- packageDir
        },
        BiocCheckDir = function(...) {
            checkDir <- list(...)[["checkDir"]]
            checkDir <- normalizePath(checkDir, winslash = "/")
            .self[["BiocCheckDir"]] <- file.path(
                checkDir, paste(pkgName, "BiocCheck", sep = ".")
            )
        },
        readDESCRIPTION = function() {
            desc <- file.path(.self[["sourceDir"]], "DESCRIPTION")
            dcf <- try({ read.dcf(desc) }, silent=TRUE)
            .self[["isValid"]] <- !inherits(dcf, "try-error")
            if (.self[["isValid"]]) {
                .self[["DESCRIPTION"]] <- dcf
                .self[["packageName"]] <- as.character(dcf[, "Package"])
                .self[["packageVersion"]] <- as.character(dcf[, "Version"])
            }
        },
        getPackageVersion = function() {
            .self[["packageVersion"]]
        },
        untarTarball = function(pkgTarball, tempDir = tempfile()) {
            if (!dir.exists(tempDir))
                dir.create(tempDir)
            suppressMessages({ untar(pkgTarball, exdir = tempDir) })
            tempDir
        }
    )
)
