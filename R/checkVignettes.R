# Vignette Checks ---------------------------------------------------------

checkVignetteDir <- function(.BiocPackage)
{
    pkgdir <- .BiocPackage$sourceDir
    vigdir <- .BiocPackage$vignettesDir

    if (!dir.exists(vigdir)) {
        handleError("No 'vignettes' directory.")
        return()
    }

    vigdircontents <- .BiocPackage$VigSources
    if (!length(vigdircontents)) {
        handleError("No vignette sources in vignettes/ directory.")
        return()
    }

    checkInstContents(.BiocPackage)

    checkVigFiles(.BiocPackage)

    checkVigBuilder(.BiocPackage)

    checkVigMetadata(vigdircontents)

    checkVigTypeRNW(.BiocPackage)

    checkVigEngine(.BiocPackage)

    checkVigSuggests(.BiocPackage)

    checkVigTemplate(vigdircontents)

    checkVigChunkEval(vigdircontents)

    checkDupChunkLabels(vigdircontents)

    checkVigBiocInst(.BiocPackage)

    checkVigInstalls(.BiocPackage)

    checkVigClassUsage(.BiocPackage)

    checkTFSymbolUsage(.BiocPackage)

    checkVigSessionInfo(.BiocPackage)

    checkVigEvalAllFalse(.BiocPackage)
}

checkInstContents <- function(.BiocPackage)
{
    instdocdir <- file.path(.BiocPackage$sourceDir, "inst", "doc")
    contents <- list.files(
        instdocdir,
        pattern = "\\.Rmd$|\\.Rnw$|\\.Rrst$|\\.Rhtml$|\\.Rtex$",
        ignore.case = TRUE, full.names = TRUE
    )
    if (length(contents) && .BiocPackage$isSourceDir)
        handleWarning(
            "Remove vignette sources from inst/doc; ",
            "they belong in vignettes/."
        )
}

checkVigFiles <- function(.BiocPackage) {
    vigdir <- .BiocPackage$vignettesDir
    vigdircontents <- .BiocPackage$VigSources
    vigs <- tolower(basename(vigdircontents))
    allvigfiles <- setdiff(
        tolower(
            list.files(
                vigdir, all.files = TRUE, ignore.case = TRUE, recursive = TRUE
            )
        ),
        vigs
    )

    if (length(allvigfiles) != 0) {
        badFiles <- unlist(lapply(vigs,
                           FUN = function(x, allvigfiles){
                               vl <- tools::file_path_sans_ext(x)
                               badext <- c(".tex", ".html", ".pdf",
                                           ".aux", ".log")
                               ext <- paste0(vl, badext)
                               fnd <- intersect(allvigfiles, ext)
                               fnd
                           },
                           allvigfiles = allvigfiles))
        if (length(badFiles) != 0){
            handleNote(
                "Potential intermediate files found:",
                messages = paste0("vignettes/", badFiles)
            )
        }
    }
}

checkVigBuilder <- function(.BiocPackage)
{
    builders <- .BiocPackage$VigBuilder
    builders <- builders[builders != "Sweave"]
    if (!length(builders))
        return()
    vigdircontents <- .BiocPackage$VigSources
    # check DESCRIPTION is in at least one vignette
    vigExt <- tolower(tools::file_ext(vigdircontents))
    vigdircontents <- vigdircontents[vigExt != "rnw"]

    badBuilder <- character(0)
    for (builder in builders) {
        res <- vapply(
            vigdircontents, isEngineInBuilder, logical(1), builder = builder
        )
        if (any(!res, na.rm=TRUE)) {
            badBuilder <- c(badBuilder, builder)
        }
    }
    # check if a listed builder is not found in any vignette
    if (length(badBuilder)) {
        handleError(
            "'VignetteBuilder' listed in DESCRIPTION but not ",
            "found as 'VignetteEngine' in any vignettes:",
            messages = badBuilder
        )
    }
}

checkVigTypeRNW <- function(.BiocPackage) {
    vigdircontents <- .BiocPackage$VigSources
    vigExt <- tolower(tools::file_ext(vigdircontents))
    isRNW <- vigExt == "rnw"
    vigNames <- basename(vigdircontents[isRNW])
    if (length(vigNames))
        handleWarning(
            "Use RMarkdown instead of Sweave 'Rnw' vignettes.",
            help_text = "Rnw vignette(s) found:",
            messages = vigNames
        )
}

checkVigMetadata <- function(vigdircontents)
{
    badVig <- character(0)
    vigExt <- tolower(tools::file_ext(vigdircontents))
    dx <- which(vigExt != "rnw")
    vigdircontents <- vigdircontents[dx]
    for (file in vigdircontents) {
        lines <- readLines(file, n=100L, warn=FALSE)
        idx <- grep(lines, pattern="vignette:")
        if (length(idx) == 0L)
            badVig <- c(badVig, basename(file))
    }
     if (length(badVig) != 0L){
        handleWarning(
            "Vignette(s) missing Vignette metadata. See ",
            "http://r-pkgs.had.co.nz/vignettes.html",
            help_text = "Update the following files:",
            messages = badVig
        )
    }
}

checkVigEngine <- function(.BiocPackage)
{
    builder <- .BiocPackage$VigBuilder
    vigdircontents <- .BiocPackage$VigSources
    # check Engines are in DESCRIPTION
    vigExt <- tolower(tools::file_ext(vigdircontents))
    vigdircontents <- vigdircontents[vigExt != "rnw"]

    # check for very rare case that multiple build
    # engines specified in vignette
    res <- lapply(vigdircontents, getVigEngine)
    if (any(lengths(res) > 1L)) {
        handleErrorFiles(
            "More than one VignetteEngine specified.",
            messages = names(which(lengths(res) > 1L))
        )
    }
    # check for missing engine
    if (any(!lengths(res))) {
        handleError(
            "No 'VignetteEngine' specified in vignette.",
            help_text = "Add 'VignetteEngine' to the following files:",
            messages = names(res[!lengths(res)])
        )
    }
    vigdircontents <- vigdircontents[lengths(res) == 1L]
    if (length(vigdircontents)) {
        res <- vapply(
            vigdircontents, isEngineInBuilder, logical(1), builder = builder
        )
        # check for missing engine in DESCRIPTION
        if (any(!res)) {
            handleError(
                "'VignetteEngine' specified but not in the DESCRIPTION.",
                help_text =
                    "Add 'VignetteEngine' to DESCRIPTION from the following:",
                messages = names(res[!res])
            )
        }
        # check for missing engine in vignette
        if (anyNA(res)) {
            nadx <- which(is.na(res))
            files <- names(res[nadx])
            if (is.null(builder))
                files <- c(files, "DESCRIPTION")
            handleError(
                "No 'VignetteEngine' specified in vignette or DESCRIPTION. ",
                help_text = paste(
                    "Add a 'VignetteEngine' to the following files or",
                    "a default 'VignetteBuilder' in DESCRIPTION: "
                ),
                messages = files
            )
        }
    }
}

checkVigSuggests <- function(.BiocPackage)
{
    builder <- .BiocPackage$VigBuilder
    vigdircontents <- .BiocPackage$VigSources
    vigExt <- tolower(tools::file_ext(vigdircontents))
    res <- lapply(vigdircontents, getVigEnginePkg)
    lst <- unique(c(unlist(unname(res)), builder))
    if (any(is.na(lst)))
        lst <- lst[!is.na(lst)]
    dep <- .BiocPackage$dependencies
    if (!all(lst %in% dep)){
        handleWarning(
            "Package listed as VignetteEngine or VignetteBuilder ",
            "but not currently Suggested. ",
            help_text = "Add the following to Suggests in DESCRIPTION:",
            messages = lst[!(lst %in% dep)]
        )
    }
}

checkVigTemplate <- function(vigdircontents)
{
    badVig <- character(0)
    badVig2 <- character(0)
    for (file in vigdircontents) {
        lines <- readLines(file, warn=FALSE)
        if (identical(tolower(tools::file_ext(file)), "rmd"))
            lines <- .getYAMLfront(lines)
        idx <- grep(lines, pattern="VignetteIndexEntry")
        if (length(idx)) {
            title <- tolower(gsub(".*\\{|\\}.*", "", lines[idx]))
            if (identical(title, "vignette title"))
                badVig <- c(badVig, basename(file))
        }
        if (!length(idx))
            badVig2 <- c(badVig2, basename(file))
    }
    if (length(badVig))
        handleWarning(
            "Vignette(s) still using 'VignetteIndexEntry{{Vignette Title}}' ",
            help_text = "The following files use template defaults:",
            messages = badVig
        )
    if (length(badVig2))
        handleWarning(
            "Vignette(s) missing '\\%VignetteIndexEntry{{Vignette Title}}'. ",
            help_text = "Update the following files:",
            messages = badVig2
        )
}

checkVigChunkEval <- function(vigdircontents)
{
    chunks <- 0
    efs <- 0
    noneval <- 0
    for (file in vigdircontents)
    {
        lines <- readLines(file, warn=FALSE)
        vignetteType <- knitr:::detect_pattern(lines, tools::file_ext(file))
        if (is.null(vignetteType)) {
            chunklines <- character(0)
            nonEvalChunk <- character(0)
        } else {
            chunkPattern <- knitr::all_patterns[[vignetteType]]$chunk.begin
            chunklines <- lines[grep(chunkPattern, lines)]

            # find non evaluated code chunks (```, ```r, ```R, etc.)
            # assumes every other one for start and stop of code chunk
            nonEvalChunk <- lines[grep("^[\t >]*```+\\s*", lines)]
            if (length(nonEvalChunk)) {
                nonEvalChunk <- nonEvalChunk[c(TRUE,FALSE)]
                indx <- grep("^[\t >]*```+\\s*\\{([a-zA-Z0-9_]+.*)\\}\\s*$",
                             nonEvalChunk)
                if (length(indx))
                    nonEvalChunk <- nonEvalChunk[-indx]
            }
        }
        chunks <- chunks + length(chunklines) + length(nonEvalChunk)

        efs <- efs +
            length(grep("eval\\s*=\\s*F(ALSE)?", chunklines))

        noneval <- noneval + length(nonEvalChunk)
    }

    totnon <- efs + noneval
    percent <- ifelse(
        chunks == 0 && totnon == 0,
        0L,
        as.integer((totnon * 100 / chunks))
    )

    if (percent >= 50){
        handleWarning("Evaluate more vignette chunks.")
        msg <- sprintf(
            "%s out of %s code chunks = %i%% unevaluated",
            totnon,
            chunks,
            percent
        )
        handleMessage(msg, indent = 8)
        handleMessage(
            sprintf("%s non-exec code chunk(s) (e.g., '```r')", noneval),
            indent = 8
        )
    }
}

checkVigEvalAllFalse <- function(.BiocPackage) {
    vigfiles <- .BiocPackage$VigSources
    shortnames <- .getDirFiles(vigfiles)
    viglist <- structure(
        vector("logical", length(vigfiles)), .Names = shortnames
    )
    for (i in seq_along(vigfiles)) {
        shortName <- shortnames[i]
        tempR <- tempfile(fileext=".R")
        try_purl_or_tangle(input = vigfiles[i], output = tempR, quiet = TRUE)
        pfile <- parseFile(.BiocPackage, tempR)
        symbolOK <- .getTokenTextCode(
            parsedf = pfile,
            token = "SYMBOL_FUNCTION_CALL",
            text = "set",
            hasLookback = c("opts_chunk", "$")
        )
        if (nrow(symbolOK)) {
            setRange <- .findSymbolRanges(pfile, "set", "SYMBOL_FUNCTION_CALL")
            if (length(setRange) > 1L)
                warning("More than one `opts_chunk$set()` in ", vigfiles[i])
            datarange <- pfile[unlist(setRange), ]
            viglist[[shortName]] <-
                grepl("eval=F", paste(datarange$text, collapse = ""))
        }
    }
    if (any(viglist)) {
        handleWarningFiles(
            " Vignette set global option 'eval=FALSE'",
            messages = names(viglist[viglist])
        )
    }
}

checkDupChunkLabels <- function(vigfiles) {
    viglist <- structure(
        vector("logical", length(vigfiles)),
        .Names = vigfiles
    )
    for (vfile in vigfiles) {
        tempR <- tempfile(fileext=".R")
        tryCatch({
            quiet_knitr_purl(input = vfile, output = tempR, quiet = TRUE)
        }, error = function(e) {
            viglist[[vfile]] <<- grepl(
                "Duplicate chunk label", conditionMessage(e), fixed = TRUE
            )
            if (viglist[[vfile]])
                invisible(NULL)
            else
                stop(e)
        })
    }
    if (any(viglist))
        handleErrorFiles(
            " Vignette(s) found with duplicate chunk labels",
            messages = basename(vigfiles[viglist])
        )
}

.OLD_INSTALL_CALLS <-
    c("BiocInstaller", "biocLite", "useDevel", "biocinstallRepos")

checkVigBiocInst <- function(.BiocPackage) {
    msg_return <- findSymbolsInVignettes(
        .BiocPackage,
        Symbols = .OLD_INSTALL_CALLS,
        tokenTypes = c("COMMENT", "SYMBOL_FUNCTION_CALL")
    )
    if (length(msg_return)) {
        handleWarningFiles(
            " BiocInstaller code found in vignette(s)",
            messages = msg_return
        )
    }
}

.BAD_INSTALL_CALLS <- c("biocLite", "install.packages", "install_packages",
    "update.packages", "install")

checkVigInstalls <- function(.BiocPackage) {
    match_return <- findSymbolsInVignettes(
        .BiocPackage,
        Symbols = .BAD_INSTALL_CALLS,
        tokenTypes = "SYMBOL_FUNCTION_CALL"
    )
    grep_return <- findSymbolsInVignettes(
        .BiocPackage,
        Symbols = ".*install[^ed].*",
        tokenTypes = "SYMBOL_FUNCTION_CALL",
        FUN = .grepTokenTextCode
    )
    msg_return <- c(match_return, grep_return)
    if (length(msg_return)) {
        handleErrorFiles(
            "Installation calls found in vignette(s)",
            messages = unlist(msg_return, use.names = FALSE)
        )
    }
}

checkTFSymbolUsage <- function(.BiocPackage) {
    viglist <- findSymbolsInVignettes(.BiocPackage, c("T", "F"), "SYMBOL")
    if (length(viglist)) {
        handleWarningFiles(
            " Avoid T/F variables; If logical, use TRUE/FALSE",
            messages = unlist(viglist, use.names = FALSE)
        )
    }
}

## Completely suppress spurious knitr:::remind_sweave() warning that shows up
## in TclTk popup window in addition to the usual text-only warning.
quiet_knitr_purl <- function(...)
{
    args <- list(...)
    callr::r(
        function(...) suppressWarnings(knitr::purl(...)),
        args = args,
        env = c(CI = "true")
    )
}

purl_or_tangle <- function(input, output, quiet, ...) {
    vigEng <- getVigEnginePkg(input)
    vigExt <- tolower(tools::file_ext(input))
    if (!identical(vigExt, "rnw") || identical(vigEng, "knitr"))
        quiet_knitr_purl(input = input, output = output, quiet = quiet, ...)
    else
        utils::Stangle(file = input, output = output, quiet = quiet)
}

try_purl_or_tangle <- function(input, output, quiet, ...) {
    tryCatch({
        purl_or_tangle(input = input, output = output, quiet = quiet, ...)
    }, error = function(e) {
        hasDups <- grepl(
            "Duplicate chunk label", conditionMessage(e), fixed = TRUE
        )
        if (hasDups) {
            file.create(output)
            invisible(NULL)
        } else {
            stop(e)
        }
    })
}

checkVigClassUsage <- function(.BiocPackage) {
    vigfiles <- .BiocPackage$VigSources
    viglist <- structure(
        vector("list", length(vigfiles)), .Names = basename(vigfiles)
    )
    for (vfile in vigfiles) {
        tempR <- tempfile(fileext=".R")
        try_purl_or_tangle(input = vfile, output = tempR, quiet = TRUE)
        tokens <- getClassNEEQLookup(tempR)
        viglist[[basename(vfile)]] <- sprintf(
            "%s (code line %d, column %d)",
            basename(vfile), tokens[,"line1"], tokens[,"col1"]
        )
    }
    viglist <- Filter(length, viglist)
    if (length(viglist)) {
        handleWarningFiles(
            " Avoid class membership checks with class() / is() and == / !=",
            "; Use is(x, 'class') for S4 classes",
            messages = unlist(viglist, use.names = FALSE)
        )
    }
}

checkVigSessionInfo <- function(.BiocPackage) {
    vigfiles <- .BiocPackage$VigSources
    notFoundVig <- structure(
        vector("logical", length(vigfiles)), .Names = vigfiles
    )
    for (vfile in vigfiles) {
        pc <- structure(
            list(parseFile(.BiocPackage, vfile)), .Names = vfile
        )
        res <- findSymbolsInParsedCode(
            parsedCodeList = pc,
            symbolNames = c("sessionInfo", "session_info"),
            tokenTypes = "SYMBOL_FUNCTION_CALL"
        )
        if (!length(res)) {
            notFoundVig[[vfile]] <- TRUE
        }
    }
    if (any(notFoundVig)) {
        handleNote(
            " 'sessionInfo' not found in vignette(s)",
            help_text = "Missing from file(s):",
            messages = .getDirFiles(vigfiles[notFoundVig])
        )
    }
}
