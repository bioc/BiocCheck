checkManDocumentation <- function(.BiocPackage, libloc)
{
    package_dir <- .BiocPackage$sourceDir
    package_name <- .BiocPackage$packageName
    # canned man prompts
    checkForPromptComments(.BiocPackage)

    # non empty value section exists
    checkForValueSection(.BiocPackage)

    # exports are documented and 80% runnable
    checkExportsAreDocumented(.BiocPackage, lib.loc = libloc)

    # usage of donttest and dontrun
    checkUsageOfDont(.BiocPackage)
}

checkForPromptComments <- function(.BiocPackage)
{
    manfiles <- .BiocPackage$manSources
    bad <- vapply(
        manfiles,
        function(manpage) {
            lines <- readLines(manpage, warn=FALSE)
            any(grepl("^%%\\s+~", lines))
        },
        logical(1L)
    )

    if (any(bad))
        handleNote(
            "Auto-generated '%% ~' comments found in Rd man pages.",
            messages = names(bad)[bad]
        )
}

.tagListExtract <- function(rd, tags, Tag) {
    if (missing(tags))
        tags <- tools:::RdTags(rd)
    if (!Tag %in% tags)
        character(0L)
    else
        unlist(rd[tags == Tag], recursive = FALSE)
}

.tagsExtract <- function(rd, tags, Tag) {
    tagList <- .tagListExtract(rd = rd, tags = tags, Tag = Tag)
    as.character(tagList)
}

.valueInParsedRd <- function(rd, tags) {
    tagList <- .tagListExtract(rd, tags, "\\value")
    values <- Filter(function(x) attr(x, "Rd_tag") != "COMMENT", tagList)
    value <- paste(values, collapse = "")
    nzchar(trimws(value)) && length(values)
}

.parse_Rd_pack <- function(manpage, usesRdpack) {
    sysfile_rdpack <- system.file(package = "Rdpack")
    rdpack_avail <- nzchar(sysfile_rdpack)
    if (usesRdpack && rdpack_avail)
        rdmacros <- file.path(
            sysfile_rdpack, "help", "macros", "refmacros.Rd"
        )
    else
        rdmacros <- file.path(R.home("share"), "Rd", "macros", "system.Rd")

    tools::parse_Rd(manpage, macros = rdmacros)
}

.read_all_rds <- function(manpages, usesRdpack) {
    lapply(
        manpages,
        function(manpage, usesRdpack) {
            .parse_Rd_pack(manpage, usesRdpack)
        },
        usesRdpack = usesRdpack
    )
}

.formatsInParsedRd <- function(rd, tags) {
    formats <- .tagsExtract(rd, tags, "\\format")
    value <- paste(formats, collapse = "")
    nzchar(trimws(value)) && length(formats)
}

.skipRdCheck <- function(rd, tags) TRUE

.whichRdCheck <- function(docType) {
    switch(
        docType,
        package =,
        class = .skipRdCheck,
        data = .formatsInParsedRd,
        fun =,
        .valueInParsedRd
    )
}

checkForValueSection <- function(.BiocPackage)
{
    all_rds <- .read_all_rds(.BiocPackage$manSources, .BiocPackage$usesRdpack)
    all_tags <- lapply(all_rds, tools:::RdTags)
    docTypes <- mapply(docType, rd = all_rds, tags = all_tags, SIMPLIFY = FALSE)
    docTypes[!lengths(docTypes)] <- "fun"
    funs <- lapply(docTypes, .whichRdCheck)
    isData <- unlist(docTypes) == "data"
    ok <- mapply(
        function(afun, rds, atags) {
            afun(rds, atags)
        },
        afun = funs, rds = all_rds, atags = all_tags,
        SIMPLIFY = TRUE
    )
    dataOK <- ok[isData]
    elseOK <- ok[!isData]
    if (!all(dataOK)) {
        not_oks <- names(ok[isData][!dataOK])
        handleWarningFiles(
            "Empty or missing \\format sections found in data man page(s).",
            messages = not_oks
        )
    }
    if (!all(elseOK)) {
        not_oks <- names(ok[!isData][!elseOK])
        handleWarningFiles(
            "Empty or missing \\value sections found in man page(s).",
            messages = not_oks
        )
    }
}

# Which pages document things that are exported?
checkExportsAreDocumented <- function(.BiocPackage, lib.loc)
{
    pkgdir <- .BiocPackage$sourceDir
    pkgname <- .BiocPackage$packageName
    uses_rd_pack <- .BiocPackage$usesRdpack
    manpages <- .BiocPackage$manSources
    pkg_ns <- loadNamespace(pkgname, lib.loc = lib.loc)
    exports <- getNamespaceExports(pkg_ns)
    ## attempt to unload package namespace
    try(unloadNamespace(pkg_ns), silent = TRUE)
    badManPages <- character(0)
    exportingPagesCount <- 0L
    noExamplesCount <- 0L

    for (manpage in manpages)
    {
        rd <- .parse_Rd_pack(manpage, usesRdpack = uses_rd_pack)
        name <-
            unlist(rd[unlist(lapply(rd, function(x)
                attr(x, "Rd_tag") == "\\name"))][[1]][1])
        aliases <- unlist(lapply(rd[unlist(lapply(rd,
            function(x) attr(x, "Rd_tag") == "\\alias"))], "[[", 1))
        namesAndAliases <- c(name, aliases)
        exportedTopics <- unique(namesAndAliases[namesAndAliases %in% exports])
        if (length(exportedTopics))
        {
            exportingPagesCount <- exportingPagesCount + 1
        }
        if (length(exportedTopics) &&
            !doesManPageHaveRunnableExample(rd))
        {
            noExamplesCount <- noExamplesCount + 1
            badManPages <- append(badManPages, basename(manpage))
        }
    }

    ratio <- (exportingPagesCount - noExamplesCount) / exportingPagesCount

    if (exportingPagesCount > 0 && ratio  < 0.8)
        handleError(
            "At least 80% of man pages documenting exported objects must ",
            "have runnable examples.",
            help_text = "The following pages do not:",
            messages = badManPages
        )
    else if (length(badManPages))
        handleNote(
            "Consider adding runnable examples to man pages that document ",
            "exported objects.",
            messages = badManPages
        )

    badManPages # for testing
}

checkUsageOfDont <- function(.BiocPackage)
{
    manpages <- .BiocPackage$manSources

    hasBad <- rep(FALSE, length(manpages))
    hasdontrun <- rep(FALSE, length(manpages))
    uses_rd_pack <- .BiocPackage$usesRdpack
    for (dx in seq_along(manpages))
    {
        manpage <- manpages[dx]
        rd <- .parse_Rd_pack(manpage, usesRdpack = uses_rd_pack)
        example <- unlist(lapply(rd,
            function(x) attr(x, "Rd_tag") == "\\examples"))
        hasExamples <- any(example)
        if (hasExamples){
            rdCode <- as.character(rd)
            exampleCode <- rdCode[which(rdCode == "\\examples"):length(rdCode)]
            donttestVec <- vapply(exampleCode, grepl, logical(1),
                                  pattern="\\\\donttest", perl=TRUE,
                                  USE.NAMES=FALSE)
            dontrunVec <- vapply(exampleCode, grepl, logical(1),
                                  pattern="\\\\dontrun", perl=TRUE,
                                  USE.NAMES=FALSE)
            ## check for the 'internal' keyword - this will be a false positive
            keyword <- unlist(lapply(rd,
                function(x) attr(x, "Rd_tag") == "\\keyword"))
            if (any(keyword)) {
                internalVec <- vapply(
                    as.character(rd[keyword]), grepl, logical(1L),
                    pattern="internal", USE.NAMES=FALSE
                )
            } else {
                internalVec <- FALSE
            }
            if (any(donttestVec | dontrunVec) & !any(internalVec))
                hasBad[dx] <- TRUE

            if (any(dontrunVec) & !any(internalVec))
                hasdontrun[dx] <- TRUE
        }
    }
    if (any(hasBad)){
        perVl <- as.character(round(length(which(hasBad))/length(hasBad)*100))
        handleNoteFiles(
            "Usage of dontrun{} / donttest{} tags found in man page examples. ",
            paste0(perVl, "% of man pages use at least one of these tags."),
            messages = basename(manpages)[hasBad]
        )
    }
    if (any(hasdontrun)){
        handleNoteFiles(
            "Use donttest{} instead of dontrun{}.",
            messages = basename(manpages)[hasdontrun]
        )
     }

}

