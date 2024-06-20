.readDESCRIPTION <- function(package_dir) {
    dcf <- try({
        read.dcf(file.path(package_dir, "DESCRIPTION"))
    }, silent=TRUE)
    .BiocCheck$validDESC <- !inherits(dcf, "try-error")
    if (.BiocCheck$validDESC)
        .BiocCheck$DESCRIPTION <- dcf
    dcf
}

.checkDESCRIPTION <- function(package_dir) {
    handleCheck("Checking if DESCRIPTION is well formatted...")
    dcf <- .readDESCRIPTION(package_dir)
    if (!.BiocCheck$validDESC) {
        handleError("DESCRIPTION is malformed.")
        handleMessage(dcf)
    }
    dcf
}

validMaintainer <- function() {
    handleCheck("Checking for valid maintainer...")
    dcf <- .BiocCheck$DESCRIPTION
    authr <- "Authors@R" %in% colnames(dcf)
    autmain <- c("Author","Maintainer") %in% colnames(dcf)
    if (authr && any(autmain))
        handleError(
            "Use Authors@R field not Author/Maintainer fields. Do not use both."
        )
    else if (any(autmain))
        handleError("Do not use Author/Maintainer fields. Use Authors@R.")
}

checkDESCRIPTIONFile <- function(package_dir) {
    dcf <- .BiocCheck$DESCRIPTION

    checkLicenseForRestrictiveUse(dcf[, "License"])
    checkDESCfields(dcf)
    checkBiocDepsDESC(dcf)
    checkPinnedDeps(dcf)
}

checkRemotesUsage <- function(pkgdir)
{
    dcf <- .BiocCheck$DESCRIPTION
    if (!length(dcf))
        dcf <- .readDESCRIPTION(pkgdir)
    if ("Remotes" %in% colnames(dcf))
        handleError(
            "Package dependencies must be on CRAN or Bioconductor.",
            " Remove 'Remotes:' from DESCRIPTION"
        )
}

checkNewPackageVersionNumber <- function(pkgdir)
{
    dcf <- .BiocCheck$DESCRIPTION
    if (!length(dcf))
        dcf <- .readDESCRIPTION(pkgdir)
    version <- dcf[, "Version"]
    if (!grepl("^0+[-.][0-9]+[-.][0-9]+$", version))
        handleWarning(
            "New package x version starting with non-zero value ",
            "(e.g., 1.y.z, 2.y.z); got ", sQuote(version), ".")
    if (!grepl("^[0-9]+[-.]99[-.][0-9]+$", version))
        handleError(
            "New package 'y' version not 99 (i.e., x.99.z)",
            "; Package version: ", version
        )
}

checkForVersionNumberMismatch <- function(package, package_dir)
{
    bn <- basename(package)
    bn <- sub(".tar.gz", "", bn, TRUE)
    ver <- strsplit(bn, "_")[[1]][2]
    dcf <- .BiocCheck$DESCRIPTION
    if (!length(dcf))
        dcf <- .readDESCRIPTION(package_dir)
    dcfVer <- unname(dcf[, "Version"])
    if (!ver == dcfVer)
    {
        handleError(
            "Version number in tarball filename must match Version field ",
            "in DESCRIPTION. (Tip: create tarball with R CMD build)")
    }
}

checkLazyDataUsage <- function(pkgdir)
{
    dcf <- .BiocCheck$DESCRIPTION
    if (!missing(pkgdir))
        dcf <- .readDESCRIPTION(pkgdir)
    if ("LazyData" %in% colnames(dcf) &&
        tools:::str_parse_logic(dcf[, "LazyData"]))
        handleNote(
            "'LazyData:' in the 'DESCRIPTION' should be set to false or removed"
        )
}

checkVersionNumber <- function(pkgdir)
{
    dcf <- .BiocCheck$DESCRIPTION
    if (!missing(pkgdir))
        dcf <- .readDESCRIPTION(pkgdir)
    version <- dcf[, "Version"]
    regex <- "^[0-9]+[-\\.]([0-9]+)[-\\.][0-9]+$"
    if(!grepl(regex, version))
    {
        handleError(
            "Invalid package Version, see ",
            "https://contributions.bioconductor.org/versionnum.html"
        )
        return()
    }
    tryCatch({
        pv <- package_version(version)
        x <- pv$major
        y <- pv$minor
        mod <- y %% 2
        isDevel <- identical(
            BiocManager:::.version_bioc("devel"), BiocManager::version()
        )
        bioc.mod <- ifelse(isDevel, 1, 0)
        if (x == 0) {
            handleMessage("Package version ", as.character(pv), "; pre-release")
        } else if (mod != bioc.mod) {
            shouldBe <- ifelse(isDevel, "odd", "even")
            vers <- ifelse(isDevel, "devel", "release")
            handleWarning(
                "y of x.y.z version should be ", shouldBe, " in ", vers)
        }

    }, error = function(e) {
        handleError("Invalid package version")
    })
}

getMaintainerEmail <- function(pkgdir)
{
    # Eventually update this to just look at Authors@R
    # Since the intention is to possible start running
    # this on the daily builder, leave Maintainer field
    # check. This is used to check for mailing list registration

    dcf <- .BiocCheck$DESCRIPTION
    if (!length(dcf))
        dcf <- .readDESCRIPTION(pkgdir)
    if ("Maintainer" %in% colnames(dcf))
    {
        m <- unname(dcf[, "Maintainer"])
        ret <- regexec("<([^>]*)>", m)[[1]]
        ml <- attr(ret, "match.length")
        email <- substr(m, ret[2], ret[2]+ml[2]-1)
    } else if ("Authors@R" %in% colnames(dcf)) {
        ar <- dcf[, "Authors@R"]
        env <- new.env(parent=emptyenv())
        env[["c"]] <- c
        env[["person"]] <- utils::person
        pp <- parse(text=ar, keep.source=TRUE)
        tryCatch(people <- eval(pp, env),
            error=function(e) {
                # could not parse Authors@R
                return(NULL)
            })
        for (person in people)
        {
            if ("cre" %in% person$role)
            {
                email <- person$email
            }
        }
    }
    return(email)
}

getAllDependencies <- function(pkgdir)
{
    dcf <- .BiocCheck$DESCRIPTION
    fields <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
    out <- c()
    for (field in fields)
    {
        if (field %in% colnames(dcf))
            out <- append(out, cleanupDependency(dcf[, field]))
    }
    out
}

checkRVersionDependency <- function(package_dir) {
    dcf <- .BiocCheck$DESCRIPTION
    if (!missing(package_dir))
        dcf <- .readDESCRIPTION(package_dir)
    if ("Depends" %in% colnames(dcf))
    {
        res <- cleanupDependency(dcf[, "Depends"], FALSE)
        if ("R" %in% res)
        {
            ind <- which(res == "R")
            verStr <- names(res)[ind]
            if (nchar(verStr))
            {
                pkgVer <- as.package_version(verStr)
                RVer <- package_version(
                    paste0(getRversion()[, c(1, 2)], ".0")
                )
                if (pkgVer < RVer)
                    handleNote(sprintf(
                        "Update R version dependency from %s to %s.",
                        pkgVer, RVer
                    ))
            }
        }
    }
}

isInfrastructurePackage <- function(pkgDir)
{
    dcf <- .BiocCheck$DESCRIPTION
    if (!nrow(dcf))
        return(FALSE)
    if (!"biocViews" %in% colnames(dcf))
    {
        return(FALSE)
    }
    biocViews <- dcf[, "biocViews"]
    views <- strsplit(gsub("\\s", "", biocViews), ",")[[1]]
    "Infrastructure" %in% views
}

.roxygen_in_desc <- function(pkgdir) {
    dcf <- .BiocCheck$DESCRIPTION
    "RoxygenNote" %in% colnames(dcf)
}

.LICENSE_DB_LOCATION <- "$R_HOME/share/licenses/license.db"

checkLicenseForRestrictiveUse <- function(license) {
    handleCheck("Checking License: for restrictive use...")

    if (!identical(length(license), 1L) || is.na(license)) {
        handleNote("malformed 'License:' field '", license, "'")
        return(invisible())
    }
    ldb_file <- file.path(R.home("share"), "licenses", "license.db")
    if (!file.exists(ldb_file)) {
        handleNote(
            "license database not found. ",
            "Expected location: '", ldb_file, "'. ",
            "License: '", license, "'"
        )
        return(invisible())
    }
    licenses <- read.dcf(ldb_file)
    result <- tools:::analyze_licenses(license, licenses)
    test <- result[["restricts_use"]]
    if (isTRUE(test))
        handleError("License '", license, "' restricts use")
    else if (is.na(test) || !result[, "is_verified"]) {
        handleNote(
            "License '", license, "' unknown; refer to ", .LICENSE_DB_LOCATION
        )
        handleMessage(
            "and https://choosealicense.com/appendix/ for more info.",
            indent = 6L
        )
    }
}

checkDESCfields <- function(dcf) {
    handleCheck("Checking for recommended DESCRIPTION fields...")

    fields <- c("URL", "BugReports")
    if ("Date" %in% colnames(dcf)) {
        date <- dcf[, "Date"]
        if (!grepl("^\\d{4}-\\d{2}-\\d{2}$", date))
            handleNote("'Date:' field format is not 'YYYY-MM-DD': ", date)
    }
    present <- fields %in% colnames(dcf)
    res <- fields[!present]
    if (length(res)) {
        notFields <- paste(shQuote(res), collapse = ", ")
        handleNote("Provide ", notFields, " field(s) in DESCRIPTION")
    }
}

checkBiocDepsDESC <- function(dcf, which = c("Depends", "Imports")) {
    handleCheck("Checking for Bioconductor software dependencies...")
    which_fields <- dcf[, colnames(dcf) %in% which]
    all_deps <- unlist(
        lapply(which_fields, function(x) strsplit(x, ",\\s*")[[1L]]),
        use.names = FALSE
    )
    all_deps <- gsub("(\\w+)\\s+\\(.*\\)$", "\\1", all_deps)
    all_deps <- all_deps[all_deps != "R"]
    repo <- BiocManager:::.repositories_bioc(BiocManager::version())["BioCsoft"]
    biocdb <- utils::available.packages(repos = repo)
    bioc_deps <- all_deps %in% rownames(biocdb)
    percent <- unname(round(prop.table(table(bioc_deps))["TRUE"], 2L) * 100)

    if (!any(bioc_deps)) {
        views <- .parseBiocViews(dcf = dcf)
        handleFUN <-
            if ("Infrastructure" %in% views) handleNote else handleWarning
        msg <- "No Bioconductor dependencies detected. Note that some
            infrastructure packages may not have Bioconductor dependencies.
            For more information, reach out to the Bioconductor community
            and/or consider a CRAN submission."
        handleFUN(msg)
    } else {
        handleMessage(
            "Bioconductor dependencies found in Imports & Depends (",
            percent,
            "%)."
        )
    }
}

checkPinnedDeps <- function(dcf) {
    handleCheck("Checking for pinned package versions in DESCRIPTION...")
    deps <- c("Depends", "Imports", "Suggests", "Enhances", "LinkingTo")
    validdeps <- deps[deps %in% colnames(dcf)]
    doubleeq <- grepl("==", dcf[, validdeps], fixed = TRUE)
    if (any(doubleeq))
        handleError("Dependencies in the DESCRIPTION file contain '=='")
}
