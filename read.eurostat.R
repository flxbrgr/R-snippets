## read.eurostat() is a convenience function to replace read.delim() when
##   importing datasets from Eurostat (http://epp.eurostat.ec.europa.eu/portal/page/portal/eurostat/home/).
## read.eurostat() takes eurostat tsv.gz files and reads them after 
##   transformation with read.delim().
## tsv.gz (ie. gzip compressed and tabulator-separated) files are available via
##   bulk download or from the database. You need to register for free to be able to download *.tsv.gz files.

read.eurostat <- function(name, na = ":", destfile = td <- tempdir(), 
  as.is = TRUE, var.name = NULL, flush = TRUE, aggregates = FALSE, blank.lines.skip = TRUE, 
  strip.white = TRUE, ...) {
  ## name is the coded name of the variable. For example if you wanted to download annual GDP at current prices
  ##   name would equal "nama_gdp_c".
  ## if destfile is not specified, the data will be downloaded to a temporary directory
  ## var.name can be freely specified by you and will be the colnames() of your variable.
  ## #F# TODO aggregates is unused
  
  name <- paste(gsub(".tsv.gz", "", name), ".tsv.gz", sep = "")
  
  ## bulk download
  url <- paste("http://epp.eurostat.ec.europa.eu/NavTree_prod/everybody/BulkDownloadListing?sort=1&file=data%2F", 
    name, sep = "")
  download.file(url, file.path(destfile, name))
  
  x <- readLines(file.path(destfile, name))
  ## some NA observations seem to slip through
  x <- gsub(":", "NA", x)
  ## also handles flagged NA observations
  x <- gsub("NA [[:alpha:]]", "NA", x)
  
  ## some datasets are not flat.
  has.comma <- any(grepl(",", x))
  if (has.comma) {
    x <- gsub(",", "\t", x)
    tmp <- tempfile()
    writeLines(x, tmp)
  }
  ret <- read.delim(file = if (has.comma) tmp else file.path(destfile, name), na = "NA", as.is = as.is, 
                    flush = flush, blank.lines.skip = blank.lines.skip, strip = strip.white, ...)
                    
  years <- grepl("[0-9]{4}", colnames(ret))
  ret[, years] <- apply(ret[, years], 2, function(x) gsub(":", NA_character_, x))
  ## remove EU-wide aggregates
  ret <- ret[-c(grep("^EU", ret$geo.time), grep("^EA", ret$geo.time)), ]
  
  if (!is.null(var.name) & !is.na(var.name)) {
    if (length(var.name) > 1) {
      var.name <- var.name[1]
    }
    colnames(ret) <- gsub("X", var.name, colnames(ret))
  }
  return(ret)
}
