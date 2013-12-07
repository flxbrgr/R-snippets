cloudmade <- function(id = 96931, high.res = FALSE, 
  api.key, dim = c(256, 64)) {
  ## returns the tileserver url for the specified cloudmade map style id. Needs your cloudmade API key.
  
  ## id: ID of the desired mapstyle.
  ## high.res: logical, if TRUE the high resolution tiles will be downloaded.
  ## api.key: Your cloudmade API key
  ## dim: tile size, can be either 256 or 64 (px). Defaults to 256.
  
  url <- "http://tile.cloudmade.com"
  f <- file.path(url, api.key, paste0(id, ifelse(high.res, "@2x", "")), 
    dim[1], "{z}", "{x}", "{y}.png")
  f
}

leaflet <- function(center = c(52, 9), 
  zoom = 8, file, tileserver = "http://a.tile.stamen.com/toner-lite/{z}/{x}/{y}.png", 
  height = "500px", page = TRUE, popup.coordinates = c(52, 9), popup.text, popup.open = FALSE) {
  
  ## Returns a leaflet map in a HTML page. For more on leaflet.js see http://leafletjs.com/.
  ## Default output is HTML 5 file in a temporary directory.
  ## center: center of the map.
  ## zoom: Zoom level.
  ## file (optional): Output HTML is saved to file. If no file is specified and page = TRUE, 
  ##   a HTML file is saved to a temporary directory.
  ## tileserver: either specify one yourself or pass a cloudmade URL to it with cloudmade() or 
  ##   choose from "mapnik", "mapnik bw", "stamen", "stamen watercolor".
  ## height: height is specified in inline CSS.
  ## page (logical): page = TRUE returns the full page, page = FALSE only the code fragment for the map.
  ## popup.coordinates: rbind() more than one pairs of coordinates. e.g. rbind(c(50, 9), c(0, 0))
  ## popup.text: text for the popups. needs to have same length as pairs of coordinates.
  ## popup.open (logical): If popup is opened or closed.
  
  ## different tileservers
  tiles <- c(
    mapnik = "http://a.tile.openstreetmap.org/{z}/{x}/{y}.png", 
    "mapnik bw" = "http://a.www.toolserver.org/tiles/bw-mapnik/{z}/{x}/{y}.png",
    "stamen toner" = "http://a.tile.stamen.com/toner/{z}/{x}/{y}.png",
    "stamen watercolor" = "http://a.tile.stamen.com/watercolor/{z}/{x}/{y}.jpg")
  if (!grepl("^http", tileserver)) {
    tileserver <- tiles[grep(tileserver, names(tiles))[1]]
  }
  ## attribution text
  attrib <- list(
    Stamen = list(name = "Stamen", url = "stamen.com", license = "CC BY 3", 
      license.url = "http://creativecommons.org/licenses/by/3.0"),
    Cloudmade = list(url = "cloudmade.com", license = "CC BY SA 2", 
      license.url = "http://creativecommons.org/licenses/by-sa/2.0"),
    Mapnik = list(url = "stamen.com", license = "", license.url = "")
    )
  m <- gsub("(^https*://)(.*)", "\\2", tileserver)
  m <- sapply(strsplit(sapply(strsplit(m, "/"), "[", 1), "\\."), 
    function(x) x[length(x) - 1])
  which.m <- grep(m, names(attrib), ignore.case = TRUE)
  a <- attrib[[which.m]]
  attrib <- c("Map tiles by <a href=\"", paste0("http://", a$url), "\">", 
    names(attrib)[which.m], 
    "</a>, under <a href=\"", a$license.url, "\">", a$license, "</a>.",
    "Map data &copy; <a href=\"http://openstreetmap.org\">OpenStreetMap</a> contributors, ",
    "under <a href=\"http://creativecommons.org/licenses/by-sa/3.0\">CC BY SA</a>.")
  if (!grepl("stamen|cloudmade", tileserver))
    attrib <- attrib[9:10]
  attrib <- paste0(attrib, collapse = "")
  
  ## add popups
  if (is.null(nrow(popup.coordinates)))
    popup.coordinates <- rbind(popup.coordinates)
    
  ## If no popup text is provided, only the marker is added to the map.
  popup <- rep(!missingArg(popup.text), nrow(popup.coordinates))
  
  if (missingArg(popup.text)) {
    popup.text <- character(0)
  }
  length(popup.text) <- nrow(popup.coordinates)
  popup.txt <- paste0("L.marker([", apply(popup.coordinates, 1, paste0, collapse = ", "), 
      "]).addTo(map)", 
    ifelse(popup, paste0(".bindPopup('", popup.text, "')"), ""), 
    ifelse(popup.open, ".openPopup()", ""), ";", collapse = "\n") 
    
  ## If page = TRUE a full HTML 5 page is returned invisibly and saved to the file argument. If page = FALSE only the code
  ## needed for the map is returned.
  if(page) {
    output <- c("<!DOCTYPE html>", "<html lang=\"en\">", "<head>",
      "<meta content=\"text/html;charset=UTF-8\" http-equiv=\"content-type\">",
      "<link href=\"http://netdna.bootstrapcdn.com/bootstrap/3.03/css/bootstrap.min.css\" rel=\"stylesheet\">",
      "<link rel=\"stylesheet\" href=\"http://cdnjs.cloudflare.com/ajax/libs/leaflet/0.6.4/leaflet.min.css\">",
      "<style type=\"text/css\">", paste0("#map {height: ", height, ";width:100%}"),
      "</style>", "</head>", "<body>", "<div class=\"row well\"><div class=\"col-md-6 col-md-offset-3\"><div id=\"map\" class=\"thumbnail\"/></div></div>",
      "<!-- place js at the end of the body element -->",
      "<script src=\"http://cdnjs.cloudflare.com/ajax/libs/leaflet/0.6.4/leaflet.js\"></script>",
      "<script type=\"text/javascript\">",
      paste0("var map = L.map('map').setView([", paste(center, collapse = ","), "], ", zoom, ");"),
      paste0("L.tileLayer('", tileserver, "', {"),
      paste0("attribution: '", attrib, "'"),
      "}).addTo(map);",  popup.txt, "</script>", "</body>", "</html>")
    outfile <- ifelse(missingArg(file), tempfile("myleafletmap", fileext = ".html"), file)
    writeLines(output, outfile)
    invisible(output)
    } else {
      ret <- paste(c("<script>", paste0("var map = L.map('map').setView([", paste(center, collapse = ","), "], ", zoom, ");"),
      paste0("L.tileLayer('", tileserver, "', {"),
      "attribution: 'Map tiles by <a href=\"http://stamen.com\">Stamen Design</a>, under <a href=\"http://creativecommons.org/licenses/by/3.0\">CC BY 3.0</a>. Data by <a href=\"http://openstreetmap.org\">OpenStreetMap</a>, under <a href=\"http://creativecommons.org/licenses/by-sa/3.0\">CC BY SA</a>.'",
      "}).addTo(map);",  popup.txt, "</script>"), collapse = "\n")
      cat(ret)
    }
}
leaflet(center = c(52, 9), popup.coordinates = rbind(c(52, 9)), height = "500px", tile = "toner", page = TRUE, file = "~/Dropbox/R/leaflet/index.html")
