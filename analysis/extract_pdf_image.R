# analysis/extract_pdf_image.R
# ------------------------------------------------------------------------------
# PURPOSE
#   Pull a single embedded image out of a sitrep PDF and write it as a PNG.
#
# WHY THIS EXISTS
#   Some reports lay charts on top of the per-state table: 2022 week 2 draws the
#   age/sex pyramid and the cases-by-state bar chart over Table 3, hiding most of
#   it. The table is a raster underneath them, so cropping or zooming cannot
#   recover it - the covering objects have to be left undrawn. Extracting the
#   table's own image does that without editing the PDF.
#
# SCOPE
#   Analysis tooling, not part of the app. A PNG produced here is convenience
#   output for the sidecar, exactly as decision record 0007 describes rendered
#   pages: the protocol still treats the original PDF as the source, and the
#   ledger's Image_File should name the report, not this derivative.
#
# LIMITS
#   Handles the shape NCDC reports actually use: /FlateDecode, 8 bits per
#   component, /DeviceRGB or /DeviceGray, with an optional /SMask composited
#   over white. Anything else (DCTDecode/JPEG, a /Predictor, JPXDecode, indexed
#   colour) raises an error naming what it found rather than writing something
#   wrong. Objects inside compressed object streams are not visible to this
#   parser; every NCDC report checked so far keeps images as top-level objects.
#
# USAGE (from the project root)
#   source("analysis/extract_pdf_image.R")
#   pdf_page_images("data/sitreps/lassa_sitrep_2022_w02_20220108.pdf", page = 4)
#   extract_pdf_image("data/sitreps/lassa_sitrep_2022_w02_20220108.pdf",
#                     page = 4, out = "data/sitreps/png/w02_table3.png")
#   # `which` defaults to the largest image on the page, which is the table in
#   # every layout seen so far; pass an object number to override.
# ------------------------------------------------------------------------------

# --- RAW OBJECT ACCESS --------------------------------------------------------
# Nul bytes are swapped for 0x01 so the file can be searched as a string without
# shifting a single byte offset; stream bytes are always re-read from the
# original raw vector at those offsets.
.pdf_open <- function(path) {
  stopifnot(file.exists(path))
  raw <- readBin(path, "raw", file.size(path))
  r2  <- raw; r2[r2 == as.raw(0)] <- as.raw(1)
  s   <- rawToChar(r2); Encoding(s) <- "latin1"

  m    <- gregexpr("[0-9]+[ ]+[0-9]+[ ]+obj", s)[[1]]
  if (m[1] == -1) stop("no PDF objects found in ", basename(path),
                       " (encrypted, or objects held in compressed object streams)")
  num  <- as.integer(sub("^([0-9]+).*", "\\1", substring(s, m, m + attr(m, "match.length") - 1)))
  ends <- gregexpr("endobj", s)[[1]]
  oend <- vapply(m, function(st) { e <- ends[ends > st]; if (length(e)) e[1] else nchar(s) }, numeric(1))

  list(raw = raw, s = s, m = m, num = num, oend = oend)
}

# Header dictionary of object n, newlines flattened.
.pdf_dict <- function(d, n) {
  i <- which(d$num == n)[1]
  if (is.na(i)) return(NA_character_)
  gsub("[\r\n]", " ", substr(substring(d$s, d$m[i], d$oend[i]), 1, 600))
}

# Inflated stream bytes of object n.
.pdf_stream <- function(d, n) {
  i  <- which(d$num == n)[1]
  ot <- substring(d$s, d$m[i], d$oend[i])
  len <- as.integer(sub(".*/Length[ ]*([0-9]+).*", "\\1", gsub("[\r\n]", " ", substr(ot, 1, 600))))
  if (is.na(len)) stop("object ", n, " has no direct /Length")
  r <- regexpr("stream\r?\n", ot)
  if (r < 0) stop("object ", n, " has no stream")
  start <- d$m[i] + r - 1 + attr(r, "match.length")
  memDecompress(d$raw[start:(start + len - 1)], type = "gzip")
}

.num_field <- function(dict, key) {
  r <- regexpr(paste0("/", key, "[ ]*[0-9]+"), dict)
  if (r < 0) return(NA_integer_)
  as.integer(sub(paste0("/", key, "[ ]*"), "", regmatches(dict, r)))
}
.name_field <- function(dict, key) {
  r <- regexpr(paste0("/", key, "[ ]*/[A-Za-z0-9]+"), dict)
  if (r < 0) return(NA_character_)
  sub(paste0("/", key, "[ ]*/"), "", regmatches(dict, r))
}

# Page objects in document order, from the /Pages tree's /Kids array.
.pdf_page_objs <- function(d) {
  dicts <- vapply(d$num, function(n) .pdf_dict(d, n), character(1))
  tree  <- d$num[grepl("/Type[ ]*/Pages", dicts)]
  if (length(tree)) {
    i <- which(d$num == tree[1])[1]
    full <- substring(d$s, d$m[i], d$oend[i])
    kids <- regmatches(full, regexpr("/Kids[ ]*\\[[^]]*\\]", full))
    if (length(kids)) {
      ord <- as.integer(regmatches(kids, gregexpr("[0-9]+(?=[ ]+[0-9]+[ ]+R)", kids, perl = TRUE))[[1]])
      if (length(ord)) return(ord)
    }
  }
  # Fall back to file order if the tree is not a simple flat /Kids list.
  d$num[grepl("/Type[ ]*/Page[^s]", dicts)]
}

# --- PUBLIC -------------------------------------------------------------------

# pdf_page_images(path, page)
#   One row per image XObject referenced by that page, largest first.
pdf_page_images <- function(path, page) {
  d     <- .pdf_open(path)
  pages <- .pdf_page_objs(d)
  if (page > length(pages)) stop("page ", page, " does not exist (", length(pages), " pages)")

  i    <- which(d$num == pages[page])[1]
  full <- substring(d$s, d$m[i], d$oend[i])
  xo   <- regmatches(full, regexpr("/XObject[ ]*<<[^>]*>>", full))
  if (length(xo) == 0) return(data.frame())

  nm  <- regmatches(xo, gregexpr("/[A-Za-z0-9]+[ ]+[0-9]+[ ]+[0-9]+[ ]+R", xo))[[1]]
  out <- do.call(rbind, lapply(nm, function(entry) {
    name <- sub("^/([A-Za-z0-9]+).*", "\\1", entry)
    obj  <- as.integer(sub("^/[A-Za-z0-9]+[ ]+([0-9]+).*", "\\1", entry))
    dd   <- .pdf_dict(d, obj)
    if (is.na(dd) || !grepl("/Subtype[ ]*/Image", dd)) return(NULL)
    data.frame(name = name, obj = obj,
               width  = .num_field(dd, "Width"),
               height = .num_field(dd, "Height"),
               bpc    = .num_field(dd, "BitsPerComponent"),
               filter = .name_field(dd, "Filter"),
               colorspace = .name_field(dd, "ColorSpace"),
               smask  = .num_field(dd, "SMask"),
               predictor = grepl("/Predictor", dd),
               stringsAsFactors = FALSE)
  }))
  if (is.null(out)) return(data.frame())
  out$megapixels <- round(out$width * out$height / 1e6, 2)
  out[order(-out$megapixels), ]
}

# extract_pdf_image(path, page, which, out)
#   `which` is an object number; default is the largest image on the page.
#   Returns the path written, invisibly.
extract_pdf_image <- function(path, page, which = NULL, out = NULL) {
  imgs <- pdf_page_images(path, page)
  if (nrow(imgs) == 0) stop("no image XObjects on page ", page, " of ", basename(path))

  row <- if (is.null(which)) imgs[1, ] else imgs[imgs$obj == which, ]
  if (nrow(row) == 0) stop("object ", which, " is not an image on page ", page)

  if (!identical(row$filter, "FlateDecode"))
    stop("object ", row$obj, " uses /", row$filter, "; only FlateDecode is handled. ",
         "A DCTDecode image is already a JPEG and can be copied out of the stream verbatim.")
  if (isTRUE(row$predictor))
    stop("object ", row$obj, " uses a /Predictor; not handled")
  if (!identical(row$bpc, 8L))
    stop("object ", row$obj, " has ", row$bpc, " bits per component; only 8 is handled")
  nch <- switch(row$colorspace, DeviceRGB = 3L, DeviceGray = 1L,
                stop("object ", row$obj, " colour space /", row$colorspace, " is not handled"))

  d <- .pdf_open(path)
  w <- row$width; h <- row$height
  px <- as.integer(.pdf_stream(d, row$obj)) / 255
  if (length(px) != w * h * nch)
    stop("object ", row$obj, ": inflated ", length(px), " bytes, expected ", w * h * nch)

  arr <- if (nch == 3L) aperm(array(px, dim = c(3, w, h)), c(3, 2, 1))
         else array(t(matrix(px, nrow = w, ncol = h)), dim = c(h, w, 1))

  # A soft mask is alpha, not colour. Composite over white rather than writing
  # RGBA: the sidecar shows the image on a light page, and an unmasked image
  # renders its transparent areas black, which hides half a banded table.
  if (!is.na(row$smask)) {
    a  <- as.integer(.pdf_stream(d, row$smask)) / 255
    if (length(a) == w * h) {
      al <- t(matrix(a, nrow = w, ncol = h))
      for (ch in seq_len(dim(arr)[3])) arr[, , ch] <- arr[, , ch] * al + (1 - al)
    } else {
      warning("SMask ", row$smask, " is ", length(a), " bytes, expected ", w * h, "; ignored")
    }
  }

  if (is.null(out)) {
    out <- file.path("data", "sitreps", "png",
                     sub("[.]pdf$", sprintf("_p%d_obj%d.png", page, row$obj), basename(path)))
  }
  dir.create(dirname(out), recursive = TRUE, showWarnings = FALSE)
  png::writePNG(if (dim(arr)[3] == 1L) arr[, , 1] else arr, out)
  message(sprintf("wrote %s (%dx%d from object %d)", out, w, h, row$obj))
  invisible(out)
}
