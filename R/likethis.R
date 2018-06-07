# ------------------------------
# from xml file
# ------------------------------
get.xmllist <- function(xmlfile) {
  require(XML)
  require(methods)
  xmlToList(xmlParse(xmlfile))
}

revision.is.null <- function(page) {
  unlist(lapply(page, is.null))
}

get.page <- function(xmlfile) {
  page <- get.xmllist(xmlfile)$page
  page <- lapply(1:length(page), function(i) {
				   if (names(page[i]) == "revision") {
					 page[[i]]}})
  page[!revision.is.null(page)]
}

# ------------------------------
# from page
# ------------------------------
get.id <- function(xmlfile) {
  page <- get.page(xmlfile=xmlfile)
  unlist(lapply(1:length(page), function(i) page[[i]]$id))
}

get.timestamp <- function(xmlfile) {
  page <- get.page(xmlfile=xmlfile)
  unlist(lapply(1:length(page), function(i) page[[i]]$timestamp))
}

get.datestamp <- function(rev=NULL, xmlfile) {
  timestamps <- get.timestamp(xmlfile=xmlfile)
  if (!is.null(rev)) timestamps[rev]
  unlist(lapply(timestamps, function(x) unlist(strsplit(x, "-"))[1]))
}

get.monthstamp <- function(rev=NULL, xmlfile) {
  timestamps <- get.timestamp(xmlfile=xmlfile)
  if (!is.null(rev)) timestamps[rev]
  unlist(lapply(timestamps, function(x) unlist(strsplit(x, "-"))[2]))
}

removeNewLines <- function(x) {
  gsub("[\r\n]", "", x) }

revisions.clean <- function(x) {
  removeNewLines(x)
}

get.revisions <- function(xmlfile) {
  page <- get.page(xmlfile=xmlfile)
  text <- lapply(1:length(page), function(i) page[[i]]$text)
  text <- sapply(text, `[[`, 1)
  lapply(text, revisions.clean)

}

Match <- function(x, pattern) {
  require(stringr)
  str_match_all(x, pattern)
}

revisions.get.refs <- function(x) {
  pattern <- "<ref(.*?)ref"
  listofmatrices <- Match(x, pattern)
  lapply(listofmatrices, function(mat) mat[,2])
}

revisions.get.old.refs <- function(x) {
  pattern <- "==Further reading==|==Literature==(.*?)==See also"
  listofmatrices <- Match(x, pattern)
  lapply(listofmatrices, function(mat) mat[,2])
}

get.refs <- function(xmlfile) {
  rv <- get.revisions(xmlfile=xmlfile)
  sapply(rv, revisions.get.refs)
}

get.old.refs <- function(xmlfile) {
  rv <- get.revisions(xmlfile=xmlfile)
  sapply(rv, revisions.get.old.refs)
}

Detect <- function(x, pattern) {
  str_detect(x, pattern)
}

refs.is.journal <- function(x) {
  pattern <- "doi|pmid"
  Detect(x, pattern)
}

refs.get.journal <- function(x, reverse=FALSE, parse=TRUE) {
  j <- refs.is.journal(x)
  if (reverse) j <- !j
  j <- x[j]
  if (parse) j <- lapply(j, refs.parse.journal)
  j
}

ref.is.field <- function(x, pattern=NULL) {
  if (is.null(pattern)) {
	pattern <- "vauthors|title|journal|volume|issue|pages|date|year|pmid|doi"}
  Detect(x, pattern)
}

ref.get.field <- function(x, reverse=FALSE, pattern=NULL, ...) {
  f <- ref.is.field(x, pattern=pattern, ...)
  if (reverse) f <- !f
  x[f]
}

clean <- function(jr) {
  jr1 <- lapply(jr, function(ref) lapply(ref, ref.get.field))
  jr2 <- lapply(jr1, function(ref) lapply(ref, function(field) {
		   ref.get.field(field, pattern="name|cite journal|Cite journal", reverse=T)}))
  jr3 <- lapply(jr2, function(ref) lapply(ref, function(field) {
									 lapply(field, function(f) trimws(f))}))
  lapply(jr3, function(ref) lapply(ref, function(field) {
									 lapply(field, function(f) gsub(("}}</"), "", f))}))
}

get.journals <- function(xmlfile) {
  rf <- get.refs(xmlfile=xmlfile)
  jr <- lapply(rf, refs.get.journal)
  jr <- clean(jr)
  jr
  #lapply(jr, function(j) lapply(j, function(ref) lapply(ref, function(r) unlist(strsplit(r, "="))))
}

# ------------------------------
# revisions
# ------------------------------

# ------------------------------
# revisions.refs
# ------------------------------
refs.get <- function(x, pattern) {
  d <- Detect(x, pattern)
  if (reverse) d <- !d
  x[d]
}

refs.is.news <- function(x) {
  pattern <- "news|web|website"
  Detect(x, pattern)
}

refs.get.news <- function(x, reverse=FALSE) {
  j <- refs.is.news(x)
  if (reverse) j <- !j
  x[j]
}

refs.get.undefined <- function(x) {
  not_j <- refs.get.journal(x, reverse=TRUE)
  refs.get.news(not_j, reverse=TRUE)
}

# ------------------------------
# ------------------------------

refs.is.parsable <- function(x, pattern) {
  Detect(x, pattern)
}

refs.get.parsable <- function(x, pattern, reverse=FALSE) {
  j <- refs.is.parsable(x, pattern)
  if (reverse) j <- !j
  x[j]
}

refs.parse.journal <- function(x){
  pattern <- "\\|"
  x <- refs.get.parsable(x, pattern)
  u <- unlist(strsplit(x, pattern))
  lapply(u, function(f) unlist(strsplit(f, "=")))
}

ref.is.field2 <- function(x) {
  pattern <- "name"
  Detect(x, pattern)
}

ref.get.field2 <- function(x, reverse=T) {
  f <- ref.is.field(x)
  if (reverse) f <- !f
  x[f]
}

ref.remove.field <- function(x, pattern, reverse=FALSE) {
  d <- Detect(x, pattern)
  if (reverse) {d <- !d}
  x[d]
}


refs.is.date <- function(x) {
  pattern <- "^(19|20)[:digit:]{2}$"
  Detect(x, pattern)
}

# x<- get.revisions()
get.journal <- function(x=NULL) {
  if (is.null(x)) x <- get.revisions()
  x <- revisions.get.refs(x)
  j <- refs.get.journal(x)
  j <- lapply(j, refs.parse.journal)
  j <- lapply(j, ref.get.field)
  j
}

#from page[[145]]
#==Literature==
#==Further reading==
#==External links==
