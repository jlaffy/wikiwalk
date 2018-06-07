library(XML)
library(stringr)
library(rowr)

source("likethis.R")

get.fields <- function(ref) {
  if (length(ref) > 0) {
    fieldnames <- lapply(ref, function(field) {
						   unlist(lapply(field, `[`, 1), recursive=F)})

    fields <- lapply(ref, function(field) {
      					   unlist(lapply(field, `[`, 2), recursive=F)})

    l <- lapply(1:length(fields), function(i) {
				  v <- c(fields[[i]])
      			  names(v) <- fieldnames[[i]]
      			  v})

    variables <- c("date","year","pmid", "doi", "vauthors", "title", "journal",
  				 "volume", "issue", "pages")

    lapply(1:length(l), function(i) {
			 naVars <- variables[!variables %in% names(l[[i]])]
  		     new <- rep(NA, length(naVars))
  		     names(new) <- naVars
  		     v2 <- c(l[[i]], new)
  		     v2[order(match(names(v2), variables))]
			 v2})
  }
}

remove.duplicates <- function(df) {
  df <- df[!duplicated(df$title), ]
  df
}


substrRight <- function(x, n){
  substr(x, nchar(x)-n+1, nchar(x))
}

get.df <- function(xmlfile, write=TRUE) {
  jr <- get.journals(xmlfile)
  f <- lapply(jr, get.fields)
  bool <- !unlist(lapply(jr, function(ref) length(unlist(ref))==0))
  f <- f[bool]
  
  dates <- get.datestamp(xmlfile=xmlfile)[bool]
  months <- get.monthstamp(xmlfile=xmlfile)[bool]
  ids <- get.id(xmlfile=xmlfile)[bool]
  revisionDate <- unlist(lapply(1:length(f), function(i) rep(dates[i], length(f[[i]]))))
  revisionMonth <- unlist(lapply(1:length(f), function(i) rep(months[i], length(f[[i]]))))
  revisionID <- unlist(lapply(1:length(f), function(i) rep(ids[i], length(f[[i]]))))
  
  variables <- c("date","year","pmid", "doi", "vauthors", "title",
				 "journal", "volume", "issue", "pages")
  df <- data.frame(revisionDate, revisionMonth, revisionID)

  lapply(variables, function(var) df[var] <<- unlist(lapply(f, function(ref) unlist(lapply(ref, function(field) field[var])))))

  df$title <- gsub("\\.", "", df$title)
  df <- remove.duplicates(df)
  df$date <- unlist(lapply(df$date, substrRight, n=4))
  df$year[is.na(df$year)] <- df$date[is.na(df$year)]
  df <- df[ , !(names(df) %in% c("date", "pages", "vauthors"))]
  if (write) write.csv(df, "CR_REF.csv")
  df
}
