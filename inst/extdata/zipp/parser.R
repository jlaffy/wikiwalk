extract_all <- function(text, pattern) {

  if (!is.character(text)) {
	stop('<extract_all> requires character vector')
  }

  parsed <- sapply(text, stringr::str_extract_all, pattern)
  sapply(parsed, stringr::str_trim, simplify=F)
}


hashTable <- function(Pattern, file="patterns.rds") {

  readRDS(file)[Pattern]
}


expandRows <- function(df, by) {

  if (is.null(row.names(df))) {
    stop("<expandRows> requires named dataframe rows")
  }

  df[rep(row.names(df), by), ]
}


main <- function(df=NULL,
                 text=NULL,
                 textFile="out.rds",
                 textColumn=14,
                 pattern="hyperlink",
                 hashFile="patterns.rds",
                 return.text=T) {

  if (is.null(text)) {
    text <- readRDS(textFile)[[textColumn]]
  }

  if (is.null(df)) {
    df <- readRDS(textFile)[-textColumn]
  }

  parsed <- extract_all(text=text, pattern=hashTable(pattern))
  df.expanded <- expandRows(df=df, by=lengths(parsed, use.names=F))

  df.expanded[pattern] <- unlist(parsed)

  df.expanded
}


# first_instance <- function(df, by='timestamp', filter='hyperlink') {
# 
#   df.ord <- df[order(df, df[by])]
# 
#   df.ord[!duplicated(df.ord[filter]), ]
# }
