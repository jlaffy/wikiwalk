#!/usr/bin/env Rscript

content <- readRDS("content.rds")
readr::write_lines(content[1684], path="content_1684_txt.R")
addLine <- function(string, file='working.R', append=T) {readr::write_lines(string, path=file, append=append)}
test <- content[1684]

patterns <- c()
patterns <- c(patterns, category="(?<=\\[\\[Category:)(.*)(?=\\]\\])")
categories <- stringr::str_extract_all(test, patterns['category'])

patterns <- c(patterns, hyperlink="(?<=\\[\\[)(.[^:]*?)(?=\\]\\])")
hyperlinks <- stringr::str_extract_all(test, patterns['hyperlink'])

patterns <- c(patterns, citation="(?<=\\{\\{[C|c]ite )(.*?)(?=\\}\\})")
patterns <- c(patterns, cite="(?<=\\{\\{[C|c]ite )(.*?)(?=\\}\\})")
cites <- stringr::str_extract_all(test, patterns['cite'])

patterns <- c(patterns, journal="(?<=\\{\\{[C|c]ite journal)(.*?)(?=\\}\\})")
journals <- stringr::str_extract_all(test, patterns['journal'])

patterns <- c(patterns, web="(?<=\\{\\{[C|c]ite web)(.*?)(?=\\}\\})")
web <- stringr::str_extract_all(test, patterns['web'])

patterns <- c(patterns, book="(?<=\\{\\{[C|c]ite book)(.*?)(?=\\}\\})")
books <- stringr::str_extract_all(test, patterns['book'])

patterns <- c(patterns, news="(?<=\\{\\{[C|c]ite news)(.*?)(?=\\}\\})")
news <- stringr::str_extract_all(test, patterns['news'])

results <- sapply(list(categories, hyperlinks, cites, journals, web, books, news), unlist, simplify=F, USE.NAMES=T)
saveRDS(patterns, file="patterns.rds")

