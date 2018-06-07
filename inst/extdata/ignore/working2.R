#!/usr/bin/env Rscript

content <- readRDS("content.rds")
readr::write_lines(content[1684], path="content_1684_txt.R")
addLine <- function(string, file='working.R', append=T) {readr::write_lines(string, path=file, append=append)}
test <- content[1684]
pattern <- list(Category="(?<=\\[\\[Category:)(.*)(?=\\]\\])")
categories <- stringr::str_extract_all(test, pattern$category)
