#!/usr/bin/env Rscript
source("parser.R")

df <- readRDS("out.rds")[-14]
text <- readRDS("out.rds")[[14]] # NOT [14]

df.out <- main(df=df, text=text, pattern='hyperlink')
saveRDS(df.out, file="outpp.rds")
