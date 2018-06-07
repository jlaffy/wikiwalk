#!/usr/bin/env Rscript

all_data <- readRDS('out.rds')
content <- all_data$`*`
saveRDS(content, file='content.rds')
