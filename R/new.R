#!/usr/bin/env Rscript

removeNewLines <- function(x) {
  gsub("[\r\n]", "", x) }

# remove new line characters from text
noNewLines <- lapply(text, removeNewLines)


pattern <- "<ref(.*?)ref>"
# retrieve all substrings flanked by pattern (above)
matches <- sapply(noNewLines, function(t) str_match_all(t, pattern))
# retrieve second column only from nested result matrices
matches <- lapply(matches, function(m) m[,2])
# unecessary nested listed structure removed
matches <- lapply(matches, unlist, recursive=F)


is_or_not <- function(vect, bool) {
  print(class(bool))
  list(true=vect[unlist(bool)], false=vect[!unlist(bool)])
}


bools <- lapply(matches, function(match)
				lapply(match, str_detect,  pattern="doi|pmid"))

lapply(1:length(matches), function(i) is_or_not(vect=matches[[i]], bool=bools[[i]])



not_true <- lapply(matches, function(match) {
				 is_or_not(vect=match,
						   bool=lapply(match,
									   str_detect,
									   pattern="doi|pmid")$false)})


# pattern <- "<ref>(.*?)<ref>"
# matches <- sapply(noNewLines, function(t) regmatches(t,regexec(pattern, t)))

