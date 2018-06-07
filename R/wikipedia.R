# #!/usr/bin/env Rscript
# 
# library(XML)
# library(methods)
# 
# cc <- xmlParse("cc.xml")
# 
# # Load the packages required to read XML files.
# library(XML)
# library(methods)
# 
# cc <- xmlParse("cc.xml")
# lcc <- xmlToList(cc)
# 
# # 3 objects in lcc:
# # siteinfo, page, attributes
# # select page object
# page <- lcc$page
# 
# info <- lapply(1:length(page), function(i) {
# 				 if (names(page[i]) == "revision") {
# 				   c(page[[i]]$timestamp, page[[i]]$text) }} )
# text <- sapply(1:length(page), function(i) {if (names(page[i]) == "revision") {page[[i]]$text }})
# text2 <- sapply(text, `[[`,1 )
# timestamps <- lapply(1:length(info), function(i) {
# 					   if (!is.null(info[[i]])) {
# 						 strsplit(page[[i]]$timestamp, "-")[[1]][1] }} )
# 
# refs.in.text.bool <- lapply(1:length(info), function(i) info[[i]]$text)
# 
# 
# 
# 
# 
# text <- page[[133]]$text
# 
# pos <- gregexpr('ref', text[[1]])
# comb <- embed(as.vector(pos[[1]]), 2)[, 2:1]
# 
# refs <- lapply(1:nrow(comb), function(i) {
# 				 substr(text[[1]],
# 						start=comb[i,][1],
# 						stop=comb[i,][2]) })
# 
# refs <- sapply(refs, function(ref) {
# 				 strsplit(gsub("[^[:alnum:] ]", " ", ref), " +") })
# 
# 
# bool <- sapply(refs, function(ref) {
# 				 "doi" %in% ref | "journal" %in% ref | "pmid" %in% ref })
# 
# realrefs <- refs[bool]
# 
# bool2 <- sapply(realrefs, function(listy) !is.na(as.numeric(listy))
# 
# nums <- sapply(1:length(realrefs), function(i) as.numeric(realrefs[[i]][bool2[[i]]]))
# 







# page <- lcc$page
#
# timestamps <- lapply(1:length(info), function(i) {
# 					   if (!is.null(info[[i]])) {
# 						 strsplit(page[[i]]$timestamp, "-")[[1]][1] }} )


# text <- sapply(1:length(page), function(i) {if (names(page[i]) == "revision") {page[[i]]$text }})
# text <- sapply(text, `[[`,1 )
# text <- text[!sapply(text, is.null)]



find.pos <- function(x, search) {

  # x is character string
  # sub is search substring
  print(class(x))
  print(x)
  pos <- regexpr(pattern=search, text=as.character(x))
  print(paste("POS",class(pos)))
  print(pos)

  if (pos[[1]][1] == -1) {return()}
  
  else {
	
	comb <- embed(as.matrix(unlist(pos[[1]])),2)[,2:1]
	
	return(list(pos=pos, comb=comb)) }

}


split.pos <- function(x, comb) {

  if (!is.matrix(comb) | !is.numeric(comb) | ncol(comb) != 2) {
	return("Error in input 'mat'. Numeric matrix required; ncol=2")}

  if (!is.character(x)) {
	return("Error in input 'x'. Character vector required")}

  else {
	lapply(1:nrow(comb), function(i) {
			 substr(x, start=comb[i,][1], stop=comb[i,][2]) })}

}


clean.char <- function(x) {

  # x is character string
  unlist(strsplit(gsub("[^[:alnum:] ]", " ", x), " +"))

}


check <- function(char, keep, any=TRUE) {
  # char is character vector to be checked

  if (any) { return(any(char %in% keep)) }
  if (!any) { return(all(char %in% keep)) }

}



keep <- function(span, vect) {

  if (check(char=span, vect)) {
	span}
}


funme2 <- function(char, search, keep, ...){

  comb <- find.pos(x=char, search=search)

  if (is.null(comb)) {return()}

  else {
	
	spans <- split.pos(char, comb$comb)
	
	clean.spans <- lapply(spans, clean.char)
	
	sapply(clean.spans, function(span) {
			 keep(span, vect=c("pmid", "doi", "date", "year"))})

  }

}



funme <- function(char, search, keep, ...){

  comb <- find.pos(x=char, search=search)

  if (is.null(comb)) {return()}

  else {

	spans <- split.pos(char, comb$comb)
	
	real <- unlist(lapply(spans, function(span) {
						 check(char=clean.char(span),
							   keep=c("pmid", "doi"), ...)}))

	funme2(spans[real], search="\\|", keep=c("doi", "pmid", "date", "year"))}

}



# text <- page[[133]]$text[[1]]
# real.spans <- funme(char=text, search="<ref>", keep=c("doi", "pmid"))

# span <- real.spans[[1]]
# funme2(char=span, search="\\|", keep=c("doi", "pmid", "date", "year"))



