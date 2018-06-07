addLine <- function(string, file='working.R', append=T) {
  readr::write_lines(string, path=file, append=append)
}
