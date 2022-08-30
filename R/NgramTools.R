#### Initialize ####


#' @import stringr
#' @import data.table
#' @importFrom dplyr %>% left_join
#' @importFrom bruceR %^% Print formatN
.onAttach = function(libname, pkgname) {
  ## Loaded Package
  pkgs = c("dplyr", "stringr", "data.table")

  suppressMessages({
    suppressWarnings({
      loaded = sapply(pkgs, require, character.only=TRUE)
    })
  })
}


#### Ngram RData ####


if(FALSE) {
  library(bruceR)
  set.wd("D:/Research/[Database]/GoogleNgram/")
  load("GoogleNgram-v2/total.v2.Rdata")
  load("GoogleNgram-v3/total.v3.Rdata")
  load("GoogleNgram-v2/ngram.v2.chi_TAG.Rdata")
  load("GoogleNgram-v3/ngram.v3.chi_TAG.Rdata")
  set.wd()
  set.wd("../")
  usethis::use_data(total.v2, overwrite=TRUE, compress="xz")
  usethis::use_data(total.v3, overwrite=TRUE, compress="xz")
  usethis::use_data(ngram.v2.chi_TAG, overwrite=TRUE, compress="xz")
  usethis::use_data(ngram.v3.chi_TAG, overwrite=TRUE, compress="xz")
}


#' Google Ngram datasets for direct use.
#'
#' @description
#' \code{data.table} of Google Books Ngram datasets, including
#' Google Books Version 2 (2012) and Version 3 (2020).
#'
#' @format
#' \describe{
#'   \item{\strong{1. Total counts (1-grams, all languages)}}{
#'     \itemize{
#'       \item \code{total.v2} - Google Books Version 2 (2012)
#'       \item \code{total.v3} - Google Books Version 3 (2020)
#'     }
#'   }
#'   \item{\strong{2. Google Chinese Books Ngram (1-grams, with part-of-speech tags)}}{
#'     \itemize{
#'       \item \code{ngram.v2.chi_TAG} - Chinese, Version 2
#'       \item \code{ngram.v3.chi_TAG} - Chinese, Version 3
#'     }
#'   }
#' }
#'
#' @source
#' \url{https://storage.googleapis.com/books/ngrams/books/datasetsv2.html}
#'
#' \url{https://storage.googleapis.com/books/ngrams/books/datasetsv3.html}
#'
#' @name GoogleNgramData
#' @aliases
#' total.v2 total.v3
#' ngram.v2.chi_TAG ngram.v3.chi_TAG
NULL


#### Ngram Functions ####


get_ngram_data = function(data, word) {
  i = which(data$ngram==word)
  di = data[[i, "data"]]
  # di = data[[ngram==word, "data"]]
  d = do.call(rbind, str_split(str_split(di, "\\t", simplify=TRUE), ","))
  d = as.data.table(d)
  names(d) = c("year", "match_count", "volume_count")
  for(name in names(d))
    d[[name]] = as.numeric(d[[name]])
  return(d)
}


ngram_keywords = function(data, pattern) {
  data.table(ngram=sort(data$ngram[str_detect(data$ngram, pattern)]))
}


#' Count word frequency from Google Ngram data.
#'
#' @param data A \code{data.table} like the demodata (see \link{GoogleNgramData}).
#' @param years A numeric vector of years to be included.
#' @param words [Option 1] Word strings (\code{NULL}; a single word; a vector of words).
#' @param pattern [Option 2] Pattern of regular expression (see \code{\link[stringr:str_subset]{str_subset}}).
#'
#' @return
#' A numeric vector of word counts for all specified years.
#'
#' @examples
#' ngram_count(ngram.v3.chi_TAG,
#'             years=1949:2019,
#'             pattern="^独特_|^独一无二_")
#'
#' years = 1949:2019
#' pattern = "^独特_|^独一无二_"
#' data = data.table(
#'   year = years,
#'   total.words = total.v3[year %in% years]$words.chi,
#'   freq = ngram_count(ngram.v3.chi_TAG, years, pattern=pattern)
#' )
#' data[, prop := freq / total.words]
#' data
#'
#' @export
ngram_count = function(data, years,
                       words=NULL, pattern=NULL) {
  ## Use either "words" (character vector) or "pattern" (regular expression)
  if(!is.null(pattern))
    words = ngram_keywords(data, pattern)$ngram
  if(length(words)==0) {
    counts = rep(0, length(years))
  } else {
    data.subset = data[ngram %in% words]
    data.counts = do.call(cbind, lapply(words, function(word) {
      left_join(data.table(year=years),
                get_ngram_data(data.subset, word),
                by="year")$match_count
    }))
    counts = rowSums(data.counts, na.rm=TRUE)
    counts.each.word = colSums(data.counts, na.rm=TRUE)
    words.info = paste0(words, " (", counts.each.word, ")")
    Print("<<blue Keywords matched:>>
          <<green {paste(words.info, collapse=', ')}>>
          <<white (total count = {formatN(sum(counts))})>>
          \n
         ")
  }
  names(counts) = paste0("|", years, "|")
  return(counts)
}
