
library(tabr)

#' Concatenate with extra care
#'
#' Simple expansion of [tabr::pc()]: before concatenating the parts are handled by [stringr::str_squish()]
#'
#' @export
#' @name pc2
#' @param ... Character strings with notes for tabr

pc2 = function (...) {
  x = list(...)
  x = purrr::map(x,stringr::str_squish)
  do.call(tabr::pc,x)
}

#' Expand notes
#'
#' A character string with incomplete note information for can be expanded to the full information. See
#' **Details** for the transformations that can be applied.
#'
#' @export
#' @name expand_notes
#' @param ns Character string with notes for tabr
#' @param sh_fl Integer indicating the number of sharps (when positive) or flats (when negative). In the latter case the number of flats in `(-sh_fl)`
#'
#' @section Details:
#' Two transformation are applied to the notes:
#' - when the octave number is not specified it is taken from the last one specified
#' - when a note without a sharp or flat is specified it will be sharpened or flattened according to the number of sharps and flats indicated in `sh_fl`. A note that should not be sharpened or flattened can be given a additional x: e.g. with `sh_fl=1` the note 'fx3' will be noted as 'f3' and not 'f#3'

expand_notes <- function (ns,sh_fl=0) {
  ns = stringr::str_squish(ns)
  ns = strsplit(ns,' ')[[1]]
  num = stringr::str_extract(ns,"\\d+$")
  ns = stringr::str_replace(ns,"\\d+$","")
  prev = "3"
  notes = c("f","c","g","d","a","e","b")
  for (i in seq_along(num) ) {
    if (!is.na(num[i])) {
      prev = num[i]
    } else {
      num[i] =prev
    }
  }
  if (sh_fl != 0){
    if (sh_fl < 0) {
      notes=rev(notes)[1:(-sh_fl)]
      teken = '_'
    } else {
      notes=notes[1:sh_fl]
      teken = '#'
    }
    for (i in seq_along(ns) ) {
      if (ns[i] %in% letters[1:7]) {
        if (ns[i] %in% notes) {
          ns[i]=paste0(ns[i],teken)
        }
      }
    }
    ns = paste0(ns,num)
  }
  ns = stringr::str_replace(ns,"x","")
  paste(ns,collapse = ' ')
}
