
#' Concatenate with extra care
#'
#' Simple expansion of [`tabr::pc`]: before concatenating the parts are handled by [`stringr::str_squish`]
#'
#' @export
#' @name pc2
#' @param ... Character strings with e.g. notes and times for tabr
#' @examples
#' \dontrun{
#' pc2('a3 b c g  f ','  a3 b3 c4 g f')
#' }

pc2 = function (...) {
  f1 <- function(x1) {
    x2 = stringr::str_squish(x1)
    if ('character' %in% class(x1) && nchar(x1)>0 && nchar(x2) == 0) x2 = " "
    x2
  }
  x = list(...)
  x = purrr::map(x,f1)
  do.call(tabr::pc,x)
}

#' Expand notes
#'
#' A character string with incomplete note information for can be expanded to the full information. See
#' **Details** for the transformations that can be applied.
#'
#' @export
#' @name expand_notes
#' @param ns Character vector with notes for tabr
#' @param sh_fl Integer indicating the number of sharps (when positive) or flats (when negative). In the latter case the number of flats in `(-sh_fl)`. When `ns` contains more than one character string the value(s) in `sh_fl` are recycled when necessary
#'
#' @section Details:
#' Two transformation are applied to the notes:
#' - when the octave number is not specified it is taken from the last one specified
#' - when a note without a sharp or flat is specified it will be sharpened or flattened according to the number of sharps and flats indicated in `sh_fl`. A note that should not be sharpened or flattened can be given a additional x: e.g. with `sh_fl=1` the note 'fx3' will be noted as 'f3' and not 'f#3'
#' @examples
#' \dontrun{
#' expand_notes('a3 b c g f',sh_fl = 2)
#' expand_notes(c('a3 b c g f','a3 b c g f','a3 b c g f'),sh_fl = c(2,3))
#' }

expand_notes <- function (ns,sh_fl=0) {
  expand_notes1 <- function(ns,sh_fl=0) {
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
  }
  num[ns %in% 'r'] = ''
  ns = paste0(ns,num)
  ns = stringr::str_replace(ns,"x","")
  paste(ns,collapse = ' ')
  }
  if (length(sh_fl) > length(ns))
    stop("expand_notes: more sharp_flat specifications than note strings")
  ea = expand_args(ns,sh_fl)
  purrr::map2_chr(ea[[1]],ea[[2]],expand_notes1)
}

expand_args <- function(...) {
  # recycles arguments
  # https://stackoverflow.com/questions/9335099/implem
  #    entation-of-standard-recycling-rules baptiste
  dots <- list(...)
  max_length <- max(sapply(dots, length))
  lapply(dots, rep, length.out = max_length)
}
