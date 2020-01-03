
#' Expand notes
#'
#' A character string with incomplete note information for `tabr` can be expanded to the full information. See **Details** for the transformations that can be applied.
#'
#' @export
#' @name expand_notes
#' @param ns Character vector with notes for tabr
#' @param sh_fl Integer indicating the number of sharps (when positive) or flats (when negative). In the latter case the number of flats is `(-sh_fl)`. When `ns` contains more than one character string the value(s) in `sh_fl` are recycled when necessary
#' @param rmv_mi Boolean should measure indications (`|`) be removed
#' @return Character vector of the same length as `ns` with the expanded character strings
#' @section Details:
#' Two transformation are applied to the notes:
#' - when the octave number is not specified it is taken from the last one specified
#' - when a note without a sharp or flat is specified it will be sharpened or flattened
#' according to the number of sharps and flats indicated in `sh_fl`.
#' A note that should not be sharpened or flattened can be given a additional x:
#' e.g. with `sh_fl=1` the note 'fx3' will be noted as 'f3' and not 'f#3'
#' @examples
#' \dontrun{
#' expand_notes('a3 b c g f',sh_fl = 2)
#' expand_notes(c('a3 b c g f | a3 b c g f | a3 b c g f'),sh_fl = c(2,3),rmv_mi=T)
#' }

expand_notes <- function (ns,sh_fl=0,rmv_mi=T) {
  expand_notes1 <- function(ns,sh_fl=0) {
  if (rmv_mi == T) {
    ns = stringr::str_replace_all(ns,"\\|"," ")
  }
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
  num[ns %in% c('r','s','|')] = ''
  ns = paste0(ns,num)
  ns = stringr::str_replace_all(ns,"x","")
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

#' check_times
#'
#' For a character string with the timing information for one or more measures the total time is calculated in units of `steps`. See **Details** for the timing information that is recognized.
#'
#' @export
#' @name check_times
#' @param times Character string with `tabr` timing information for one or more measures. The separation of measures is done with the '|' character. Note that this is not recognized by `tabr`. Use the `times` element of the result of this function as timing information in `tabr`
#' @param steps Integer indicating the unit that the total time is reported in. E.g. 4 when the time is measured in quarter notes.
#' @return Named list with in the first element `times` the time information without the '|' character and in the second element `counts` the number of units in each measure.
#' @section Details:
#' The following information is recognized:
#' - an integer number like 1, 2, 4, 8, 16, 32
#' - such a number with one or two dots
#' - such a number with a replication factor like '*2', '*3' etc
#' @examples
#' \dontrun{
#' x= check_times(" 2. | 8*3 16*4 8|8*6| 8*3 16*4 8",8)
#' identical(x$times,"2. 8*3 16*4 8 8*6 8*3 16*4 8")
#' # [1] TRUE
#' identical(x$counts,rep(6,4))
#' # [1] TRUE
#' }

check_times  <- function(times, steps = 8) {
  check_times1 <- function(times1, steps) {
    t1 =  strsplit(stringr::str_squish(times1), ' ')[[1]]
    t1p = stringr::str_extract(t1, '^\\d+')
    t1s = stringr::str_replace(t1, '^\\d+', '')
    t  = steps / as.numeric(t1p)
    ia = stringr::str_detect(t1s, '^\\*')
    t2s = stringr::str_replace(t1s, '^\\*', '')
    t[ia] = t[ia] * as.numeric(t2s[ia])
    ib = stringr::str_detect(t2s, '^\\.')
    t[ib] = t[ib] * 1.5
    t3s = stringr::str_replace(t2s, '^\\.', '')
    ib = stringr::str_detect(t3s, '^\\.')
    t[ib] = t[ib] * 1.5
    sum(t)
  }
  times1 = strsplit(times, '\\|')[[1]]
  times2 = stringr::str_replace_all(times, '\\|', ' ')
  list(times=stringr::str_squish(times2),
       counts=
         purrr::map_dbl(times1, ~check_times1(., steps))
  )
}

#' lilypond_version
#'
#' Retrieve the version number of Lilypond that is in use.
#'
#' @export
#' @name lilypond_version
#' @return Character string with the full version number (e.g. "2.19.83")
#' @examples
#' \dontrun{
#' lilypond_version()
#' [1] "2.19.83"
#' }

lilypond_version <- function() {
  v = system("lilypond.exe --version",intern=T)
  stringr::str_extract(v[1] ,"(?<=GNU LilyPond )(.*)$")
}


#' Edit a phrase
#'
#' The `edit_phrase` is a wrapper around the R base functions `sub` and `gsub` to change the `phrase` in a 'musical phrase' or a 'track'
#'
#' @export
#' @name edit_phrase
#' @param obj Object to change. Only objects of class `phrase` or `track` are supported
#' @param pattern Character string to be used as pattern in `sub` and `gsub` (depending on parameter `all`)
#' @param replacement Character string to be used as replacement in `sub` and `gsub` (depending on parameter `all`)
#' @param all Boolean indicating if all patterns found should be replaced (when TRUE) or only the first (when FALSE)
#' @return an object of the same type as `obj` with possibly (i.e. when the pattern is found) a replaced 'phrase'
#' @section Details:
#' See the documentation for the use of [sub] and [gsub].
#' @examples
#' \dontrun{
#' #include a test function
#' }
edit_phrase <- function (obj, pattern, replacement, all = T) {
  if ('phrase' %in% class(obj)) {
    ot = 'p'
    np = obj
  }
  else if ('track' %in% class(obj)) {
    ot = 't'
    np = obj[['phrase']]
  } else
    stop('edit_phrase: invalid type for obj')
  if (all == T)
    np2 = gsub(pattern, replacement, np)
  else
    np2 = sub(pattern, replacement, np)
  if (ot == 'p') {
    class(np2) <- class(obj)
    obj = np2
  }
  else  {
    np2 = list(np2)
    class(np2[[1]]) <- class(np[[1]])
    obj[['phrase']] = np2
  }
  obj
}
