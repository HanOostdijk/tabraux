#' transpose above with octave when below certain note
#'
#' Useful for e.g. guitar where e (or d) is the lowest note
#'
#' @name cond_transpose
#' @param notes character string with notes
#' @param lowest  lowest note that will not be transposed
#' @return transposed piece of music
#' @export
#' @examples
#' \dontrun{
#'   cond_transpose("c, d, e, f,")
#' #  [1] "c d e, f,"
#'   cond_transpose("c, d, e, f,",lowest='d,')
#' #  [1] "c d, e, f,"
#' }

cond_transpose <- function (notes, lowest="e,") {
  # transpose notes below 'lowest' one octave upwards
  # music contains no flats

  mn <- stringr::str_split(as.character(notes), " ")[[1]]

  lu_tab <- create_lookup_table()
  f1 <- lu_tab$f1
  f2 <- lu_tab$f2
  me  <- match(lowest, lu_tab$f1)

  mn1tf <- stringr::str_count(mn, "[abcdefg]")  == 1

  # first do single notes
  mn1 <- mn[mn1tf]
  mn1 <- cond_transpose1(mn1, me, f1, f2)
  mn[mn1tf] <- mn1
  # then do chords
  mn2 <- mn[!mn1tf]
  mn2 <- purrr::map_chr (mn2, function(x) {
    mn2a <-
      unlist(stringr::str_extract_all(mn2, "[abcdefg][^abcdefg]{0,2}"))
    paste0(cond_transpose1(mn2a, me, f1, f2), collapse = '')
  })
  mn[!mn1tf] <- mn2

  paste0(mn,collapse = ' ')
}

cond_transpose1 <- function (mn, me, f1, f2) {
  mmn <- match(mn, f1)
  tt  <- seq(1, length(mn))
  mmn[is.na(mmn)] <- Inf
  mni <- mmn[mmn < me]
  tt  <- tt[mmn < me]
  mn[tt] <- f2[mni]
  mn
}

create_lookup_table <- function () {
  # create tables with single notes in order
  #  and with the upwards octave transposed notes
  f0 <- c("c", "d", "e", "f", "g", "a", "b")
  t0 <- expand.grid(f1 = c(" ", "#"), f0 = f0)
  f1 <-
    unlist(purrr::map2(t0$f0, t0$f1,  ~ stringr::str_squish(paste0(.x, .y))))
  f2 <- c(",,", ",", "", "'", "''")
  f3 <- c(",", "", "'", "''", "'''")
  t1  <- expand.grid(f1 = f1, f2 = f2)
  t1$f2 <- unlist(purrr::map2(t1$f1, t1$f2,  ~ paste0(.x, .y)))
  t2  <- expand.grid(f1 = f1, f3 = f3)
  t1$f3 <- unlist(purrr::map2(t2$f1, t2$f3,  ~ paste0(.x, .y)))
  data.frame(f1 = t1$f2, f2 = t1$f3)
}