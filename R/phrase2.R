#' phrase with possibility of anchor points
#'
#' The function `phrase2` and its alias `p2` extend the function [tabr::phrase][tabr::phrase()] (and its alias `p`) with the possibility to insert an anchorpoint of the form `'^'` or `'^n'` with `'n'` an integer. In this way it is possible When editing/copying a musical score to insert an easily recognisable point that later can be changed in e.g. a bar end.
#'
#' @name phrase2
#' @param notes noteworthy and note info strings. When info = NULL, it is assumed that notes refers to a music object or string formatted as such.
#' @param info noteworthy and note info strings. When info = NULL, it is assumed that notes refers to a music object or string formatted as such.
#' @param string space-delimited character string or vector (or integer vector if simple string numbers). This is an optional argument that specifies which instrument strings to play for each specific timestep. Otherwise NULL.
#' @param bar character or NULL (default). Terminates the phrase with a bar or bar check. See Details. Also see the LilyPond help documentation on bar notation for all the valid options.
#' @return NULL
#' @export
#' @section Details:
#' See for details the documentation of [tabr::phrase][tabr::phrase()]. \cr If an anchorpoint is specified in `notes` a corresponding `info` element should be present, but the value is ignored. In the examples 3 notes and 1 anchorpoint are specified in `notes` and therefore the `info` parameter must describe 4 elements.\cr \cr A PR to include this functionality directly in the package  [tabr][tabr::tabr] was rejected because it did not fit in the overall setup of the package.\cr Because this function is dependent on the internals of [tabr][tabr::tabr] it is sensitive to changes in that package.
#' @examples
#' \dontrun{
#' notes <- "e3 ^ f g"
#' info  <- "8*4"
#' phrase2(notes,info,bar= ":|.")
#'  # <Musical phrase>
#'  # <e>8 ^ <f>8 <g>8 \bar ":|."
#'
#' notes <- "e3 ^2 f g"
#' info  <- "8 1 8 8"
#' phrase2(notes,info,bar= ":|.")
#'  # <Musical phrase>
#'  # <e>8 ^2 <f>8 <g>8 \bar ":|."
#' }

phrase2 <- function(notes, info = NULL, string = NULL, bar = NULL){
  is_note_3 <- tabr::is_note
  phrase_3 <- tabr::phrase
  .phrase_3 <- tabr:::.phrase
  on.exit(assignInNamespace("phrase", phrase_3, ns = "tabr"),add=T)
  on.exit(assignInNamespace(".phrase", .phrase_3, ns = "tabr"),add=T)
  on.exit(assignInNamespace("is_note", is_note_3, ns = "tabr"),add=T)
  is_note_2 <- function(x, na.rm = FALSE){
    if(na.rm){
      x <- x[!is.na(x)]
      if(!is.character(x)) x <- as.character(x)
    }
    x <- .uncollapse(x)
    y1 <- grepl("[a-grs^]", x) & !grepl("[h-qt-zA-Z]", x)
    y2 <- gsub("\\d|,|'|_|#|~|\\*", "", x)
    y1 & nchar(y2) == 1 & y2 == substr(x, 1, 1) & !grepl("(r|s)\\d", x)
  }
  environment(is_note_2) <- asNamespace("tabr")
  phrase_2 <- function(notes, info = NULL, string = NULL, bar = NULL){
    if(is.null(info)){
      if(!inherits(notes, "music")) notes <- as_music(notes)
      if(is.null(string)) string <- music_strings(notes)
      info <- .uncollapse(music_info(notes))
      notes <- music_notes(notes)
    } else {
      notes <- as_noteworthy(notes)
      n <- length(notes)
      if(is.character(info)) info <- as_noteinfo(info)
      info <- .uncollapse(info)
      if(length(info) == 1) info <- rep(info, n)
      if(length(string) == 1 && is.na(string)) string <- NULL
      if(!is.null(string)){
        string <- .uncollapse(string)
        if(length(string) == 1) string <- rep(string, n)
        if(length(string) != length(notes))
          stop(
            paste("`string` must have the same number of timesteps as `notes`,",
                  "or a single value to repeat, or be NULL."),
            call. = FALSE
          )
        string <- .music_infer_strings(notes, .uncollapse(string))
      }
    }
    notes <- .uncollapse(notes)
    idx1 <- grepl("\\d", notes) # 'notes' that have octave numbers ?
    idx2 <- !grepl("\\^", notes) # 'notes' that are no hooks
    idx  <- idx1 & idx2 # no-hook notes with octave numbers
    notes[idx] <- .octave_to_tick(notes[idx]) # only for non-hook notes
    if(length(notes) != length(info))
      stop(paste("`info` must have the same number of timesteps as `notes`",
                 "or a single value to repeat."), call. = FALSE)

    dur <- as.character(info_duration(info))
    trp <- gsub("t", "", gsub("^\\d+(\\.+|)$", "", dur))
    rl <- rle(trp)

    if(is.logical(bar) && !bar) bar <- NULL

    x <- purrr::map(seq_along(rl$values), ~{
      idx2 <- sum(rl$lengths[1:.x])
      idx1 <- idx2 - rl$lengths[.x] + 1
      idx <- idx1:idx2
      x <- notes[idx]
      y <- info[idx]
      z <- string[idx]
      v <- as.integer(rl$values[.x])
      p0 <- .phrase(x, y, z)
      if(!is.na(v)){
        p0 <- paste(p0, collapse = " ")
        if(!is.null(bar)){
          p0 <- if(is.logical(bar)) p0 <- paste(p0, "|") else
            paste0(p0, " \\bar \"", bar, "\"")
        }
        p0 <- gsub("\\| \\|", "\\|", p0)
        p0 <- gsub(">t", ">", triplet(as_phrase(p0), v))
      }
      p0
    })

    idx <- which(rl$values == "")
    if(length(idx)){
      x[idx] <- purrr::map(x[idx], ~{
        x <- paste(.x, collapse = " ")
        if(!is.null(bar)){
          x <- if(is.logical(bar)) paste(x, "|") else
            paste0(x, " \\bar \"", bar, "\"")
        }
        x <- gsub(" \\| \\|", " \\|", x)
        as_phrase(x)
      })
    }
    do.call(c, x)
  }
  environment(phrase_2) <- asNamespace("tabr")
  .phrase_2 <- function(notes, info, string){
    notes <- purrr::map_chr(notes, .tabsub)
    info <- purrr::map_chr(info, .tabsub)
    bend <- which(purrr::map_int(info, ~{
      length(grep("[^-]\\^", strsplit(.x, ";")[[1]][1]))
    }) == 1)
    dead <- which(purrr::map_int(info, ~{
      length(grep("xDEADNOTEx", strsplit(.x, ";")[[1]][1]))
    }) == 1)
    if(length(bend)) info[bend] <- gsub(";\\^", ";", info[bend])
    if(length(dead)) info[dead] <- gsub("xDEADNOTEx", "", info[dead])
    info <- gsub(";", "", info)
    .bend <- "\\bendAfter #+6"
    s <- !is.null(string)
    if(s) string <- .strsub(string)
    notes2 <- notes # make copy
    idx <- !grepl("\\^", notes2) # locate non-hook notes
    notes <- notes[idx]   # select non-hook notes
    info <- info[idx]     # corresponding info
    string <- string[idx] # corresponding strings
    # no corresponding info and strings information for hooks !
    notes <- purrr::map_chr(
      seq_along(notes),
      ~paste0("<", paste0(
        .split_chord(notes[.x], abb = TRUE),
        if(s && notes[.x] != "r" && notes[.x] != "s")
          paste0("\\", .split_chord(string[.x], TRUE)), collapse = " "), ">"))
    notes <- gsub("<s>", "s", gsub("<r>", "r", notes))
    x <- paste0(notes, info)
    notes2[idx] = x      # replace non-hook note-info
    if(length(bend))
      notes2[bend] <- gsub("\\^\\\\bend", "\\\\bend", paste0(notes2[bend], .bend))
    if(length(dead)) notes2[dead] <- paste("\\deadNote", notes2[dead])
    gsub("\\\\x", "", notes2)
  }
  environment(.phrase_2) <- asNamespace("tabr")

  assignInNamespace("phrase", phrase_2, ns = "tabr")
  assignInNamespace(".phrase", .phrase_2, ns = "tabr")
  assignInNamespace("is_note", is_note_2, ns = "tabr")

  notes = trimws(gsub("\\s\\s+", " ",notes))
  if (!is.null(info)) info =  trimws(gsub("\\s\\s+", " ",info))
  phrase_2(notes, info = info, string = string, bar = bar)
}


#' @export
#'
#' @name p2
#' @rdname phrase2
#'
p2 <- phrase2
