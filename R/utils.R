#' Internal function for converting words to lines
#'
#' extents the str_wrap() function from the stringr pacakge to work with longer
#' strings.
#'
#' @param words data.frame. Where each row is a seperate word words with the
#'   column name text.
#' @param wot_number Numeric. how many words to split whole string by.
#' @return Character. have each element be a seperate line.
#' @export
word_to_line <- function(words, wot_number = 1000) {

  words %>%
    dplyr::mutate(split = rep(1:ceiling(NROW(words) / wot_number),
                              by = wot_number,
                              length.out = NROW(words))) %>%
    split(.$split) %>%
    purrr::map(~ .x %>% dplyr::pull(.data$text) %>%
          stringr::str_c(collapse = " ") %>%
          stringr::str_wrap() %>%
          stringr::str_split("\n *")) %>%
    unlist()
}

#' paragraph split
#'
#' Converts a word vector into a line vector with variable paragraph lengths.
#'
#' FUN most be a function that takes in a number n and returns a vector of
#' natural numbers.
#'
#' @param n Numeric. Numbers of words.
#' @param FUN Numeric. how many words to split whole string by.
#' @param ... Extra arguments.
#' @return Numeric. paragraph indicator.
#' @export
para_index <- function(n, FUN, ...) {

  numbers <- FUN(n, ...)

  if(any(numbers < 0)) stop("FUN must return non-negative numbers.")

  index <- sum(cumsum(numbers) < n) + 1
  out <- numbers[seq_len(index)]

  out[index] <- out[index] - (sum(out) - n)
  out
}

#' Repeating of indexes
#'
#' @param x Numerical, vector.
#' @return Numerical.
#' @examples
#' break_help(c(1, 2, 3))
#' break_help(c(6, 8, 23, 50))
#' @export
break_help <- function(x) {
  unlist(
    purrr::map2(x, 1:length(x), ~ rep(.y, .x))
  )
}

#' Identify the egdes of the paper of each page
#'
#' @param data data.frame created by ggpage_build.
#' @return data.frame,
#' @examples
#' paper_shape(ggpage_build(tinderbox))
#' @export
paper_shape <- function(data) {
  dplyr::group_by(data, .data$page) %>%
    dplyr::summarise(xmin = max(.data$xmin),
                     xmax = min(.data$xmax),
                     ymin = max(.data$ymin),
                     ymax = min(.data$ymax))
}

#' Add line number within pages
#'
#' @param data data.frame
#' @return data.frame
#' @export
page_liner <- function(data) {
  line <- data %>%
    dplyr::group_by(.data$page) %>%
    dplyr::tally() %>%
    dplyr::pull(.data$n) %>%
    purrr::map(~ seq_len(.x)) %>%
    unlist()

  data %>%
    dplyr::mutate(line)
}

#' Adjust lines
#'
#' @param line data.frame
#' @param max_length numerical. number of letters allowed on a line.
#' @param type Type of line alignment. Must be one of "left", "right" or "both".
#' @return data.frame
#' @export
line_align <- function(line, max_length, type) {

  line_length <- abs(max(line$xmin) - min(line$xmax))
  n_words <- NROW(line)

  adjust <- 0

  if(n_words > 1) {
    if(type == "both") {
      adjust <- c(0, (max_length - line_length) / (n_words - 1) * seq_len(n_words - 1))
    }
  }
    if(type == "right") {
      adjust <- max_length - line_length
    }
    if(type == "left") {
      adjust <- 0
    }
    line$xmax <- line$xmax + adjust
    line$xmin <- line$xmin + adjust

  line
}
