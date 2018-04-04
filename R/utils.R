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


