#' Internal function for converting words to lines
#'
#' extents the str_wrap() function from the stringr pacakge to work with longer
#' strings.
#'
#' @param words Character. Having each element being seperate words.
#' @param wot_number Numeric. how many words to split whole string by.
#' @return Character. have each element be a seperate line.
#' @keywords internal
#' @export
word_to_line <- function(words, wot_number = 1000) {

  words %>%
    dplyr::mutate(split = rep(1:ceiling(n / wot_number),
                              by = wot_number,
                              length.out = NROW(words))) %>%
    split(.$split) %>%
    purrr::map(~ .x %>% dplyr::pull(.data$text) %>%
          stringr::str_c(collapse = " ") %>%
          stringr::str_wrap() %>%
          stringr::str_split("\n *")) %>%
    unlist()
}
