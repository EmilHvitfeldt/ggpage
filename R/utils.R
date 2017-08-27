#' Internal function for converting words to lines
#' @param words Character. Having each element being seperate words.
#' @param wot_number Numeric. how many words to split whole string by.
#' @return Character. have each element be a seperate line.
#' @keywords internal
word_to_line <- function(words, wot_number = 1000) {
  words %>%
    mutate(split = rep(1:ceiling(n() / wot_number),
                       by = wot_number, length.out = n())) %>%
    split(.$split) %>%
    map(~ .x %>% pull(text) %>%
          str_c(collapse = " ") %>%
          str_wrap() %>%
          str_split("\n *")) %>%
    unlist()
}
