#' converts paragraph tokens into line tokens
#'
#' extents the str_wrap() function from the stringr pacakge to work with longer
#' strings.
#'
#' @param data data.frame. With one paragraph per row.
#' @param input column that gets split as string or symbol.
#' @return data.frame.
#' @export
nest_paragraphs <- function(data, input) {
  quo_input <- rlang::quo_name(rlang::enquo(input))

  data[[quo_input]] %>%
    stringr::str_wrap() %>%
    stringr::str_split("\n") %>%
    unlist() %>%
    data.frame(text = ., stringsAsFactors = FALSE)
}
