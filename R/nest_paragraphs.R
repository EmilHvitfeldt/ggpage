#' converts paragraph tokens into line tokens
#'
#' extents the str_wrap() function from the stringr package to work with longer
#' strings.
#'
#' @param data data.frame. With one paragraph per row.
#' @param input column that gets split as string or symbol.
#' @param ... Extra arguments passed to str_wrap.
#' @return data.frame.
#' @export
nest_paragraphs <- function(data, input, ...) {
  quo_input <- rlang::quo_name(rlang::enquo(input))

  sections <- data[[quo_input]] %>%
    stringr::str_wrap(...) %>%
    stringr::str_split("\n")

  purrr::map_df(seq_len(nrow(data)),
                ~ data.frame(text = sections[[.x]], stringsAsFactors = FALSE) %>%
                  bind_cols(
                    bind_rows(
                      replicate(
                        length(sections[[.x]]),
                        dplyr::select(data, -which(names(data) == quo_input))[.x, ],
                        simplify = FALSE)
                      )
                    )
                )
}


