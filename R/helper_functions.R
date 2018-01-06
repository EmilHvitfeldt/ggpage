#' Repeating of indexes
#'
#' @param x Numerical, vector.
#' @return Numerical.
#' @examples
#' break_help(c(1, 2, 3))
#' break_help(c(6, 8, 23, 50))
#' @export
break_help <- function(x) {
  purrr::map2(x, 1:length(x), ~ rep(.y, .x)) %>% unlist()
}
