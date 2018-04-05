#' Creates a quick visualization of the page layout
#'
#' @param book Character or data.frame. Can either have each element be a
#'   seperate line or having each element being seperate words.
#' @param lpp Numeric. Lines Per Page. Number of lines allocated for each page.
#' @param character_height Numeric. Relative size of the height of each letter
#'   compared to its width.
#' @param vertical_space Numeric. Distance between each lines vertically.
#' @param x_space_pages Numeric. Distence between pages along the x-axis.
#' @param y_space_pages Numeric. Distence between pages along the y-axis.
#' @param nrow Numeric. Number of rows of pages, if omitted defaults to square
#'   layout.
#' @param ncol Numeric. Number of columns of pages, if omitted defaults to
#'   square layout.
#' @param bycol Logical.  If TRUE (the default) the matrix is filled by
#'   columns, otherwise the matrix is filled by rows.
#' @return A ggplot object with the given visualization.
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(tidytext)
#' library(ggpage)
#' # quick
#' ## data.frame with full lines
#' ggpage_quick(tinderbox)
#' ## vector with full lines
#' ggpage_quick(tinderbox %>%
#'                pull(text))
#' ## data.frame with single words
#' ggpage_quick(tinderbox %>%
#'                unnest_tokens(text, text))
#' ## vector with single words
#' ggpage_quick(tinderbox %>%
#'                unnest_tokens(text, text) %>%
#'                pull(text))
#'
#' # nrow and ncol
#' ggpage_quick(tinderbox, nrow = 2)
#' ggpage_quick(tinderbox, ncol = 2)
#' }
#' @export
ggpage_quick <- function(book, lpp = 25, character_height = 3,
                         vertical_space = 1, x_space_pages = 10,
                         y_space_pages = 10, nrow = NULL, ncol = NULL,
                         bycol = TRUE) {

  if(!any(class(book) %in% c("character", "data.frame"))) {
    stop("Please supply character string or data.frame.")
  }

  # Makes strings to data.frames
  if (inherits(book, "character")) {
    book <- data.frame(text = book)
  }

  # Makes single words to lines
  if (book %>%
      dplyr::slice(1:25) %>%
      dplyr::pull(.data$text) %>%
      stringr::str_detect(" ") %>%
      mean() < 0.9) {
    book <- data.frame(text = word_to_line(book))
  }

  # Data table with full lines needed here
  n <- NROW(book)

  data <- book %>%
    dplyr::mutate(index_line = dplyr::row_number(),
                  page = rep(1:ceiling(n / lpp),
                             length.out = n,
                             each = lpp)) %>%
    dplyr::group_by(.data$page) %>%
    dplyr::mutate(line = dplyr::row_number()) %>%
    dplyr::ungroup() %>%
    tidytext::unnest_tokens(output = "word", input = "text") %>%
    dplyr::mutate(word_length = stringr::str_length(.data$word)) %>%
    dplyr::group_by(.data$index_line) %>%
    dplyr::mutate(first_word = dplyr::lag(.data$line, default = 0) != .data$line,
                  x_space_right = cumsum(.data$word_length + 1) - 1,
                  x_space_left = cumsum(dplyr::lag(.data$word_length + 1,
                                                   default = 0))) %>%
    dplyr::ungroup()

  # Longest line
  max_line_length <- book %>%
    dplyr::pull(.data$text) %>%
    stringr::str_length() %>%
    max()

  # Add page spacing
  num_pages <- data %>%
    dplyr::pull(.data$page) %>%
    dplyr::n_distinct()

  if (!is.null(nrow) || !is.null(ncol)) {
    if (!is.null(ncol)) {
      n_row_y <- ncol
      n_row_x <- ceiling(num_pages / n_row_y)
    }
    if (!is.null(nrow)) {
      n_row_x <- nrow
      n_row_y <- ceiling(num_pages / n_row_x)
    }
  } else {
    n_row_x <- n_row_y <- ceiling(sqrt(num_pages))
  }

  if (bycol) {
  page_spacing <- data.frame(
    page = 1:num_pages,
    x_page = rep(1:n_row_x, length.out = num_pages, each = n_row_y),
    y_page = rep(1:n_row_y, length.out = num_pages)
    )
  } else {
    page_spacing <- data.frame(
      page = 1:num_pages,
      x_page = rep(1:n_row_x, length.out = num_pages),
      y_page = rep(1:n_row_y, length.out = num_pages, each = n_row_x)
    )
  }

  data_1 <- data %>%
    dplyr::left_join(page_spacing, by = "page")

  data_1 %>%
    ggplot2::ggplot() +
    ggplot2::geom_rect(ggplot2::aes(xmin = data_1$x_space_right + data_1$x_page *
                    (max_line_length + x_space_pages),
                  xmax = data_1$x_space_left + data_1$x_page *
                    (max_line_length + x_space_pages),
                  ymin = - data_1$line * (character_height + vertical_space) -
                    data_1$y_page * (lpp * (character_height + vertical_space) +
                                y_space_pages),
                  ymax = - data_1$line * (character_height + vertical_space) -
                    character_height - data_1$y_page *
                    (lpp * (character_height + vertical_space) +
                       y_space_pages))) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_void()
}
