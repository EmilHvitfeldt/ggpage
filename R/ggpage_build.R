#' Creates a data frame for further analysis and plotting
#'
#' This function can be used in combination with \code{ggpage_plot} to get the
#' same result as \code{ggpage_quick}. However by splitting the data.frame
#' construction and plotting we are able to do intermediate analysis which
#' can be included in the visualization.
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
#' @param bycol Logical. If TRUE (the default) the matrix is filled by
#'   columns, otherwise the matrix is filled by rows.
#' @param wtl logical. If TRUE will convert single word vector into a vector
#'   with full lines. (defaults to FALSE).
#' @param para.fun Function that generates random numbers to determine number
#'  of word in each paragraph.
#' @param ... Extra arguments.
#' @return `data.frame` containing the following columns:
#'
#'   * `word`: Character. The words of the text.
#'   * `page`: Integer. Page number.
#'   * `line`: Integer. Line number within the page.
#'   * `xmin`: Numeric. Border of rectangle, used by \code{ggpage_plot} do not
#'     alter.
#'   * `xmax`: Numeric. Border of rectangle, used by \code{ggpage_plot} do not
#'     alter.
#'   * `ymin`: Numeric. Border of rectangle, used by \code{ggpage_plot} do not
#'     alter.
#'   * `ymax`: Numeric. Border of rectangle, used by \code{ggpage_plot} do not
#'     alter.
#' @examples
#' \dontrun{
#' library(tidyverse)
#' library(tidytext)
#' library(ggpage)
#' # build and plot
#' ## data.frame with full lines
#' ggpage_build(tinderbox) %>%
#'   ggpage_plot()
#' ## vector with full lines
#' ggpage_build(book = tinderbox %>%
#'   pull(text)) %>%
#'   ggpage_plot()
#' ## data.frame with single words
#' ggpage_build(tinderbox) %>%
#'   unnest_tokens(text, word) %>%
#'   ggpage_plot()
#' ## vector with single words
#' ggpage_build(tinderbox %>%
#'   unnest_tokens(text, text) %>%
#'   pull(text)) %>%
#'   ggpage_plot()
#'
#' # nrow and ncol
#' ggpage_build(tinderbox, nrow = 2) %>%
#'   ggpage_plot()
#' ggpage_build(tinderbox, ncol = 2) %>%
#'   ggpage_plot()
#'
#' # Include analysis within
#' ggpage_build(tinderbox) %>%
#'   mutate(word_length = str_length(word)) %>%
#'   ggpage_plot(aes(fill = word_length))
#' }
#' @export
ggpage_build <- function(book, lpp = 25, character_height = 3,
                         vertical_space = 1, x_space_pages = 10,
                         y_space_pages = 10, nrow = NULL, ncol = NULL,
                         bycol = TRUE, wtl = FALSE,
                         para.fun = NULL, ...) {

  if(!any(class(book) %in% c("character", "data.frame"))) {
    stop("Please supply character string or data.frame.")
  }

  # Makes strings to data.frames
  if (inherits(book, "character")) {
    book <- data.frame(text = book)
  }

  # Makes single words to lines
  single_word_check <- book %>%
    dplyr::slice(1:25) %>%
    dplyr::pull(.data$text) %>%
    trimws("both") %>%
    stringr::str_detect(" ") %>%
    mean() < 0.9

  if(!is.logical(wtl)) stop("wtl must be logical.")

  if (any(single_word_check, wtl)) {

    if (is.null(para.fun)) {
      book <- data.frame(text = word_to_line(book))
    } else {
      if(!is.function(para.fun)) stop("wtl must be a function")

      book <- book %>%
        dplyr::mutate(paragraph_id = para_index(NROW(book), para.fun, ...) %>%
                                       break_help())

      book <- purrr::map_df(book %>% dplyr::pull(.data$paragraph_id) %>% unique(),
                     ~ book %>%
                         dplyr::filter(paragraph_id == .x) %>%
                         word_to_line() %>%
                         data.frame(text = .))

    }
  }

  # Data table with full lines needed here
  data <- book %>%
    dplyr::mutate(index_line = 1:NROW(book),
                  page = rep(1:ceiling(NROW(book) / lpp),
                             length.out = NROW(book),
                             each = lpp)) %>%
    page_liner() %>%
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

  data %>% dplyr::left_join(page_spacing, by = "page") %>%
    dplyr::mutate(
      xmin = .data$x_space_right + .data$x_page * (max_line_length + x_space_pages),
      xmax = .data$x_space_left + .data$x_page * (max_line_length + x_space_pages),
      ymin = - .data$line * (character_height + vertical_space) - .data$y_page *
             (lpp * (character_height + vertical_space) + y_space_pages),
      ymax = - .data$line * (character_height + vertical_space) -
             character_height - .data$y_page *
             (lpp * (character_height + vertical_space) + y_space_pages)) %>%
    dplyr::select(-.data$index_line, -.data$word_length, -.data$first_word,
                  -.data$x_space_right, -.data$x_space_left, -.data$x_page,
                  -.data$y_page) %>%
    dplyr::select(.data$word, dplyr::everything())
}
