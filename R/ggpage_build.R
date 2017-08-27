#' Creates a data frame for further analysis and plotting
#'
#' This function can be used in combination with \code{ggpage_plot} to get the
#' same result as \code{ggpage_quick}. However by splitting the data.frame
#' construction and plotting we are able to do intermediate analysis which
#' can be included in the visualization.
#'
#' @param book Character or data.frame. Can either have each element be a
#'   seperate line or having each element being seperate words.
#' @param lpp Numeric. Lines Per Page. numver of lines allocated for each page.
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
#' @return `tibble` containing the following columns:
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
#' # build and plot
#' ## tibble with full lines
#' ggpage_build(tinderbox)) %>%
#'   ggpage_plot()
#' ## vector with full lines
#' ggpage_build(book = tinderbox) %>%
#'   pull(text)) %>%
#'   ggpage_plot()
#' ## tibble with single words
#' ggpage_build(tinderbox) %>%
#'   unnest_tokens(text, text)) %>%
#'   ggpage_plot()
#' ## vector with single words
#' ggpage_build(tinderbox) %>%
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
ggpage_build <- function(book, lpp = 25, character_height = 3,
                         vertical_space = 1, x_space_pages = 10,
                         y_space_pages = 10, nrow = NULL, ncol = NULL,
                         bycol = TRUE) {

  if(!any(class(book) %in% c("character", "data.frame"))) {
    stop("Please supply character string or data.frame.")
  }

  # Makes strings to tibbles
  if (inherits(book, "character")) {
    book <- tibble(text = book)
  }

  # Makes single words to lines
  if (book %>% slice(1:25) %>% pull(text) %>% str_detect(" ") %>%
      mean() < 0.9) {
    book <- tibble(text = word_to_line(book))
  }

  # Data table with full lines needed here
  data <- book %>%
    mutate(index_line = row_number(),
           page = rep(1:ceiling(n() / lpp), length.out = n(), each = lpp)) %>%
    group_by(page) %>%
    mutate(line = row_number()) %>%
    ungroup() %>%
    unnest_tokens(output = word, input = text) %>%
    mutate(word_length = str_length(word)) %>%
    group_by(index_line) %>%
    mutate(first_word = lag(line, default = 0) != line,
           x_space_right = cumsum(word_length + 1) - 1,
           x_space_left = cumsum(lag(word_length + 1, default = 0))) %>%
    ungroup()

  # Longest line
  max_line_length <- book %>% pull(text) %>% str_length() %>% max()

  # Add page spacing
  num_pages <- data %>% pull(page) %>% n_distinct()
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
    page_spacing <- tibble(
      page = 1:num_pages,
      x_page = rep(1:n_row_x, length.out = num_pages, each = n_row_y),
      y_page = rep(1:n_row_y, length.out = num_pages)
    )
  } else {
    page_spacing <- tibble(
      page = 1:num_pages,
      x_page = rep(1:n_row_x, length.out = num_pages),
      y_page = rep(1:n_row_y, length.out = num_pages, each = n_row_x)
    )
  }

  data %>% left_join(page_spacing, by = "page") %>%
    mutate(xmin = x_space_right + x_page * (max_line_length + x_space_pages),
           xmax = x_space_left + x_page * (max_line_length + x_space_pages),
           ymin = - line * (character_height + vertical_space) - y_page *
             (lpp * (character_height + vertical_space) + y_space_pages),
           ymax = - line * (character_height + vertical_space) -
             character_height - y_page *
             (lpp * (character_height + vertical_space) + y_space_pages)) %>%
    select(-index_line, -word_length, -first_word, -x_space_right,
           -x_space_left, -x_page, -y_page) %>%
    select(word, everything())
}
