#' Creates a quick visualization of the page layout
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
#' @return A ggplot object with the given visualization.
#' @examples
#' # quick
#' ## tibble with full lines
#' ggpage_quick(tinderbox)
#' ## vector with full lines
#' ggpage_quick(tinderbox %>% pull(text))
#' ## tibble with single words
#' ggpage_quick(tinderbox %>% unnest_tokens(text, text))
#' ## vector with single words
#' ggpage_quick(tinderbox %>% unnest_tokens(text, text) %>% pull(text))
#'
#' # nrow and ncol
#' ggpage_quick(tinderbox, nrow = 2)
#' ggpage_quick(tinderbox, ncol = 2)
ggpage_quick <- function(book, lpp = 25, character_height = 3,
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

  data_1 <- data %>%
    left_join(page_spacing, by = "page")

  data_1 %>%
    ggplot() +
    geom_rect(aes(xmin = x_space_right + x_page *
                    (max_line_length + x_space_pages),
                  xmax = x_space_left + x_page *
                    (max_line_length + x_space_pages),
                  ymin = - line * (character_height + vertical_space) -
                    y_page * (lpp * (character_height + vertical_space) +
                                y_space_pages),
                  ymax = - line * (character_height + vertical_space) -
                    character_height - y_page *
                    (lpp * (character_height + vertical_space) +
                       y_space_pages))) +
    coord_fixed(ratio = 1) +
    theme_void()
}
