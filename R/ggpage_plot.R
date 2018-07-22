#' Creates a visualization from the ggpage_build output
#'
#' @param data data.frame. Expects output from \code{ggpage_build} with
#'   optional intermediate analysis.
#' @param mapping Default list of aesthetic mappings to use for plot to be
#'   handed to internal \code{ggplot} call.
#' @param paper.show Shows the paper underneath the text.
#' @param paper.color Color of the pages. Needs to be of length 1 or the same
#'   as the number of pages.
#' @param paper.alpha Alpha of the pages. Needs to be of length 1 or the same
#'   as the number of pages.
#' @param paper.limits Numerical. Extends the edges of the paper in all
#'   directions.
#' @param page.number Position of the page number. Defaults to none.
#' @param page.number.x Distance the page number is pushed away from the text
#'   along the x-axis.
#' @param page.number.y Distance the page number is pushed away from the text
#'   along the y-axis.
#' @return A ggplot object with the given visualization.
#' @examples
#' \donttest{
#' library(dplyr)
#' library(stringr)
#' library(ggplot2)
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
ggpage_plot <- function(data, mapping = ggplot2::aes(),
                        paper.show = FALSE, paper.color = "grey90",
                        paper.alpha = 1, paper.limits = 3,
                        page.number = character(1), page.number.x = 3,
                        page.number.y = 3) {
  p <- data %>%
    ggplot2::ggplot(mapping = mapping) +
    ggplot2::geom_rect(ggplot2::aes(xmin = data$xmin, xmax = data$xmax,
                                    ymin = data$ymin, ymax = data$ymax)) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_void()

  if(paper.show) {
    paper_data <- data %>% paper_shape()

    p <- p +
      ggplot2::geom_rect(data = paper_data,
                         ggplot2::aes(xmin = paper_data$xmin + paper.limits,
                                      xmax = paper_data$xmax - paper.limits,
                                      ymin = paper_data$ymin + paper.limits,
                                      ymax = paper_data$ymax - paper.limits),
                         fill = paper.color, alpha = paper.alpha)
    p$layers = rev(p$layers)
  }

  directions <- c("top", "top-right", "right", "bottom-right", "bottom",
                  "bottom-left", "left", "top-left")
  if(page.number %in% directions) {
    paper_number_data <- data %>%
      paper_shape()

    page.number <- match.arg(page.number, directions)

    if(page.number %in% c("top-right", "right", "bottom-right")) {
      page_x <- paper_number_data$xmin + page.number.x
    } else if(page.number %in% c("top-left", "left", "bottom-left")) {
      page_x <- paper_number_data$xmax - page.number.x
    } else {
      page_x <- (paper_number_data$xmax - paper_number_data$xmin) / 2 +
        paper_number_data$xmin
    }

    if(page.number %in% c("top-left", "top", "top-right")) {
      page_y <- paper_number_data$ymin + page.number.y
    } else if(page.number %in% c("bottom-left", "bottom", "bottom-right")) {
      page_y <- paper_number_data$ymax - page.number.y
    } else {
      page_y <- (paper_number_data$ymax - paper_number_data$ymin) / 2 +
        paper_number_data$ymin
    }

    p <- p +
      ggplot2::geom_text(data = paper_number_data, inherit.aes = FALSE,
                         ggplot2::aes(x = page_x, y = page_y,
                                      label = paper_number_data$page))
  }

  p
}
