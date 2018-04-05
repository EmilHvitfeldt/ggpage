#' Creates a visualization from the ggpage_build output
#'
#' @param data data.frame. Expects output from \code{ggpage_build} with
#'   optional intermediate analysis.
#' @param mapping Default list of aesthetic mappings to use for plot to be
#'   handed to internal \code{ggplot} call.
#' @param paper.show Shows the paper underneigt the text.
#' @param paper.color Color of the pages. Needs to be of length 1 or the same
#'   as the number of pages.
#' @param paper.alpha Alpha of the pages. Needs to be of length 1 or the same
#'   as the number of pages.
#' @param paper.limits Numerical.Extends the egdes of the paper in all
#'   directions.
#' @return A ggplot object with the given visualization.
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
ggpage_plot <- function(data, mapping = ggplot2::aes(),
                        paper.show = FALSE, paper.color = "grey90",
                        paper.alpha = 1, paper.limits = 3) {
  p <- data %>%
    ggplot2::ggplot(mapping = mapping) +
    ggplot2::geom_rect(ggplot2::aes(xmin = data$xmin, xmax = data$xmax,
                                    ymin = data$ymin, ymax = data$ymax)) +
    ggplot2::coord_fixed(ratio = 1) +
    ggplot2::theme_void()

  if(paper.show) {
    paper_data <- ggpage::tinderbox %>% ggpage_build() %>% paper_shape()

    p <- p +
      ggplot2::geom_rect(data = paper_data,
                         ggplot2::aes(xmin = paper_data$xmin + paper.limits,
                                      xmax = paper_data$xmax - paper.limits,
                                      ymin = paper_data$ymin + paper.limits,
                                      ymax = paper_data$ymax - paper.limits),
                         fill = paper.color, alpha = paper.alpha)
  }

  p$layers = rev(p$layers)

  p
}
