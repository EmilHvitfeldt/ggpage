library(tidyverse)
library(ggpage)
tinderbox_paragraph <- tinderbox %>%
  mutate(split = dplyr::lag(stringr::str_length(text) < 20, default = FALSE),
         paragraph = cumsum(split)) %>%
  dplyr::select(text, paragraph) %>%
  nest(text) %>%
  mutate(text = purrr::map(data, unlist),
         text = purrr::map_chr(text, paste, collapse = " ")) %>%
  select(text)
