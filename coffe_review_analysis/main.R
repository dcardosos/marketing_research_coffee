library(ggplot2)
library(tidymodels)


df <- readr::read_csv('R_scrapping_coffee_reviews/coffe_reviews_infos.csv')

   

t1 <- df |> 
  tidyr::separate(
    col = agtron,
    into = c('first', 'second'),
    sep = '/') |> 
  dplyr::mutate(
    dplyr::across(first:second, .fns = as.integer),
    media = (first + second) / 2) |> 
  dplyr::arrange(-media) |> 
  #dplyr::select(first, media, roast_level) |> 
  dplyr::mutate( agtron_range = dplyr::case_when(
    media >= 25 & media <= 30 ~ '25-30',
    media >= 31 & media <= 35 ~ '30-35',
    media >= 36 & media <= 40 ~ '35-40',
    media >= 41 & media <= 50 ~ '40-50',
    media >= 51 & media <= 70 | roast_level == 'Medium' ~ '50-70',
    media >= 71 & media <= 99999 | roast_level == 'Light' ~ '70-80'))

t1 |> 
  dplyr::left_join(t2)

t2 <- xml2::read_html('https://www.coffeereview.com/coffee-reference/coffee-categories/roast-styles/roast-table/') |> 
  xml2::xml_find_first('//table') |> 
  rvest::html_table(header = TRUE) |> 
  janitor::clean_names() |>
  dplyr::mutate(agtron_numbers = stringr::str_replace_all(agtron_numbers, " ", "")) |> 
  tidyr::separate(
    col = agtron_numbers,
    into = c('upper', 'lower'),
    sep = '-|–') |> 
  dplyr::mutate(agtron_range = paste0(lower,'-',upper)) |> 
  dplyr::select(-lower, -upper)
  

df |> 
  ggplot() +
  geom_histogram(aes(score)) +
  theme_minimal()