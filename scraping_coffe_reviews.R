get_info <- function(.page){
  
  logger::log_info('Page n. {.page}')
  
  response <- glue::glue('https://www.coffeereview.com/advanced-search/page/{.page}') |> 
    httr::GET() |> 
    xml2::read_html() |> 
    xml2::xml_find_all('//p[@class="review-title"]//a')
  
  tibble::tibble(
    `urls` = xml2::xml_attr(response, 'href'),
    `names` = xml2::xml_text(response)
    )
}


monta_tabela <- function(url) {
  
  #logger::log_info('')
  
  url |> 
    xml2::read_html() |> 
    xml2::xml_find_all('//table') |> 
    rvest::html_table() |> 
    purrr::reduce(~ dplyr::bind_rows(...)) |> 
    tidyr::pivot_wider(
      names_from = X1,
      values_from = X2) |> 
    janitor::clean_names()
}

doParallel::registerDoParallel()
purrr::map_dfr(1:109, get_info) |> 
  dplyr::pull('urls')|> 
  purrr::map_dfr(monta_tabela) -> df


df |> 
  readr::write_csv('Documents/mkt/coffe_reviews_infos.csv')

