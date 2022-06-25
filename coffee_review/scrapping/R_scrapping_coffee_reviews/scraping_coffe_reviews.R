get_request_info <- function(.page){
  
  logger::log_info('`get_request_info` - Page n. {.page}')
  
  response <- glue::glue('https://www.coffeereview.com/advanced-search/page/{.page}') |> 
    httr::GET() |> 
    xml2::read_html() |> 
    xml2::xml_find_all('//p[@class="review-title"]//a')
  
  tibble::tibble(
    `urls` = xml2::xml_attr(response, 'href'),
    `names` = xml2::xml_text(response),
    `page` = .page
    )
}

monta_tabela <- function(url) {
  count <<- count + 1
  logger::log_info('`monta_tabela` - Review n. {count}')
  html <- xml2::read_html(url)
  
  names <- html |>  
    xml2::xml_find_all('//*[@class="review-roaster"]') |> 
    xml2::xml_text()

  brand <-html |> 
    xml2::xml_find_all('//*[@class="review-title"]') |>  
    xml2::xml_text()
  
  score <- html |> 
    xml2::xml_find_all('//*[@class="review-template-rating"]') |> 
    xml2::xml_text()
  
  textos <- html |> 
    xml2::xml_find_all('//*[@class="entry-content"]//p') |> 
    xml2::xml_text() |> 
    purrr::reduce(~ paste(..., " - "), .init = "")
    
  html |> 
    xml2::xml_find_all('//table') |> 
    rvest::html_table() |> 
    purrr::reduce(~ dplyr::bind_rows(...)) |> 
    tidyr::pivot_wider(
      names_from = X1,
      values_from = X2) |> 
    janitor::clean_names() |> 
    dplyr::mutate(
      names = names,
      brand = brand,
      score = score,
      review_text = textos,
      url = url)
}


doParallel::registerDoParallel()
count <- 0
purrr::map_dfr(1:109, get_request_info) |> 
  purrr::pluck('urls')|> 
  purrr::map_dfr(monta_tabela) -> df


df |> 
  dplyr::relocate(
    names:score,
    .before = roaster_location) |> 
  readr::write_csv('dados/coffe_reviews_infos.csv')

