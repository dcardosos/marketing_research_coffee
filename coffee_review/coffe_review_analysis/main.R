#library(ggplot2)
library(tidymodels)


df <- readr::read_csv('dados/coffe_reviews_infos.csv')

# tratando o agtron number ----------------------------------------------------
t1 <- df |> 
  tidyr::separate(
    col = agtron,
    into = c('first', 'second'),
    sep = '/') |> 
  dplyr::mutate(
    dplyr::across(first:second, .fns = as.integer),
    media = (first + second) / 2) |> 
  #dplyr::arrange(-media) |> 
  #dplyr::select(first, media, roast_level) |> 
  dplyr::mutate( agtron_range = dplyr::case_when(
    media >= 25 & media <= 30 ~ '25-30',
    media >= 31 & media <= 35 ~ '30-35',
    media >= 36 & media <= 40 ~ '35-40',
    media >= 41 & media <= 50 ~ '40-50',
    media >= 51 & media <= 70 | roast_level == 'Medium' ~ '50-70',
    media >= 71 & media <= 99999 | roast_level == 'Light' ~ '70-80'))

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

tab <- t1 |> 
  dplyr::left_join(t2)


# tratando os prices ----------------------------------------------------------
tab |> 
  tidyr::separate(est_price, c('price', 'quantity'), sep  = '/') |> 
  tidyr::separate(price, c('country_money', 'price_value'), sep = ' ')  |> 
  tidyr::separate(quantity, c('qtd_value', 'type'), sep = ' ') |>
  dplyr::mutate(
    price_value = dplyr::case_when(
      is.na(price_value) ~ country_money,
      TRUE ~ price_value),
    country_money = dplyr::case_when(
      country_money == price_value ~ 'NA',
      TRUE ~ country_money)) |>
  dplyr::select(country_money, price_value, qtd_value, type) |> 
  dplyr::count(type) 
  
  
  



# rand forest --------------------------------------------------------------------
dados <- tab |> 
  dplyr::mutate(roast_level = dplyr::case_when(
    is.na(roast_level) ~ 'Medium-Light',
    TRUE ~ roast_level)) |>
  dplyr::select(score, roast_level, roast_color, aroma, acidity_structure, body, flavor, aftertaste, bean_surface)

set.seed(123)
tab_split <- initial_split(dados)
train <- training(tab_split)
test <- testing(tab_split)

set.seed(234)
folds <- vfold_cv(train)

# 
usemodels::use_glmnet(score ~ ., train)
#

ranger_recipe <- 
  recipe(formula = score ~ ., data = train) %>% 
  step_string2factor(one_of("roast_level", "roast_color", "bean_surface")) %>%
  step_novel(all_nominal_predictors()) |> 
  step_impute_linear(aroma, acidity_structure, body, flavor, aftertaste, impute_with = imp_vars(roast_level)) |> 
  step_impute_mode(bean_surface, roast_color)
  
ranger_spec <- 
  rand_forest(mtry = tune(), min_n = tune(), trees = 100) %>% 
  set_mode("regression") %>% 
  set_engine("ranger") 

ranger_workflow <- 
  workflow() %>% 
  add_recipe(ranger_recipe) %>% 
  add_model(ranger_spec) 

set.seed(6964)
doParallel::registerDoParallel()
ranger_tune <-
  tune_grid(
    ranger_workflow, 
    resamples = folds, 
    grid = 10)


show_best(ranger_tune, metric = "rmse")
show_best(ranger_tune, metric = "rsq")

autoplot(ranger_tune)

final_rf <- ranger_workflow |> 
  finalize_workflow(select_best(ranger_tune))

final_rf


df_fit <- last_fit(final_rf, tab_split)
df_fit

collect_metrics(df_fit)

collect_predictions(df_fit) |> 
  ggplot(aes(score, .pred)) + 
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "midnightblue") + 
  coord_fixed()

library(vip)

imp_spec <- ranger_spec |> 
  finalize_model(select_best(ranger_tune)) |> 
  set_engine("ranger", importance = "permutation")


workflow() |> 
  add_recipe(ranger_recipe) |> 
  add_model(imp_spec) |> 
  fit(train) |> 
  pull_workflow_fit() |> 
  vip(aesthetic = list(alpha = 0.8, fill = "midnightblue"))
