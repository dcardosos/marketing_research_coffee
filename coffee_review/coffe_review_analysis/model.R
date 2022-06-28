library(magrittr)
library(tidymodels)
# model ---------------------------------------------------------------------
dados <- 'dados/wip_clean_coffe_reviews.csv' |> 
  readr::read_csv() |> 
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

## linear model 
usemodels::use_glmnet(score ~ ., train)

lm_recipe <- 
  recipe(formula = score ~ ., data = train) %>% 
  step_string2factor(one_of("roast_level", "roast_color", "bean_surface")) %>% 
  step_novel(all_nominal_predictors()) %>% 
  step_dummy(all_nominal_predictors()) %>% 
  step_zv(all_predictors()) %>% 
  step_normalize(all_numeric_predictors()) 

lm_spec <- 
  linear_reg(penalty = tune(), mixture = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("lm") 

lm_workflow <- 
  workflow() %>% 
  add_recipe(lm_recipe) %>% 
  add_model(lm_spec) 

set.seed(345)
lm_tune <- 
  tune_grid(lm_workflow, resamples = folds, grid = 10)

# show_best(lm_tune, metric = "rmse")
# show_best(lm_tune, metric = "rsq")
# 
# autoplot(lm_tune)

final_lm <- lm_workflow |> 
  finalize_workflow(select_best(lm_tune))

df_lm <- last_fit(final_lm, tab_split)

collect_metrics(df_lm)

collect_predictions(df_lm) |> 
  ggplot(aes(score, .pred)) + 
  geom_abline(lty = 2, color = "gray50") +
  geom_point(alpha = 0.5, color = "midnightblue") + 
  coord_fixed()


final_lm |> 
  fit(train) |> 
  extract_fit_parsnip() |> 
  tidy() |> 
  filter(term != '(Intercept)', !is.na(estimate)) |> 
  ggplot(aes(reorder(term, estimate), estimate)) +
  geom_col() +
  theme_minimal() +
  labs(x = NULL, y = 'Importance') +
  coord_flip()


vip::vip(df_lm)

## rand forest 
usemodels::use_ranger(score ~ ., train)

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

set.seed(567)
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

imp_spec <- ranger_spec |> 
  finalize_model(select_best(ranger_tune)) |> 
  set_engine("ranger", importance = "permutation")

workflow() |> 
  add_recipe(ranger_recipe) |> 
  add_model(imp_spec) |> 
  fit(train) |> 
  pull_workflow_fit() |> 
  vip(aesthetic = list(alpha = 0.8, fill = "midnightblue"))


## decision tree
dt_recipe <- recipe(score ~ . , data = train)

dt_model <- 
  decision_tree(
    cost_complexity = tune(), 
    tree_depth = tune(),
    min_n = tune()) %>% 
  set_mode("regression") %>% 
  set_engine("rpart") 

dt_workflow <- 
  workflow() |> 
  add_model(dt_model) |> 
  add_recipe(dt_recipe)

dt_grid <- 
  grid_regular(
    cost_complexity(), 
    tree_depth(), 
    min_n(), 
    levels = 4)

set.seed(456)
dt_tuning <- 
  dt_workflow |> 
  tune_grid(resamples = folds, grid = dt_grid)

show_best(dt_tuning)

autoplot(dt_tuning)

final_dt <- dt_workflow |> 
  finalize_workflow(select_best(dt_tuning))

final_dt

dt_fit <- final_dt |> 
  fit(data = dados) |> 
  extract_fit_parsnip()

vip::vip(dt_fit)

pdf("tree.pdf")
rpart.plot::rpart.plot(dt_fit$fit, roundint = FALSE)
dev.off()