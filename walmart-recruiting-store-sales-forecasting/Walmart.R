## Libraries I need
library(tidyverse)
library(vroom)
library(tidymodels)
library(DataExplorer)

## Read in the Data
train <- vroom("train.csv")
test <- vroom("test.csv")
features <- vroom("features.csv")
walmart_departments <- tibble::tribble(
  ~Dept, ~Name,
  1,  "Candy & Tobacco",
  2,  "Heath & Beauty Aids",
  3,  "Stationery",
  4,  "Household Paper",
  5,  "Media & Gaming",
  6,  "Cameras & Supplies",
  7,  "Toys",
  8,  "Pets & Supplies",
  9,  "Sporting Goods",
  10, "Automotive",
  11, "Hardware",
  12, "Paint & Accessories",
  13, "Household Chemicals",
  14, "Cook & Dine",
  15, "Health and Wellness Clinics",
  16, "Lawn & Garden",
  17, "Home Decor",
  18, "Seasonal",
  19, "Piece Goods & Crafts",
  20, "Bath & Shower",
  21, "Books & Magazines",
  22, "Bedding",
  23, "Menswear",
  24, "Boyswear",
  25, "Shoes",
  26, "Infant Apparel",
  27, "Family Socks",
  28, "Hosiery",
  29, "Sleepwear",
  30, "Foundations",
  31, "Accessories",
  32, "Jewelry",
  33, "Griswold",
  34, "Ladies Apparel",
  35, "Plus Sizes & Maternity",
  36, "Ladies Outerwear",
  37, "Auto Service",
  38, "Pharmacy Rx",
  39, "Consumer Service",
  40, "OTC Pharmacy",
  42, "Automotive SubDepartment",
  46, "Cosmetics & Skincare",
  49, "Optical",
  50, "Optical",
  56, "Horticulture",
  58, "Wireless Service, Inc.",
  60, "Concept Stores",
  65, "Gasoline",
  67, "Celebration",
  71, "Furniture",
  72, "Electronics",
  74, "Home Management",
  75, "Fixtures",
  77, "Large Household Goods",
  79, "Infant Consumables Hardlines",
  80, "Service Deli",
  81, "Commercial Bread",
  82, "Impulse Merchandise",
  84, "Balloons & Flowers",
  85, "One-Hour Photo",
  86, "Walmart Services",
  87, "Wireless",
  88, "PMDC Signing",
  89, "Travel",
  90, "Dairy",
  91, "Frozen",
  92, "Grocery",
  93, "Meat",
  94, "Produce",
  95, "DSD Grocery",
  96, "Liquor",
  97, "Wall Deli",
  98, "Bakery",
  99, "Office & Store"
)

#########
## EDA ##
#########
plot_missing(features)
plot_missing(test)

### Impute Missing Markdowns
features <- features %>%
  mutate(across(starts_with("MarkDown"), ~ replace_na(., 0))) %>%
  mutate(across(starts_with("MarkDown"), ~ pmax(., 0))) %>%
  mutate(
    MarkDown_Total = rowSums(across(starts_with("MarkDown")), na.rm = TRUE),
    MarkDown_Flag = if_else(MarkDown_Total > 0, 1, 0),
    MarkDown_Log   = log1p(MarkDown_Total)
  ) %>%
  select(-MarkDown1, -MarkDown2, -MarkDown3, -MarkDown4, -MarkDown5)

## Impute Missing CPI and Unemployment
feature_recipe <- recipe(~., data=features) %>%
  step_mutate(DecDate = decimal_date(Date)) %>%
  step_impute_bag(CPI, Unemployment,
                  impute_with = imp_vars(DecDate, Store))
imputed_features <- juice(prep(feature_recipe))

########################
## Merge the Datasets ##
########################

joined_Train <- left_join(train, imputed_features, by=c("Store", "Date")) %>%
  select(-IsHoliday.y) %>%
  rename(IsHoliday=IsHoliday.x) %>%
  select(-MarkDown_Total)%>%
  left_join(walmart_departments, by = "Dept")
joined_Test <- left_join(test, imputed_features, by=c("Store", "Date")) %>%
  select(-IsHoliday.y) %>%
  rename(IsHoliday=IsHoliday.x) %>%
  select(-MarkDown_Total)%>%
  left_join(walmart_departments, by = "Dept")
plot_missing(joined_Train)
plot_missing(joined_Test)

pairs <- joined_Train %>% distinct(Store, Dept)


# --- 1. Select a store/dept pair ---
store <- pairs$Store[1]
dept  <- pairs$Dept[1]

# --- 2. Filter subset for that pair ---
df <- joined_Train %>%
  filter(Store == store, Dept == dept)

# --- 3. Safely summarise even if df is empty ---
summary_result <- df %>%
  summarise(
    n_rows     = n(),
    non_na_y   = sum(!is.na(Weekly_Sales)),
    min_date   = if (n() == 0) NA_Date_ else min(Date),
    max_date   = if (n() == 0) NA_Date_ else max(Date)
  )

# --- 4. Print diagnostic info ---
if (nrow(df) == 0) {
  message("⚠️ No rows found for Store = ", store, 
          ", Dept = ", dept)
}

summary_result



library(tidyverse)
library(prophet)

###########################################
## 1. Choose Store and Department
###########################################

library(prophet)
library(tidyverse)

# --- 0. Select store/dept pair (now that pairs works) ---
pairs <- joined_Train %>% distinct(Store, Dept)

store <- pairs$Store[1]
dept  <- pairs$Dept[1]

# --- 1. Prepare training data for Prophet ---
sd_train <- joined_Train %>%
  filter(Store == store, Dept == dept) %>%
  transmute(
    ds = Date,
    y  = Weekly_Sales,
    x1 = as.numeric(IsHoliday),
    x2 = Temperature,
    x3 = Fuel_Price
  )

# --- 2. Prepare test data ---
sd_test <- joined_Test %>%
  filter(Store == store, Dept == dept) %>%
  transmute(
    ds = Date,
    x1 = as.numeric(IsHoliday),
    x2 = Temperature,
    x3 = Fuel_Price
  )

# --- 3. Fit Prophet model ---
prophet_model <- prophet() %>%
  add_regressor("x1") %>%
  add_regressor("x2") %>%
  add_regressor("x3")

prophet_model <- fit.prophet(prophet_model, sd_train)

# --- 4. Predict fitted + forecasted values ---
fitted_vals <- predict(prophet_model, sd_train)
test_preds   <- predict(prophet_model, sd_test)

# --- 5. Plot ---
ggplot() +
  geom_line(
    data = sd_train,
    aes(x = ds, y = y, color = "Data"),
    linewidth = 0.9
  ) +
  geom_line(
    data = fitted_vals,
    aes(x = as.Date(ds), y = yhat, color = "Fitted"),
    linewidth = 1
  ) +
  geom_line(
    data = test_preds,
    aes(x = as.Date(ds), y = yhat, color = "Forecast"),
    linewidth = 1
  ) +
  scale_color_manual(values = c(
    "Data"     = "black",
    "Fitted"   = "blue",
    "Forecast" = "red"
  )) +
  labs(
    title = paste("Prophet Model - Store", store, "Dept", dept),
    x = "Date",
    y = "Weekly Sales",
    color = ""
  ) +
  theme_bw(14)


##################################
## Loop Through the Store-depts ## 
## and generate predictions.    ##
##################################
all_preds <- tibble(Id = character(), Weekly_Sales = numeric())
n_storeDepts <- fullTest %>% distinct(Store, Dept) %>% nrow()
cntr <- 0
for(store in unique(fullTest$Store)){
  
  store_train <- fullTrain %>%
    filter(Store==store)
  store_test <- fullTest %>%
    filter(Store==store)
  
  for(dept in unique(store_test$Dept)){
    
    ## Filter Test and Training Data
    dept_train <- store_train %>%
      filter(Dept==dept)
    dept_test <- store_test %>%
      filter(Dept==dept)
    
    ## If Statements for data scenarios
    if(nrow(dept_train)==0){
      
      ## Predict 0
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
               Weekly_Sales=0)
      
    } else if(nrow(dept_train) < 10 && nrow(dept_train) > 0){
      
      ## Predict the mean
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=mean(dept_train$Weekly_Sales))
      
    } else {
      
      ## Fit a penalized regression model
      my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
        step_mutate(Holiday = as.integer(IsHoliday)) %>%
        step_date(Date, features=c("month","year")) %>%
        step_rm(Date, Store, Dept, IsHoliday)
      prepped_recipe <- prep(my_recipe)
      tst <- bake(prepped_recipe, new_data=dept_test)
      
      my_model <- rand_forest(mtry=3,
                              trees=100,
                              min_n=5) %>%
        set_engine("ranger") %>%
        set_mode("regression")
      
      my_wf <- workflow() %>%
        add_recipe(my_recipe) %>%
        add_model(my_model) %>%
        fit(dept_train)

      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=predict(my_wf, new_data = .) %>%
                    pull(.pred))
      
    }
  
    ## Bind predictions together
    all_preds <- bind_rows(all_preds,
                           preds)
    
    ## Print out Progress
    cntr <- cntr+1
    cat("Store", store, "Department", dept, "Completed.",
        round(100 * cntr / n_storeDepts, 1), "% overall complete.\n")
    
  } ## End Dept Loop
  
} ## End Store Loop

## Write out after each store so I don't have to start over
vroom_write(x=all_preds, 
            file=paste0("./Predictions.csv"), delim=",")

# ---- Libraries ----
library(tidyverse)
library(tidymodels)
library(vroom)
library(doParallel)
library(tictoc)        # for timing (optional but helpful)
set.seed(2025)
# ---- 0. Load libraries ----
library(tidymodels)
tidymodels_prefer()

# ---- 1. Data ----
data(mtcars)

mtcars <- mtcars %>%
  mutate(across(where(is.numeric), as.double))

set.seed(123)

# ---- 2. Split ----
splits <- initial_split(mtcars, prop = 0.8)
train  <- training(splits)
test   <- testing(splits)

# ---- 3. Recipe ----
rf_recipe <- recipe(mpg ~ ., data = train)

# ---- 4. Model spec (tunable mtry + min_n) ----
rf_spec <- rand_forest(
  mtry  = tune(),
  min_n = tune(),
  trees = 500
) %>%
  set_engine("ranger") %>%
  set_mode("regression")

# ---- 5. Workflow ----
wf <- workflow() %>%
  add_model(rf_spec) %>%
  add_recipe(rf_recipe)

# ---- 6. CV folds ----
cv_folds <- vfold_cv(train, v = 5)

# ---- 7. Parameter grid ----
rf_params <- parameters(rf_spec)

rf_grid <- grid_random(
  rf_params,
  size = 10
)

# ---- 8. Tune ----
rf_tuned <- tune_grid(
  wf,
  resamples = cv_folds,
  grid = rf_grid,
  metrics = metric_set(rmse, rsq)
)

rf_tuned




# ---- Use existing joined_Train (from your code). If not present, set fullTrain <- joined_Train ----
joined_Train <- joined_Train %>%
  mutate(Id = paste(Store, Dept, Date, sep = "_"))

joined_Test <- joined_Test %>%
  mutate(Id = paste(Store, Dept, Date, sep = "_"))
fullTrain <- joined_Train

# ---- 0. Select a sample of store-dept pairs to experiment on ----
pairs <- fullTrain %>% distinct(Store, Dept)
n_pairs_to_sample <- 20   # change this (e.g., 50) depending on compute budget
sample_pairs <- pairs %>% slice_sample(n = min(n_pairs_to_sample, nrow(pairs)))

# filter training data to only those pairs
sampled_train <- fullTrain %>%
  inner_join(sample_pairs, by = c("Store", "Dept")) %>%
  arrange(Store, Dept, Date)

# quick diagnostic
message("Rows in sampled training set: ", nrow(sampled_train),
        " (", nrow(sample_pairs), " store–dept pairs)")

# ---- 1. Resampling (cross-validation) ----
cv_splits <- vfold_cv(sampled_train, v = 5, strata = Store)

# ---- 2. Recipe (same recipe for all models) ----
common_recipe <- recipe(Weekly_Sales ~ ., data = sampled_train) %>%
  update_role(Id, new_role = "id") %>%                       # mark Id as id role
  step_mutate(Holiday = as.integer(IsHoliday)) %>%
  step_date(Date, features = c("year", "month", "dow")) %>%
  step_rm(Date, Store, Dept, IsHoliday) %>%
  step_impute_median(all_numeric_predictors()) %>%
  step_unknown(all_nominal_predictors()) %>%                 # handle unseen/NA factor levels
  step_normalize(all_numeric_predictors()) %>%
  step_dummy(all_nominal_predictors(), one_hot = TRUE) %>%
  step_nzv(all_predictors())

# prep to check features (not baked yet for tuning)
prepped <- prep(common_recipe)
juice(prepped) %>% glimpse()

# ---- 3. Metrics ----
my_metrics <- metric_set(rmse, mae, rsq)

# ---- 4. Models to tune ----
tidymodels_prefer()
rf_spec <- rand_forest(
  mtry  = tune(),     # must be tune()
  trees = 1000,
  min_n = tune()      # tune a second parameter
) %>%
  set_engine("ranger") %>%
  set_mode("regression")


xgb_spec <- boost_tree(
  trees = tune(),
  learn_rate = tune(),
  mtry = tune(),
  tree_depth = tune(),
  min_n = tune(),
  loss_reduction = tune(),
  sample_size = tune()
) %>%
  set_engine("xgboost") %>%
  set_mode("regression")


glmnet_spec <- linear_reg(
  penalty = tune(),
  mixture = tune()
) %>%
  set_engine("glmnet")


# ---- 5. Workflows ----
wf_rf <- workflow() %>% add_model(rf_spec) %>% add_recipe(common_recipe)
wf_xgb <- workflow() %>% add_model(xgb_spec) %>% add_recipe(common_recipe)
wf_glm <- workflow() %>% add_model(glmnet_spec) %>% add_recipe(common_recipe)

# ---- 6. Tuning grids ----

# Get model parameters from model specs (not workflows)
rf_params  <- parameters(rf_spec)
xgb_params <- parameters(xgb_spec)
glmnet_params <- parameters(glmnet_spec)

rf_params <- parameters(rf_spec) %>%
  update(mtry = mtry(range = c(1L, 30L)))   # rough range; will be finalized with training data

xgb_params <- parameters(xgb_spec)
glmnet_params <- parameters(glmnet_spec)

rf_spec <- rand_forest() %>%
  set_engine("ranger") %>%
  set_mode("regression")

xgb_spec <- boost_tree() %>%
  set_engine("xgboost") %>%
  set_mode("regression")

glmnet_spec <- linear_reg() %>%
  set_engine("glmnet")
parameters(rf_spec)
parameters(xgb_spec)
parameters(glmnet_spec)
pak::pak("tidymodels/tune")
pak::pak("tidymodels/parsnip")
pak::pak("tidymodels/dials")
pak::pak("tidymodels/workflows")


# Finalize mtry using the actual training data (ensures upper bound is sensible)
rf_params <- finalize(rf_params, training = sampled_train)

# Build the random grids
rf_grid <- grid_random(rf_params, size = 20)
xgb_grid <- grid_random(xgb_params, size = 25)
glmnet_grid <- grid_random(glmnet_params, size = 20)

# ---- 7. Parallel backend ----
n_cores <- parallel::detectCores(logical = FALSE)
cl <- makePSOCKcluster(max(1, n_cores - 1))
registerDoParallel(cl)

ctrl <- control_grid(save_pred = TRUE, verbose = TRUE, allow_par = TRUE)

# ---- 8. Tune each model and time it ----
results_list <- list()
time_info <- tibble(model = character(), seconds = numeric())

tune_and_time <- function(wf, grid, cv_splits, metrics, ctrl, model_name){
  tic(msg = model_name)
  res <- tune_grid(wf,
                   resamples = cv_splits,
                   grid = grid,
                   metrics = metrics,
                   control = ctrl,
                   verbose = TRUE)
  t <- toc(quiet = TRUE)    # returns list with $tic and $toc
  elapsed <- as.numeric(t$toc - t$tic, units = "secs")
  list(result = res, time_seconds = elapsed)
}

# Random Forest
rf_out <- tune_and_time(wf_rf, rf_grid, cv_splits, my_metrics, ctrl, "ranger_rf")
results_list[["ranger_rf"]] <- rf_out$result
time_info <- time_info %>% add_row(model = "ranger_rf", seconds = rf_out$time_seconds)

# XGBoost
xgb_out <- tune_and_time(wf_xgb, xgb_grid, cv_splits, my_metrics, ctrl, "xgboost")
results_list[["xgboost"]] <- xgb_out$result
time_info <- time_info %>% add_row(model = "xgboost", seconds = xgb_out$time_seconds)

# Elastic Net (glmnet) -- fixed variable name glmnet_grid
glm_out <- tune_and_time(wf_glm, glmnet_grid, cv_splits, my_metrics, ctrl, "glmnet")
results_list[["glmnet"]] <- glm_out$result
time_info <- time_info %>% add_row(model = "glmnet", seconds = glm_out$time_seconds)

# stop cluster
stopCluster(cl)
registerDoSEQ()

# ---- 9. Summarize tuning results: best RMSE and runtime ----
best_summary <- map_dfr(names(results_list), function(mnm){
  res <- results_list[[mnm]]
  best <- select_best(res, "rmse")
  best_metrics <- collect_metrics(res) %>%
    filter(.metric == "rmse") %>%
    arrange(mean) %>%
    slice(1)
  tibble(
    model = mnm,
    best_rmse = best_metrics$mean,
    best_rmse_std_err = best_metrics$std_err,
    runtime_seconds = time_info %>% filter(model == mnm) %>% pull(seconds)
  )
})

print(best_summary)

# ---- 10. Get the "best tuned model" and show the cv error that tune_grid reported ----
best_model_row <- best_summary %>% arrange(best_rmse) %>% slice(1)
best_model_name <- best_model_row$model
message("Best model by CV RMSE: ", best_model_name)

best_tune_res <- results_list[[best_model_name]]
best_params <- select_best(best_tune_res, metric = "rmse")
message("Best tuning parameters:\n")
print(best_params)

best_rmse_metric <- collect_metrics(best_tune_res) %>%
  filter(.metric == "rmse") %>%
  arrange(mean) %>%
  slice(1)
message("Cross-validated RMSE for best tuned model (as reported by tune_grid): ")
print(best_rmse_metric)

# ---- 11. Finalize & fit best workflow on full sampled_train (optional) ----
final_wf <- switch(best_model_name,
                   "ranger_rf" = finalize_workflow(wf_rf, best_params),
                   "xgboost" = finalize_workflow(wf_xgb, best_params),
                   "glmnet" = finalize_workflow(wf_glm, best_params))

final_fit <- final_wf %>% fit(data = sampled_train)

learning_suite_rmse <- best_rmse_metric$mean
message("LearningSuite RMSE to report: ", round(learning_suite_rmse, 2))

# ---- 12. Save a small summary CSV ----
write_csv(best_summary, "model_tuning_summary.csv")
message("Saved model summary to ./model_tuning_summary.csv")

#############################################
## SAMPLE STORE-DEPTS & MODEL COMPARISON   ##
#############################################
# ... (Keep this section as is) ...

print(results_summary)


##################################
## Loop Through the Store-depts ## 
## and generate predictions.    ##
##################################
# Create a new tibble to store the CV error for the prediction model
cv_errors_final <- tibble(Store = numeric(), Dept = numeric(), CV_RMSE = numeric())

all_preds <- tibble(Id = character(), Weekly_Sales = numeric())
n_storeDepts <- fullTest %>% distinct(Store, Dept) %>% nrow()
cntr <- 0

# Define the CV folds for reuse
cv_folds_pred <- vfold_cv(fullTrain, v = 10, strata = Weekly_Sales)

for(store in unique(fullTest$Store)){
  
  store_train <- fullTrain %>%
    filter(Store==store)
  store_test <- fullTest %>%
    filter(Store==store)
  
  for(dept in unique(store_test$Dept)){
    
    ## Filter Test and Training Data
    dept_train <- store_train %>%
      filter(Dept==dept)
    dept_test <- store_test %>%
      filter(Dept==dept)
    
    current_cv_rmse <- NA_real_ # Initialize CV RMSE for this pair
    
    ## If Statements for data scenarios
    if(nrow(dept_train)==0){
      
      ## Predict 0
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=0)
      
    } else if(nrow(dept_train) < 10 && nrow(dept_train) > 0){
      
      ## Predict the mean
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=mean(dept_train$Weekly_Sales))
      
    } else {
      
      ## Fit a penalized regression model
      my_recipe <- recipe(Weekly_Sales ~ ., data = dept_train) %>%
        step_mutate(Holiday = as.integer(IsHoliday)) %>%
        step_date(Date, features=c("month","year")) %>%
        step_rm(Date, Store, Dept, IsHoliday)
      
      my_model <- rand_forest(mtry=3,
                              trees=100,
                              min_n=5) %>%
        set_engine("ranger") %>%
        set_mode("regression")
      
      my_wf <- workflow() %>%
        add_recipe(my_recipe) %>%
        add_model(my_model)
      
      # --- NEW: Calculate CV Error ---
      # Use a new set of CV folds just for this department's data
      dept_cv_folds <- vfold_cv(dept_train, v = 10, strata = Weekly_Sales)
      
      cv_results <- my_wf %>%
        fit_resamples(
          resamples = dept_cv_folds,
          metrics = metric_set(rmse)
        )
      
      current_cv_rmse <- cv_results %>%
        collect_metrics() %>%
        filter(.metric == "rmse") %>%
        pull(mean)
      
      # Store the CV RMSE
      cv_errors_final <- cv_errors_final %>%
        add_row(
          Store = store,
          Dept = dept,
          CV_RMSE = current_cv_rmse
        )
      
      cat("  --> CV RMSE:", round(current_cv_rmse, 2), "\n")
      # --- END NEW ---
      
      # Now fit the model on the full training data for prediction
      final_fit <- my_wf %>%
        fit(dept_train)
      
      preds <- dept_test %>%
        transmute(Id=paste(Store, Dept, Date, sep="_"),
                  Weekly_Sales=predict(final_fit, new_data = .) %>%
                    pull(.pred))
      
    }
    
    ## Bind predictions together
    all_preds <- bind_rows(all_preds,
                           preds)
    
    ## Print out Progress
    cntr <- cntr+1
    cat("Store", store, "Department", dept, "Completed.",
        round(100 * cntr / n_storeDepts, 1), "% overall complete.\n")
    
  } ## End Dept Loop
  
} ## End Store Loop

# --- NEW: Print a summary of the CV errors from the final model ---
cat("\n\n##################################\n")
cat("## Final Model Cross-Validated RMSE Summary ##\n")
cat("##################################\n")

final_cv_summary <- cv_errors_final %>%
  summarize(
    Mean_CV_RMSE = mean(CV_RMSE, na.rm = TRUE),
    Median_CV_RMSE = median(CV_RMSE, na.rm = TRUE)
  )

print(final_cv_summary)
# --- END NEW ---

## Write out after each store so I don't have to start over
vroom_write(x=all_preds, 
            file=paste0("./Predictions.csv"), delim=",")
