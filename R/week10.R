# Script Settings and Resources
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))
library(tidyverse)
library(caret)
library(haven)
library(jtools)

# Data Import and Cleaning
gss_tbl <- read_sav(file = "../data/GSS2016.sav", user_na = TRUE) %>% 
  zap_missing() %>%
  mutate(mosthrs = as.numeric(mosthrs)) %>%
  filter(!is.na(mosthrs)) %>%
  select(-hrs1, -hrs2) %>%
  select(where(~ mean(is.na(.)) < 0.75))

# Visualization
ggplot(gss_tbl, aes(x = mosthrs)) +
  geom_histogram() +
  labs(
    title = "Distribution of Work Hours",
    x = "Most Hours/Week Worked in Past Month",
    y = "Count",
  )

# Analysis
set.seed(39)

## Split into training and holdout
holdout_indices <- createDataPartition(gss_tbl$mosthrs, p = 0.75, list=F)
gss_training <- gss_tbl[holdout_indices, ]
gss_holdout <- gss_tbl[-holdout_indices, ]

## Create folds
ctrl <- trainControl(
  method = "cv", 
  number = 10, 
  indexOut = createFolds(gss_training$mosthrs, k = 10),
  verboseIter = T
)

## Grid search
grid <- expand.grid(
  alpha = c(0, 1),
  lambda = seq(0.0001, 1, length = 10)
)

## Model training
ols_model <- train(
  mosthrs ~ .,
  data = gss_training,
  method = "lm",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = ctrl
)

enet_model <- train(
  mosthrs ~ .,
  data = gss_training,
  method = "glmnet",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = ctrl,
  tuneGrid = grid
)

forest_model <- train(
  mosthrs ~ .,
  data = gss_training,
  method = "ranger",
  na.action = na.pass,
  preProcess = "medianImpute",
  trControl = ctrl,
  tuneGrid = expand.grid(
    mtry = c(23, 50, 100),
    splitrule = "variance",
    min.node.size = c(5, 10)
  )
)

exgb_model <- train(
  mosthrs ~ .,
  data = gss_training,
  method = "xgbLinear",
  na.action = na.pass,
  preProcess = "medianImpute",
  tuneLength = 10,
  trControl = ctrl
)

models <- list("OLS" = ols_model, "Elastic Net" = enet_model, "Random Forest" = forest_model)

resamples <- resamples(models)
predictions <- c(
  predict(ols_model, new_data = gss_holdout, na.action = na.pass),
  predict(enet_model, new_data = gss_holdout, na.action = na.pass),
  predict(forest_model, new_data = gss_holdout, na.action = na.pass)
)

ols_rsq <- cor(predictions[1], gss_holdout$mosthrs, use = "complete.obs")^2
enet_rsq <- cor(predictions[2], gss_holdout$mosthrs, use = "complete.obs")^2
forest_rsq <- cor(predictions[3], gss_holdout$mosthrs, use = "complete.obs")^2

table1_tbl <- tibble(
  algo = names(models),
  cv_rsq = sapply(
    models,
    function(model_i) {
      getTrainPerf(model_i)$TrainRsquared
    }
  ),
  ho_rsq = c(
    ols_rsq,
    enet_rsq,
    forest_rsq
  )
) %>%
  mutate(
    cv_rsq = round(cv_rsq, 2),
    ho_rsq = round(ho_rsq, 2),
    cv_rsq = format(cv_rsq, nsmall = 2),
    ho_rsq = format(ho_rsq, nsmall = 2),
    cv_rsq = sub("^0", "", cv_rsq),
    ho_rsq = sub("^0", "", ho_rsq)
  )

write_csv(table1_tbl, "../out/table1.csv")