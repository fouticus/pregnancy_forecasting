{
  rm(list=ls())
  gc()
  
  data_dir <- "data"
  out_dir <- "output"
  if(!dir.exists(file.path(out_dir, "modeling_final"))){dir.create(file.path(out_dir, "modeling_final"))}
  
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  theme_set(theme_bw())
  library(ggfan)
  library(splines)
  library(rstan)
  rstan_options(javascript=FALSE, auto_write=TRUE)
  iters=20000
  chains=20
  cores = 10
  options(mc.cores = cores)
}

################################################################################
## Load data from excel files

{
  f1 <- file.path(data_dir, "Delivery_Cohort_Counts.xlsx")
  sh1 <- excel_sheets(f1)
  raw_del <- lapply(sh1, function(sh) read_xlsx(f1, sh))
  
  f2 <- file.path(data_dir, "Pregnancy_Cohort_Counts.xlsx")
  sh2 <- excel_sheets(f2)
  raw_pre <- lapply(sh2, function(sh) read_xlsx(f2, sh))
  
  f3 <- file.path(data_dir, "Delivery_Cohort_Counts_Feb_Aug.xlsx")
  sh3 <- excel_sheets(f3)
  raw_del2 <- lapply(sh3, function(sh) read_xlsx(f3, sh))
  
  f4 <- file.path(data_dir, "Pregnancy_Cohort_Counts_Feb_Aug.xlsx")
  sh4 <- excel_sheets(f4)
  raw_pre2 <- lapply(sh4, function(sh) read_xlsx(f4, sh))
  
  rm(f1, f2, f3, f4, sh1, sh2, sh3, sh4)
}

################################################################################
### format data for stan

{
  ci_pre <- c("UNCH MCLENDON CLINICAL LABORATORIES", "UNCH REX LABORATORY")
  ci_del <- c("JOHNSTON HOSPITAL", "REX HOSPITAL", "UNC HOSPITALS")
  df <- rbind(
    full_join(
      raw_pre[[4]] %>% 
        mutate(`UNCH REX LABORATORY` = as.numeric(ifelse(`UNCH REX LABORATORY` == "<10", 5, `UNCH REX LABORATORY`))) %>%
        dplyr::select(c(ci_pre, WeekStart)) %>% pivot_longer(ci_pre) %>% group_by(WeekStart) %>% summarize(screened=sum(value)) %>% rename(week=WeekStart),
      raw_del[[6]] %>% 
        dplyr::select(c(ci_del, WeekStart)) %>% pivot_longer(ci_del) %>% group_by(WeekStart) %>% summarize(delivered=sum(value)) %>% rename(week=WeekStart)
    ),
    full_join(
      raw_pre2[[2]] %>%
        mutate(`UNCH REX LABORATORY` = as.numeric(ifelse(`UNCH REX LABORATORY` == "<10", 5, `UNCH REX LABORATORY`))) %>%
        dplyr::select(c(ci_pre, WeekStart)) %>% pivot_longer(ci_pre) %>% group_by(WeekStart) %>% summarize(screened=sum(value)) %>% rename(week=WeekStart),
      raw_del2[[5]] %>% 
        dplyr::select(c(ci_del, WeekStart)) %>% pivot_longer(ci_del) %>% group_by(WeekStart) %>% summarize(delivered=sum(value)) %>% rename(week=WeekStart)
    )
  ); df <- df[1:(nrow(df)-1),] # (Take out errant last data point.)
  
  # count percent censored
  cens <- rbind(
    raw_pre[[4]] %>% 
      mutate(`UNCH REX LABORATORY` = as.numeric(ifelse(`UNCH REX LABORATORY` == "<10", 5, `UNCH REX LABORATORY`))) %>%
      dplyr::select(c(ci_pre, WeekStart)) %>% pivot_longer(ci_pre),
    raw_pre2[[2]] %>% 
      mutate(`UNCH REX LABORATORY` = as.numeric(ifelse(`UNCH REX LABORATORY` == "<10", 5, `UNCH REX LABORATORY`))) %>%
      dplyr::select(c(ci_pre, WeekStart)) %>% pivot_longer(ci_pre)
  )
  table(cens$name)
  cens %>% filter(value==5)
  cens %>% summarize(mean(value==5), sum(value==5))
  
  # count percent births at other hospitals
  count <- rbind(
    raw_del[[6]] %>% 
      dplyr::select(-c(OVERALL_COUNT, WeekStart, WeekOf)) %>%
      mutate(across(everything(), as.character)) %>% 
      pivot_longer(everything()),
    raw_del2[[5]] %>%
      dplyr::select(-c(OVERALL_COUNT, WeekStart, WeekOf)) %>%
      mutate(across(everything(), as.character)) %>% 
      pivot_longer(everything())
  ) %>%
  mutate(value = as.numeric(ifelse(value == "<10","5",value)),
         in_group = name %in% ci_del) %>%
  group_by(in_group) %>% summarize(value=sum(value))
  count[!count$in_group,]$value/sum(count$value)
  count[!count$in_group,]$value
  
  
      
  # ON WITH THE DATA PREP
  lags <- 20:30
  n <- nrow(df)
  n2 <- min(lags)
  # add rows to df for forecasting
  df2 <- data.frame(week = seq(max(df$week), by="1 week", length.out=(n2+1))[2:(n2+1)], screened=rep(NA, n2), delivered=rep(NA, n2))
  df <- rbind(df, df2)
  
  # standardize screenings
  screened_mean <- mean(df$screened, na.rm=T); screened_sd <- sd(df$screened, na.rm=T)
  df$screened_standard <- (df$screened - screened_mean)/screened_sd
  
  # prep design matrix for B-splines
  X <- matrix(0, nrow(df), length(lags))
  for(i in 1:nrow(X)){
    idx <- i - lags# + 1
    idx[idx<1] <- NA
    X[i,] <- df$screened[idx]
  }
  nNA = !is.na(rowSums(X)) # rows with no NA's, indicates where lagged regression is possible
  
  # train / val / test split
  t_train_end <- as.POSIXct("2019-12-31")
  t_val_end <- as.POSIXct("2020-12-31")
  df$train1 <- df$week <= t_train_end
  df$train2 <- df$week <= t_train_end & nNA
  df$val    <- t_train_end < df$week & df$week <= t_val_end
  df$test   <- t_val_end < df$week & !is.na(df$screened)
  df$fore   <- t_val_end < df$week
  
  n_train1 <- sum(df$train1)
  n_train2 <- sum(df$train2)
  n_val    <- sum(df$val)
  n_test   <- sum(df$test)
  n_fore   <- sum(df$fore)
  
  Ytrain1 <- (df %>% filter(train1) %>% dplyr::select(delivered))[[1]]
  Ytrain2 <- (df %>% filter(train2) %>% dplyr::select(delivered))[[1]]
  Yval    <- (df %>% filter(val)    %>% dplyr::select(delivered))[[1]]
  Ytest   <- (df %>% filter(test)   %>% dplyr::select(delivered))[[1]]
  
  # standardize Y train before modeling
  Ytrain1_mean <- mean(Ytrain1); Ytrain1_sd <- sd(Ytrain1)
  Ytrain1_standard <- (Ytrain1 - Ytrain1_mean)/Ytrain1_sd  # standardized version of Ytrain
  Ytrain2_mean <- mean(Ytrain2); Ytrain2_sd <- sd(Ytrain2)
  Ytrain2_standard <- (Ytrain2 - Ytrain2_mean)/Ytrain2_sd
  
  df$week_num <- lubridate::as.duration(df$week - min(df$week)) / lubridate::as.duration(lubridate::weeks(1))
  
  Xtrain1 <- X[df$train1,]
  Xtrain2 <- X[df$train2,]
  Xval    <- X[df$val,]
  Xtest   <- X[df$test,]
  Xfore   <- X[df$fore,]
  
  week_num_train1 <- df$week_num[df$train1]
  week_num_train2 <- df$week_num[df$train2]
  week_num_val    <- df$week_num[df$val]
  week_num_test   <- df$week_num[df$test]
  week_num_fore   <- df$week_num[df$fore]
  week_train1     <- df$week[df$train1]
  week_train2     <- df$week[df$train2]
  week_val        <- df$week[df$val]
  week_test       <- df$week[df$test]
  week_fore       <- df$week[df$fore]
  
  # prep splines
  knots <- 25
  B <- bs(lags, knots=knots, degree=3, intercept=TRUE)
  n_lags <- dim(B)[1]
  n_splines <- dim(B)[2]
  
  # ground truth deliveries (whole years)
  D2018 <- as.numeric(df %>% filter(lubridate::year(week) == "2018") %>% summarize(delivered = sum(delivered)))
  D2019 <- as.numeric(df %>% filter(lubridate::year(week) == "2019") %>% summarize(delivered = sum(delivered)))
  D2020 <- as.numeric(df %>% filter(lubridate::year(week) == "2020") %>% summarize(delivered = sum(delivered)))
  D2021 <- as.numeric(df %>% filter(lubridate::year(week) == "2021") %>% summarize(delivered = sum(delivered)))
  # ground truth deliveries (partial years for forecasting 2021)
  n_2021_weeks <- sum(week_fore >= as.POSIXct("2021-01-01"))
  F2018 <- as.numeric(df %>% filter(lubridate::year(week) == "2018" & lubridate::week(week) < n_2021_weeks) %>% summarize(delivered = sum(delivered)))
  F2019 <- as.numeric(df %>% filter(lubridate::year(week) == "2019" & lubridate::week(week) < n_2021_weeks) %>% summarize(delivered = sum(delivered)))
  F2020 <- as.numeric(df %>% filter(lubridate::year(week) == "2020" & lubridate::week(week) < n_2021_weeks) %>% summarize(delivered = sum(delivered)))
  
  rm(ci_del, ci_pre, df2, n2, nNA)
}

{
  model_list <- c("intercept", "linear_trend", "quadratic_trend", "annual_trend",
                  "linear_annual_trend", "quadratic_annual_trend",
                  "intercept_AR", "annual_trend_AR", "linear_annual_trend_AR", "quadratic_annual_trend_AR", 
                  "lag_splines_linear_annual_trend", "lag_splines_linear_annual_trend_AR", "lag_splines", "lag_regression")
  model_name_list <- c("Mean Deliveries", "Linear Deliveries", "Quadratic Deliveries", "Annual Trend Deliveries",
                       "Linear w/ Annual Trend Deliveries", "Quadratic w/ Annual Trend Deliveries", 
                       "Autoregressive Deliveries", "Annual Trend Autoregressive Deliveries", "Linear w/ Annual Trend Autoregressive Deliveries", "Quadratic w/ Annual Trend Autoregressive Deliveries",
                       "Screenings Regression with Linear & Annual Trend Deliveries", "Screenings Regression with Linear & Annual Trend Autoregressive Deliveries", "Screenings Regression",
                       "Screenings Regression No Splines")
  df0 <- df
}

for(i in seq_along(model_list)){
  # prelim
  print(i)
  model <- model_list[i]
  model_name <- model_name_list[i]
  if(!dir.exists(file.path(out_dir, "modeling_final", model))){dir.create(file.path(out_dir, "modeling_final", model))}
  print(model_name)
  df <- df0
  
  # set appropriate training data
  if(grepl("lag", model)){
    Xtrain <- Xtrain2
    Ytrain_standard <- Ytrain2_standard
    Ytrain_mean <- Ytrain2_mean
    Ytrain_sd <- Ytrain2_sd
    n_train <- n_train2
    week_num_train = week_num_train2
    week_train = week_train2
    train_idx <- df$train2
  } else {
    Xtrain <- Xtrain1
    Ytrain_standard <- Ytrain1_standard
    Ytrain_mean <- Ytrain1_mean
    Ytrain_sd <- Ytrain1_sd
    n_train <- n_train1
    week_num_train = week_num_train1
    week_train = week_train1
    train_idx <- df$train1
  }
  Yval_standard <- (Yval - Ytrain_mean)/Ytrain_sd  # standardized version of Yval
  
  # compile model
  sm <- stan_model(file.path("stan_files", paste0(model, ".stan")))
  
  ### Validation Run: train on train set, test on validation set, forecasts don't matter.
  data <- list(n_train=n_train, n_test=n_val, n_fore=n_fore, n_splines=n_splines, n_lags=n_lags,
               Y=Ytrain_standard, X=Xtrain, X_test=Xval, X_fore=Xfore, B=B,
               week_train=week_num_train, week_test=week_num_val, week_fore=week_num_fore,
               ar_k = 3)
  # fit and save model
  fit_val <- sampling(sm, data=data, iter=iters, cores=cores, chains=chains)
  saveRDS(fit_val, file.path(out_dir, "modeling_final", model, "fit_val.rds"))
  write.csv(summary(fit_val)$summary, file=file.path(out_dir, "modeling_final", model, "summary_val.csv"))
  write.csv(summary(fit_val)$c_summary, file=file.path(out_dir, "modeling_final", model, "c_summary_val.csv"))
 
  # extract draws for generated quantities (we don't care about the forecasts)
  list_of_draws <- rstan::extract(fit_val)
  Yhs_val <- list_of_draws[["y_hat"]]   * Ytrain_sd + Ytrain_mean 
  Yts_val <- list_of_draws[["y_tilde"]] * Ytrain_sd + Ytrain_mean
  Yh_val <- colMeans(Yhs_val)
  Yt_val <- colMeans(Yts_val)
  
  
  df$fit_val_train <- df$fit_val <- NA
  df$fit_val_train[train_idx] <- Yh_val
  df$fit_val[df$val] <- Yt_val
  
  ### Test Run: train on train + validation set, test on test set, and do forecast
  data <- list(n_train=n_train+n_val, n_test=n_test, n_fore=n_fore, n_splines=n_splines, n_lags=n_lags,
               Y=c(Ytrain_standard, Yval_standard), X=rbind(Xtrain, Xval), X_test=Xtest, X_fore=Xfore, B=B,
               week_train=c(week_num_train, week_num_val), week_test=week_num_test, week_fore=week_num_fore,
               ar_k = 3)
  # fit and save model
  fit_test <- sampling(sm, data=data, iter=iters, cores=cores, chains=chains)
  saveRDS(fit_test, file.path(out_dir, "modeling_final", model, "fit_test.rds"))
  write.csv(summary(fit_test)$summary, file=file.path(out_dir, "modeling_final", model, "summary_test.csv"))
  write.csv(summary(fit_test)$c_summary, file=file.path(out_dir, "modeling_final", model, "c_summary_test.csv"))
 
  # extract draws for generated quantities (we don't care about the forecasts)
  list_of_draws <- rstan::extract(fit_test)
  Yhs_test <- list_of_draws[["y_hat"]]   * Ytrain_sd + Ytrain_mean 
  Yts_test <- list_of_draws[["y_tilde"]] * Ytrain_sd + Ytrain_mean
  Yfs_test <- list_of_draws[["y_fore"]]  * Ytrain_sd + Ytrain_mean
  Yh_test <- colMeans(Yhs_test)
  Yt_test <- colMeans(Yts_test)
  Yf_test <- colMeans(Yfs_test)
  
  df$fit_test_train <- df$fit_test <- df$fit_test_fore <- NA
  df$fit_test_train[train_idx | df$val] <- Yh_test
  df$fit_test[df$test] <- Yt_test
  df$fit_test_fore[df$fore] <- Yf_test
  
  mad_test <- sum(abs(Yt_test - Ytest)/n_test)
  
  # reconstructed deliveries (2020) and forecasted deliveries (2021) compared to previous years
  R2020 <- sum(Yt_test[lubridate::year(week_test) == "2020"])
  R2020s <- rowSums(Yfs_test[,lubridate::year(week_fore) == "2021"])
  F2021 <- sum(Yf_test[lubridate::year(week_fore) == "2021"])
  write.table(quantile(R2020s, c(0.025, 0.05, 0.1, 0.5, 0.9, 0.95, 0.975)), file.path(out_dir, "modeling_final", model, "forecast_CI.txt"))
  df_preds <- data.frame(year = c("2018", "2019", "2020", "2021YTD"), deliveries=c(D2018, D2019, D2020, D2021), recon=c(NA, NA, R2020, NA), forecast=c(NA, NA, NA, F2021))
  df_preds[[paste0("first_", n_2021_weeks, "_weeks")]] <- as.vector(c(F2018, F2019, F2020, NA))
  write.csv(df_preds, file.path(out_dir, "modeling_final", model, "forecasts.csv"))
  
  df_runs <- rbind(
    as.data.frame(t(Yhs_val)) %>% mutate(week = week_train, label="val_train") %>% pivot_longer(starts_with("V"), names_to="run", values_to="delivered"),
    as.data.frame(t(Yts_val)) %>% mutate(week = week_val,   label="val")       %>% pivot_longer(starts_with("V"), names_to="run", values_to="delivered"),
    as.data.frame(t(Yhs_test)) %>% mutate(week = c(week_train, week_val), label="test_train") %>% pivot_longer(starts_with("V"), names_to="run", values_to="delivered"),
    as.data.frame(t(Yts_test)) %>% mutate(week = week_test, label = "test")                   %>% pivot_longer(starts_with("V"), names_to="run", values_to="delivered"),
    as.data.frame(t(Yfs_test)) %>% mutate(week = week_fore, label = "test_fore")              %>% pivot_longer(starts_with("V"), names_to="run", values_to="delivered")
  )
  
  # save data
  saveRDS(df, file.path(out_dir2, paste0("df_", model, ".rds")))
  saveRDS(df_runs, file.path(out_dir, "modeling_final", model, paste0("df_runs_", model, ".rds")))
}

