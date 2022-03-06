{
  rm(list=ls()); gc()
  
  out_dir <- "output"
  out_dir2 <- "output2"
  
  library(dplyr)
  library(tidyr)
  library(ggplot2)
  theme_set(theme_bw())
  library(ggpubr)
  library(gridExtra)
  library(lubridate)
  library(scales)
  library(ggpmisc)
}

################################################################################
## Load model output

model_list <- c("intercept", "linear_trend", "quadratic_trend", "annual_trend",
                "linear_annual_trend", "quadratic_annual_trend",
                "intercept_AR", "annual_trend_AR", "linear_annual_trend_AR", "quadratic_annual_trend_AR", 
                "lag_splines_linear_annual_trend", "lag_splines_linear_annual_trend_AR", "lag_splines", "lag_regression")
model_name_list <- c("Mean Deliveries", "Linear Deliveries", "Quadratic Deliveries", "Annual Trend Deliveries",
                     "Linear w/ Annual Trend Deliveries", "Quadratic w/ Annual Trend Deliveries", 
                     "Autoregressive Deliveries", "Annual Trend Autoregressive Deliveries", "Linear w/ Annual Trend Autoregressive Deliveries", "Quadratic w/ Annual Trend Autoregressive Deliveries",
                     "Screenings Regression with Linear & Annual Trend Deliveries", "Screenings Regression with Linear & Annual Trend Autoregressive Deliveries", "Screenings Regression",
                     "Screenings Regression No Splines")

dfs <- list(); for(i in seq_along(model_list)){
  model <- model_list[i]
  model_name <- model_name_list[i]
  dfs[[i]] <- readRDS(file.path(out_dir2, paste0("df_", model, ".rds"))) %>% mutate(model=as.factor(model), model_name=as.factor(model_name))
}; df <- do.call(rbind, dfs); rm(dfs)

dfqs <- list(); for(i in seq_along(model_list)){
  model <- model_list[i]
  model_name <- model_name_list[i]
  dfqs[[i]] <- readRDS(file.path(out_dir, "modeling_final", model, paste0("df_runs_", model, ".rds"))) %>% mutate(model=as.factor(model)) %>%
    group_by(week, label, model) %>%
    summarize(q025 = quantile(delivered, 0.025),
              q050 = quantile(delivered, 0.050),
              q100 = quantile(delivered, 0.100),
              q250 = quantile(delivered, 0.250),
              q500 = quantile(delivered, 0.500),
              q750 = quantile(delivered, 0.750),
              q900 = quantile(delivered, 0.900),
              q950 = quantile(delivered, 0.950),
              q975 = quantile(delivered, 0.975))
}; dfq <- do.call(rbind, dfqs); rm(dfqs)

df2 <- df %>% 
  left_join(dfq %>% pivot_wider(id_cols=c(week, model), names_from=label, values_from=q050, names_prefix="q050_")) %>%
  left_join(dfq %>% pivot_wider(id_cols=c(week, model), names_from=label, values_from=q950, names_prefix="q950_")) %>%
  left_join(dfq %>% pivot_wider(id_cols=c(week, model), names_from=label, values_from=q500, names_prefix="q500_"))
df2 <- df2 %>%
  mutate(name2 = case_when(
                           model == "intercept"       ~ "Mean Births",
                           model == "linear_trend"    ~ "Linear Trend",
                           model == "quadratic_trend" ~ "Quadratic Trend",
                           model == "annual_trend"    ~ "Seasonal Trend",
                           model == "linear_annual_trend"       ~ "Long Term & Seasonal Trends",
                           model == "quadratic_annual_trend"    ~ "Quadratic & Seasonal Trend",
                           model == "intercept_AR"    ~ "Mean Births w/ Autoregression",
                           model == "annual_trend_AR"    ~ "Seasonal Trend w/ Autoregression",
                           model == "linear_annual_trend_AR"    ~ "Long Term & Seasonal Trends w/ Autoregression",
                           model == "quadratic_annual_trend_AR" ~ "Quadratic & Seasonal Trends w/ Autoregression",
                           model == "lag_splines"     ~ "Screenings Regression",
                           model == "lag_splines_linear_annual_trend"   ~ "Long Term & Seasonal Trends w/ Screenings Regression",
                           model == "lag_splines_linear_annual_trend_AR" ~ "Long Term & Seasonal Trends w/ Autoregression & Screenings Regression",
                           model == "lag_regression"  ~ "Screenings Regression (No Splines)"),
         name2 = as.factor(name2))

# compute statistics
df2 <- df2 %>% 
  mutate(dev_val_train  = delivered - fit_val_train, 
         dev_val        = delivered - fit_val, 
         dev_test_train = delivered - fit_test_train,
         dev_test       = delivered - fit_test,
         qdev_val_train = delivered - q500_val_train, 
         qdev_val       = delivered - q500_val, 
         qdev_test_train= delivered - q500_test_train,
         qdev_test      = delivered - q500_test)

saveRDS(df2, file.path(out_dir2, "df2.rds"))
df2 <- readRDS(file.path(out_dir2, "df2.rds"))

mlist = model_name_list[c(5, 9, 11, 12, 13)]; mlist

stats <- df2 %>% 
  group_by(model, model_name, name2) %>%
  summarize(MAD_val_train =       mean(abs(qdev_val_train), na.rm=T),
            MAD_val =             mean(abs(qdev_val), na.rm=T),
            MAD_test_train =      mean(abs(qdev_test_train), na.rm=T),
            MAD_test =            mean(abs(qdev_test), na.rm=T),
            tot_err_val_train =   sum(qdev_val_train, na.rm=T),
            tot_err_val =         sum(qdev_val, na.rm=T),
            tot_err_test_train =  sum(qdev_test_train, na.rm=T),
            tot_err_test =        sum(qdev_test, na.rm=T)) %>%
  arrange(abs(tot_err_val))
View(stats %>% select(model_name, MAD_val, tot_err_val))
write.csv(stats, file.path(out_dir, "modeling_final", "model_stats.csv"))

t1 <- min(df2$week) - hours(12)
t2 <- max(df2$week[df2$train1]) + days(1) + hours(12)
t3 <- max(df2$week[df2$val]) + days(3) + hours(12)
t4 <- max(df2$week[df2$test])
t5 <- max(df2$week[df2$fore])
t6 <- as.POSIXct("2022-01-01") - hours(12)

dfqm <- readRDS(file.path(out_dir, "modeling_final", model, paste0("df_runs_", model, ".rds"))) %>%
  mutate(run_num = as.numeric(substr(run, 2, length(run))))

################################################################################
### Create figure for paper

df3 <- df2 %>% filter(model_name %in% mlist)
stats3 <- df3 %>% 
  group_by(model, model_name, name2) %>%
  summarize(MAD_val_train =       mean(abs(qdev_val_train), na.rm=T),
            MAD_val =             mean(abs(qdev_val), na.rm=T),
            MAD_test_train =      mean(abs(qdev_test_train), na.rm=T),
            MAD_test =            mean(abs(qdev_test), na.rm=T),
            tot_err_val_train =   sum(qdev_val_train, na.rm=T),
            tot_err_val =         sum(qdev_val, na.rm=T),
            tot_err_test_train =  sum(qdev_test_train, na.rm=T),
            tot_err_test =        sum(qdev_test, na.rm=T)) %>%
  arrange(abs(tot_err_val))

stats3$name2 <- droplevels(stats3$name2)
df3$name2 <- droplevels(df3$name2)
levs <- c(
  "Screenings Regression", 
  "Long Term & Seasonal Trends w/ Autoregression & Screenings Regression",
  "Long Term & Seasonal Trends w/ Screenings Regression",
  "Long Term & Seasonal Trends w/ Autoregression",
  "Long Term & Seasonal Trends"
  )
stats3$name2 <- factor(as.character(stats3$name2), levels=levs)
df3$name2 <- factor(as.character(df3$name2), levels=levs)

{
  cols <- rev(scales::hue_pal()(length(mlist))); cols <- cols[c(2, 1, 4, 3, 5)]
  scales::show_col(cols)
  
  
  # create table grobs to add to plots
  val_tab <- stats3 %>% ungroup() %>%
    select(name2, MAD_val, tot_err_val) %>%
    rename(Model = name2,
           MAD = MAD_val,
           `Total Error` = tot_err_val) %>%
    mutate(MAD = round(MAD, 1),
           `Total Error` = -round(`Total Error`, 1)) %>%
    arrange(as.numeric(Model)); val_tab
  test_tab <- stats3 %>% ungroup() %>% 
    select(name2, MAD_test, tot_err_test) %>%
    rename(Model = name2,
           MAD = MAD_test,
           `Total Error` = tot_err_test) %>%
    mutate(MAD = round(MAD, 1),
           `Total Error` = -round(`Total Error`, 1)) %>%
    arrange(as.numeric(Model)); test_tab
  
  theme_set(theme_bw(base_size=18))
  s1 <- 1.0; s2 <- 1.5; s3 <- 1.5
  c3 <- "grey80"
  sty <- "21"
  c4 <- "#F8766D"
  s4 <- 7; s5 <- 6; a <- 30
  madx <- t2 %m+% months(4)
  taby <- 180
  dy <- 10
  terx <- t2 %m+% months(8)
  hj <- 0.5
  madx2 <- t3 %m+% months(2)
  terx2 <- t3 %m+% months(6)
  taby2 <- 170
  al <- 1.0
}
p1 <- df3 %>%
  #mutate(name2 = factor(name2, rev(levels(name2)))) %>%
  ggplot(aes(week)) + 
  geom_vline(xintercept=as.POSIXct("2017-12-31 12:00:00"), color=c3, size=s3) + 
  geom_vline(xintercept=as.POSIXct("2018-12-31 12:00:00"), color=c3, size=s3) + 
  geom_vline(xintercept=as.POSIXct("2019-12-31 12:00:00"), color=c3, size=s3) + 
  geom_vline(xintercept=as.POSIXct("2020-12-31 12:00:00"), color=c3, size=s3) + 
  geom_vline(xintercept=as.POSIXct("2021-12-31 12:00:00"), color=c3, size=s3) + 
  annotate("errorbar", xmin=t1, xmax=t2, y=245, size=2.0, width=10) + 
  annotate("errorbar", xmin=t2+days(2), xmax=t3, y=245, size=2.0, width=10) + 
  annotate("text", x = as.POSIXct((as.numeric(t1) + as.numeric(t2))/ 2, origin = '1970-01-01'), y=257, label="Train", size=s4) + 
  annotate("text", x = as.POSIXct((as.numeric(t2) + as.numeric(t3))/ 2, origin = '1970-01-01'), y=257, label="Validate", size=s4) + 
  geom_line(aes(week, delivered), size=s1, color="grey60", linetype=sty) + 
  geom_line(aes(y=q500_val_train, color=name2), size=s2, alpha=al) + 
  geom_line(aes(y=q500_val, color=name2), size=s2, alpha=al) + 
  annotate("text", x=madx, y=taby, label="MAD", size=s5, fontface=2, hjust=hj) +
  annotate("text", x=madx, y=taby-5*dy, label=val_tab$MAD[1], color=cols[1], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=madx, y=taby-4*dy, label=val_tab$MAD[2], color=cols[2], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=madx, y=taby-3*dy, label=val_tab$MAD[3], color=cols[3], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=madx, y=taby-2*dy, label=val_tab$MAD[4], color=cols[4], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=madx, y=taby-1*dy, label=val_tab$MAD[5], color=cols[5], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=terx, y=taby, label="Total Error", size=s5, fontface=2, hjust=hj) +
  annotate("text", x=terx, y=taby-5*dy, label=val_tab$`Total Error`[1], color=cols[1], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=terx, y=taby-4*dy, label=val_tab$`Total Error`[2], color=cols[2], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=terx, y=taby-3*dy, label=val_tab$`Total Error`[3], color=cols[3], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=terx, y=taby-2*dy, label=val_tab$`Total Error`[4], color=cols[4], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=terx, y=taby-1*dy, label=val_tab$`Total Error`[5], color=cols[5], size=s5, fontface=2, hjust=hj) +
  scale_x_datetime(limits=c(t1, t6), date_breaks="3 months", date_minor_breaks="1 month", date_labels="%Y\n%b") +
  scale_color_manual(values=cols) + 
  scale_y_continuous(limits=c(130, 260)) + 
  labs(title="", x="", y="Weekly Deliveries", color="Model") + 
  theme(legend.position="bottom", plot.margin=margin(0, 0.1, 0, 0.1, "cm"), axis.text.x=element_text(angle=0, hjust=0.5)) + 
  guides(color=guide_legend(nrow=3,byrow=F)); p1
p2 <- df2 %>%
  filter(model_name %in% mlist) %>%
  filter(model_name == model_name_list[5]) %>%
  ggplot(aes(week)) + 
  geom_vline(xintercept=as.POSIXct("2017-12-31 12:00:00"), color=c3, size=s3) + 
  geom_vline(xintercept=as.POSIXct("2018-12-31 12:00:00"), color=c3, size=s3) + 
  geom_vline(xintercept=as.POSIXct("2019-12-31 12:00:00"), color=c3, size=s3) + 
  geom_vline(xintercept=as.POSIXct("2020-12-31 12:00:00"), color=c3, size=s3) + 
  geom_vline(xintercept=as.POSIXct("2021-12-31 12:00:00"), color=c3, size=s3) + 
  annotate("errorbar", xmin=t1, xmax=t3, y=245, size=2.0, width=10) + 
  annotate("errorbar", xmin=t3+days(2), xmax=t4, y=245, size=2.0, width=10) + 
  annotate("errorbar", xmin=t4+days(2), xmax=t6, y=245, size=2.0, width=10) + 
  annotate("text", x = as.POSIXct((as.numeric(t1) + as.numeric(t3))/ 2, origin = '1970-01-01'), y=257, label="Retrain", size=s4) + 
  annotate("text", x = as.POSIXct((as.numeric(t3) + as.numeric(t4))/ 2, origin = '1970-01-01'), y=257, label="Test", size=s4) + 
  annotate("text", x = as.POSIXct((as.numeric(t4) + as.numeric(t6))/ 2, origin = '1970-01-01'), y=257, label="Forecast", size=s4) + 
  geom_line(aes(week, delivered), size=s1, color="grey60", linetype=sty) + 
  geom_line(aes(y=q500_test_train), color=c4, size=s2, alpha=al) + 
  geom_line(aes(y=q500_test_fore), color=c4, size=s2, alpha=al) + 
  geom_ribbon(aes(ymin=q050_test_fore, ymax=q950_test_fore), color=c4, alpha=0.1) + 
  geom_ribbon(aes(ymin=q050_test_train, ymax=q950_test_train), color=c4, alpha=0.1) + 
  annotate("text", x=madx2, y=taby2, label="MAD", size=s5, fontface=2, hjust=hj) +
  annotate("text", x=madx2, y=taby2-1*dy, label=test_tab$MAD[1], color=cols[5], size=s5, fontface=2, hjust=hj) +
  annotate("text", x=terx2, y=taby2, label="Total Error", size=s5, fontface=2, hjust=hj) +
  annotate("text", x=terx2, y=taby2-1*dy, label=test_tab$`Total Error`[5], color=cols[5], size=s5, fontface=2, hjust=hj) +
  scale_x_datetime(limits=c(t1, t6), date_breaks="3 months", date_minor_breaks="1 month", date_labels="%Y\n%b") +
  scale_y_continuous(limits=c(150, 260)) + 
  labs(title="", x="Week", y="Weekly Deliveries", color="Model") + 
  theme(legend.position="bottom", plot.margin=margin(0, 0.1, 0, 0.1, "cm"), axis.text.x=element_text(angle=0, hjust=0.5)); p2
ggarrange(p1, p2, nrow=2, labels=c("Model Selection", "Forecasting"), hjust=c(-0.365,-0.48), common.legend=T, legend="bottom", 
          font.label=list(size = 20, color = "black", face = "bold", family = NULL))
ggsave(file.path(out_dir, "modeling_final", "model_selection_forecasting_figure_test.png"), height=12, width=15, units="in")


################################################################################
### Get credible intervals for 2021 YTD and total year


model <- model_list[5]
dfqm <- readRDS(file.path(out_dir, "modeling_final", model, paste0("df_runs_", model, ".rds")))


head(dfqm)
table(dfqm$label)
fcs <- dfqm %>% 
  filter(week >= as.POSIXct("2021-01-01"), week <= as.POSIXct("2021-09-20"), label=="test_fore") %>%
  group_by(run) %>% summarize(delivered=sum(delivered))
quantile(fcs$delivered, c(0.05, 0.5, 0.95))

fcs2 <- dfqm %>% 
  filter(week >= as.POSIXct("2021-01-01"), week <= as.POSIXct("2021-12-31"), label=="test_fore") %>%
  group_by(run) %>% summarize(delivered=sum(delivered))
quantile(fcs2$delivered, c(0.05, 0.5, 0.95))

