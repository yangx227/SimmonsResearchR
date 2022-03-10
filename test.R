
val <- c(0.4761905, 0.4250000, 0.4137931, 0.6000000, 0.5405405, 0.4806202, 0.5288462, 0.5056180, 0.5423729, 0.5492958, 0.5846154, 0.4657534,
         0.5223881, 0.5507246, 0.5657895, 0.5342466, 0.7101449, 0.5128205, 0.7250000, 0.6901408, 0.6351351, 0.5888889, 0.6060606, 0.5764706)

ts(1:24, frequency = 12, start = c(2020, 3))

dat <- tibble::tibble(date = seq(as.Date("2020/3/1"), by = "month", length.out = 24), 
                       value = val)

model1 <- arima_reg(seasonal_period = 12) %>%
  set_engine("auto_arima") %>%
  fit(value ~ date, data = dat)

model2 <- forecast::auto.arima(dat %>% select(value))

model1
model2


fitted1 <- modeltime_table(model1) %>%
  modeltime_forecast(
    new_data = dat,
    actual_data = dat
  ) %>%
  filter(.key == 'prediction') %>%
  pull(.value)


fitted2 <- as.numeric(model2$fitted)


modeltime_table(model1) %>% modeltime_accuracy(new_data = training(splits))

yardstick::rmse_vec(dat$value, fitted1)
yardstick::rmse_vec(dat$value, fitted2)
