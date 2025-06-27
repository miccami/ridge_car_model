# Загрузите библиотеки
install.packages("tidyverse")
install.packages("broom.mixed")

library(tidyverse)
library(broom.mixed)
library(ggplot2)

# Загрузите данные
file_path <- "all_regions.csv"
data <- read.csv("all_regions.csv")

# Подготовьте данные
data <- data %>%
  select(brand, name, year, mileage, transmission, power, price) %>%
  mutate(brand = as.factor(brand),
         name = as.factor(name),
         year = as.numeric(year),
         mileage = as.numeric(mileage),
         transmission = as.factor(transmission),
         power = as.numeric(power),
         price = as.numeric(price))

# Разбейте данные на обучающий и тестовый наборы
set.seed(123)
train_index <- sample(1:nrow(data), 0.75 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

# Разработайте модель линейной регрессии без регуляризации
model_no_ridge <- lm(price ~ ., data = train_data)

# Разработайте модель Ridge
model_ridge <- lm(price ~ ., data = train_data, control = list(ridge = 0.1))

# Оцените коэффициенты для обеих моделей
tidy(model_no_ridge) %>%
  select(term, estimate, std.error)

tidy(model_ridge) %>%
  select(term, estimate, std.error)

# Сравните ошибки кросс-валидации
cv_error_no_ridge <- crossval::crossval(model_no_ridge, train_data, method = "cv") %>%
  mean(std.error)
cv_error_ridge <- crossval::crossval(model_ridge, train_data, method = "cv") %>%
  mean(std.error)

# Вычислите метрики надежности
metrics_no_ridge <- broom.mixed::augment(model_no_ridge, newdata = test_data) %>%
  summarise(r2 = tidy::r2(.pred, .obs))
metrics_ridge <- broom.mixed::augment(model_ridge, newdata = test_data) %>%
  summarise(r2 = tidy::r2(.pred, .obs))

# Сравните результаты
results <- tibble(
  model = c("Без регуляризации", "Ridge"),
  cv_error = c(cv_error_no_ridge, cv_error_ridge),
  r2 = c(metrics_no_ridge$r2, metrics_ridge$r2)
)

results %>%
  pivot_longer(cols = 2:3) %>%
  ggplot(aes(x = model, y = value, fill = model)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name)

# Создайте график сравнения ошибок кросс-валидации
results %>%
  pivot_longer(cols = 2:3) %>%
  ggplot(aes(x = model, y = value, fill = model)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name)

# Создайте график сравнения коэффициентов детерминации
results %>%
  pivot_longer(cols = 2:3) %>%
  ggplot(aes(x = model, y = value, fill = model)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~name)


