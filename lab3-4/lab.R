data <- read.csv("https://archive.ics.uci.edu/ml/machine-learning-databases/abalone/abalone.data", sep=",")

#  Name / Data Type / Measurement Unit / Description
#  -----------------------------
#  Sex / nominal / -- / M, F, and I (infant)
#  Length / continuous / mm / Longest shell measurement
#  Diameter / continuous / mm / perpendicular to length
#  Height / continuous / mm / with meat in shell
#  Whole weight / continuous / grams / whole abalone
#  Shucked weight / continuous / grams / weight of meat
#  Viscera weight / continuous / grams / gut weight (after bleeding)
#  Shell weight / continuous / grams / after being dried
#  Rings / integer / -- / +1.5 gives the age in years

colnames(data) <- c("sex", "length", "diameter", "height",
                    "whole_weight", "shucked_weight",
                    "viscera_weight", "shell_weight", "rings")

data$sex <- as.factor(data$sex)

summary(data)


# Ищем выбросы 
par(mfrow=c(1,4))

boxplot((data$length), main = "Длина", ylab = "Длина")
boxplot((data$diameter), main = "Диаметр, мм", ylab = "Диаметр")
boxplot((data$height), main = "Высота", ylab = "Высота")
boxplot((data$whole_weight), main = "Вес", ylab = "Полный вес")


par(mfrow=c(1,3))

hist(data$diameter, main = "Диаметр, мм", ylab = "Index", xlab = "Диаметр")
hist(data$height, main = "Высота, мм", ylab = "Index", xlab = "Высота")
hist(data$whole_weight, main = "Полный вес, гр", ylab = "Index", xlab = "Вес")

#data.noout - данные без выбросов

data.noout <- data[data$length > 0.22 &
                   data$diameter > 0.15 &
                   data$height > 0.05 & data$height < 0.22 &
                   data$whole_weight < 2.1,]

summary(data.noout)


par(mfrow=c(1,4))

boxplot((data.noout$length), main = "Длина", ylab = "Длина")
boxplot((data.noout$diameter), main = "Диаметр, мм", ylab = "Диаметр")
boxplot((data.noout$height), main = "Высота", ylab = "Высота")
boxplot((data.noout$whole_weight), main = "Вес", ylab = "Полный вес")


plot (sort(data.noout$length), main = "Длина", ylab = "Длина")
plot (sort(data.noout$diameter), main = "Диаметр, мм", ylab = "Диаметр")
plot (sort(data.noout$height), main = "Высота", ylab = "Высота")
plot (sort(data.noout$whole_weight), main = "Вес", ylab = "Полный вес")


#Визуализируем возможные зависимости
par(mfrow=c(1,3))

plot(data.noout$length, data.noout$whole_weight,'p',
     main = "Зависимость веса от длины", 
     ylab = "Полный вес", xlab = "Длина")

plot(data.noout$diameter, data.noout$whole_weight,'p',
     main = "Зависимость веса от диаметра", 
     ylab = "Полный вес", xlab = "Диаметр")

plot(data.noout$height, data.noout$whole_weight,'p',
     main = "Зависимость веса от высоты", 
     ylab = "Полный вес", xlab = "Высота")


# Зависимости веса от длины и диаметра наиболее заметные - их и будем тестировать 
t.test(data.noout$whole_weight, data.noout$length)

t.test(data.noout$whole_weight, data.noout$diameter)

par(mfrow=c(1,4))
# Строим линейные зависимости

# Зависимость диаметра от веса
lm_diameter_weight <- lm (whole_weight~diameter, data=data.noout)
plot(lm_diameter_weight)
summary(lm_diameter_weight)

# Зависимость веса от длины
lm_weight_length <- lm(whole_weight~length, data = data.noout)
plot(lm_weight_length)
summary(lm_weight_length)

# Делим массив данных на 2
odds <- seq(1, nrow(data.noout), by=2)
data.train <- data.noout[odds,]
data.test <- data.noout[-odds,]

# Создаем модель для прогноза
linear.model.half <- lm (whole_weight ~ length, data=data.train)
summary (linear.model.half)

par(mfrow=c(1,2))

# Прогноз 
data_predict <- predict (linear.model.half)
cor (data.train$whole_weight, data_predict)
plot (data.train$whole_weight, data_predict, main = "Тренировочные данные")

data_predict_out <- predict(linear.model.half, data.test)
cor(data.test$whole_weight, data_predict_out)
plot (data.train$whole_weight, data_predict_out, main = "Тестовые данные")