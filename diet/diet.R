library(gplots) #библиотека устанавлевается с помощью install.packages

#Дисперсионный анализ. Пример

#Загрузим данные (требуется установить Рабочую папку с помощью setwd) или указать полный путь
data = read.csv("diet/diet.csv",row.names=1)
summary(data)
#Ознакомимся со структурой и переименуем колонки, как нам удобно

colnames(data) <- c("gender", "age", "height", "initial.weight", 
                    "diet.type", "final.weight")

data$diet.type <- factor(c("A", "B", "C")[data$diet.type])

#Добавим новую колонку - Похудение
data$weight.loss = data$initial.weight - data$final.weight

summary(data)

#Ищем выбросы
par(mfrow=c(1,3))
boxplot((data$height), main = "Рост", ylab = "")
boxplot((data$initial.weight), main = "Начальный вес", ylab = "")
boxplot((data$final.weight), main = "Итоговый вес", ylab = "")

#Находим выбросы
height_out <- boxplot.stats(data$height)$out
initial_weight_out <- boxplot.stats(data$initial.weight)$out
final_weight_out <- boxplot.stats(data$final.weight)$out

#Избавимся от выбросов
data <- data[!(data$height %in% height_out) & (data$height < 185) &
                   !(data$initial.weight %in% initial_weight_out) &
                   !(data$final.weight %in% final_weight_out)
              ,]

#Проверка
summary(data)

boxplot((data$height), main = "Рост", ylab = "")
boxplot((data$initial.weight), main = "Начальный вес", ylab = "")
boxplot((data$final.weight), main = "Итоговый вес", ylab = "")

#Найдем среднюю потерю веса
par(mfrow=c(1,1))
boxplot((data$weight.loss), main = "Потеря веса", ylab = "Минус кг")

#Проанализиуем есть ли различия по типам диет
par(mfrow=c(1,1))
boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#Удалим выбросы
data<-data[data$weight.loss<=8,]

boxplot(weight.loss~diet.type,data=data,col="light gray",
        ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

#Проверим сбалансированные ли данные
table(data$diet.type)

#График групповых средних
plotmeans(weight.loss ~ diet.type, data=data)
aggregate(data$weight.loss, by = list(data$diet.type), FUN=sd)

#Для подгонки ANOVA модели используем функцию aov, частный случай линейной модели lm
#Тест на межгрупповые различия
data_with_aov <- aov(weight.loss ~ diet.type, data=data)
summary(data_with_aov)

#Попарные различия между средними значениями для всех групп
TukeyHSD(data_with_aov)

#Проведем тест Тьюки на существенные различия
library(multcomp)
par(mar=c(5,4,6,2))
tuk <- glht(data_with_aov, linfct=mcp(diet.type="Tukey"))
#Видим, что диеты A и B не отличаются друг от друга
plot(cld(tuk, level=.05),col="lightgrey")

#Графики потери веса у мужчин и у женщин
par(mfrow=c(1,2))

data_male <- data[data$gender == 1 & data$weight.loss > -1, ]
boxplot(weight.loss~diet.type,data=data_male,col="light gray",
        main = "Потеря веса у мужчин", ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

summary(data_male$weight.loss)

data_female <- data[data$gender == 0 & data$weight.loss > -1, ]
boxplot(weight.loss~diet.type,data=data_female,col="light gray",
        main = "Потеря веса у женщин", ylab = "Weight loss (kg)", xlab = "Diet type")
abline(h=0,col="green")

summary(data_female$weight.loss)

data_male_with_aov <- aov(weight.loss ~ diet.type, data=data_male)
summary(data_male_with_aov)

data_female_with_aov <- aov(weight.loss ~ diet.type, data=data_female)
summary(data_female_with_aov)

#Поиск зависимости потери веса от типа диеты и пола.
plotmeans(weight.loss ~ diet.type, data=data_male, main = "Потеря веса у мужчин")
aggregate(data_male$weight.loss, by = list(data_male$diet.type), FUN=sd)

plotmeans(weight.loss ~ diet.type, data=data_female, main = "Потеря веса у женщин")
aggregate(data_female$weight.loss, by = list(data_female$diet.type), FUN=sd)

#Поиск возможной зависимости потери веса от роста
data_diet_a <- data[data$diet.type == "A", ]
data_diet_b <- data[data$diet.type == "B", ]
data_diet_c <- data[data$diet.type == "C", ]

par(mfrow=c(1,1))
#График групповых средних по диете А
plotmeans(weight.loss ~ height, data=data_diet_a)
data_diet_a_with_aov <- aov(weight.loss ~ height, data=data_diet_a)
summary(data_diet_a_with_aov)

#График групповых средних по диете В
plotmeans(weight.loss ~ height, data=data_diet_b)
data_diet_b_with_aov <- aov(weight.loss ~ height, data=data_diet_b)
summary(data_diet_b_with_aov)

#График групповых средних по диете С
plotmeans(weight.loss ~ height, data=data_diet_c)
data_diet_c_with_aov <- aov(weight.loss ~ height, data=data_diet_c)
summary(data_diet_c_with_aov)

#Проанализируем зависимость похудения от диеты, пола, роста, возраста
fit <- aov(weight.loss ~ diet.type + height + age, data = data)
summary(fit)
fit <- aov(weight.loss ~ diet.type + gender + height, data = data)
summary(fit)
fit <- aov(weight.loss ~ diet.type + gender + height + age, data = data)
summary(fit)

#Значения Pr(>F) у всех параметров кроме типа диеты большие
#Вывод: на похудение влияет только тип диеты