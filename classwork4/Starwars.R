#Установить пакеты
#install.packages("tidyverse")
#install.packages("dplyr")
library(dplyr)

# Загрузим тестовый набор данных
data(starwars)

# Прочитайте описание массива данных
?starwars

# Узнайте количество строк и колонок в наборе с помощью функций nrow и summary
starwars %>% nrow
summary(starwars)

###########
#СПРАВКА https://dplyr.tidyverse.org/reference/
##########

#С помощью функции distinct выберите уникальные цвета волос
starwars %>% distinct(hair_color)

#Сгруппируйте по цвету волос и посчитайте сколько всего строк каждого цвета
starwars %>%
  group_by(hair_color) %>%
  summarise(count = n())
          
#Отсортируйте по убыванию то что получили выше
starwars %>%
  group_by(hair_color) %>%
  summarise(count = n()) %>% 
  arrange(desc(count))

#Посчитайте среднюю массу всех представителей
#Необходим параметр na.rm = TRUE, посколько в данных встречаются пропущенные значения
#С помощью этого параметра R игнорирует эти пропущенные значения
starwars %>% summarise(mean = mean(mass, na.rm = TRUE))

#Теперь найдите самого высокого, самого низкого

# Самый высокий
max(starwars$height, na.rm = TRUE)
# Самый низкий
min(starwars$height, na.rm = TRUE)
  
  
#Отфильтруйте их (исключите из выборки самого высокого и самого низкого) и снова посчитайте среднюю масса
starwars %>% 
  filter(!is.na(mass)) %>%
  filter(66 < height & height < 264) %>%
  summarise(mean = mean(mass))
  
#Найдите средний рост жителя каждой из планет
starwars %>% 
  group_by(homeworld) %>%
  summarise(mean = mean(height, na.rm = TRUE))
