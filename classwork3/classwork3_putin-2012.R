# считываем данные
msg <- readLines('C:\\Users\\marku\\Desktop\\new\\Методы анализа данных\\classwork3\\putin-2012.txt')

#посчитаем количество символов
length(msg)
l<-lapply(msg, nchar)
Reduce(sum,l)

txt <- paste(msg, collapse = " ")
nchar(txt)

# количество слов, посчитанных в первый раз с ошибками
r <- grep('мы',msg, ignore.case = TRUE)
r

words <-unlist(strsplit(txt, ' '))
#Количество слов
length(words)
# статистика слов
wc <- table(words)
wc <- sort(wc, decreasing = TRUE)
head(wc,20)

# необходима перекодировка, поскольку
# буквы «ч» и «ё» распознаются как символы, не принадлежащие алфавиту,
# и удаляются при очистке текста вместе со знаками пунктуации.
words <- enc2utf8(words)

# Приводим все буквы в нижний регистр
words <- tolower(words)

# Установим библиотеку для работы с текстом
# install.packages("tm")
# library(tm)

# Удаляем союзы, предлоги, частицы и прочее
stopWord <- c('к','и','в','от','на','у','для','если','с', 'я', 'по')
words <- removeWords(words, stopWord)

# Удаляем пустые значения
words <- words[words != ""]

# Удаляем знаки пунктуации
words <- gsub("[[:punct:]]+","", words)


# Количество слов
length(words)

# таблица самых часто встречающихся слов
wc <- table(words)
wc <- sort(wc, decreasing = TRUE)
head(wc, 20)
