##### Установить пакеты
> #install.packages("tidyverse")  
> #install.packages("dplyr")  
> library(dplyr)  
##### Загрузим тестовый набор данных
> data(starwars)
##### Прочитайте описание массива данных
> ?starwars
##### Узнайте количество строк и колонок в наборе с помощью функций nrow и summary
> starwars %>% nrow  
[1] 87  

> summary(starwars)  

##### С помощью функции distinct выберите уникальные цвета волос  
> starwars %>% distinct(hair_color)   

> A tibble: 13 x 1    
   hair_color       
   <chr>            
 1 blond            
 2 NA               
 3 none             
 4 brown            
 5 brown, grey       
 6 black            
 7 auburn, white    
 8 auburn, grey     
 9 white            
10 grey             
11 auburn         
12 blonde          
13 unknown          
   
##### Сгруппируйте по цвету волос и посчитайте сколько всего строк каждого цвета
> starwars %>%    
>   group_by(hair_color) %>%    
>   summarise(count = n())  

> A tibble: 13 x 2    
   hair_color    count    
   <chr>         <int>     
 1 auburn            1     
 2 auburn, grey      1      
 3 auburn, white     1     
 4 black            13      
 5 blond             3      
 6 blonde            1      
 7 brown            18      
 8 brown, grey       1        
 9 grey              1      
10 none             37      
11 unknown           1       
12 white             4      
13 NA                5      
      
##### Отсортируйте по убыванию то что получили выше
> starwars %>%    
>   group_by(hair_color) %>%    
>   summarise(count = n()) %>%     
>   arrange(desc(count))  

> A tibble: 13 x 2    
   hair_color    count    
   <chr>         <int>    
 1 none             37    
 2 brown            18    
 3 black            13    
 4 NA                5    
 5 white             4    
 6 blond             3    
 7 auburn            1    
 8 auburn, grey      1    
 9 auburn, white     1    
10 blonde            1    
11 brown, grey       1    
12 grey              1    
13 unknown           1    
##### Посчитайте среднюю массу всех представителей  
Необходим параметр na.rm = TRUE, посколько в данных встречаются пропущенные значения.  
С помощью этого параметра R игнорирует эти пропущенные значения
> starwars %>% summarise(mean = mean(mass, na.rm = TRUE))

> A tibble: 1 x 1    
   mean    
  <dbl>   
[1]  97.3    
     
##### Самый высокий 
> max(starwars$height, na.rm = TRUE)  
[1] 264
     
##### Самый низкий
> min(starwars$height, na.rm = TRUE)  
[1] 66
     
##### Отфильтруйте их (исключите из выборки самого высокого и самого низкого) и снова посчитайте среднюю масса
> starwars %>%   
>   filter(!is.na(mass)) %>%  
>   filter(66 < height & height < 264) %>%  
>   summarise(mean = mean(mass))  

> A tibble: 1 x 1  
   mean  
  <dbl>    
[1]  98.7 
     
##### Найдите средний рост жителя каждой из планет
> starwars %>%   
>   group_by(homeworld) %>%  
>   summarise(mean = mean(height, na.rm = TRUE))  

> A tibble: 49 x 2  
   homeworld       mean  
   <chr>          <dbl>  
 1 Alderaan        176.  
 2 Aleen Minor      79   
 3 Bespin          175   
 4 Bestine IV      180   
 5 Cato Neimoidia  191   
 6 Cerea           198   
 7 Champala        196   
 8 Chandrila       150   
 9 Concord Dawn    183   
10 Corellia        175   
> ... with 39 more rows  
> i Use "print(n = ...)" to see more rows  
