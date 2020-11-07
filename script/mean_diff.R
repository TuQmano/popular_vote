# Visualizar distribucion

library(lattice)
library(tidyverse)

histogram( ~ Turnout | competitivo, data = turnout2016, layout = c(1,2), 
           col = "orange", breaks = seq(40, 80, by = 5),
           xlab = "Participación (%)")

boxplot(turnout2016$Turnout  ~  turnout2016$competitivo,
        xlab = "Competitividad", ylab = "Participación")


# Revisar medias de Turnout por grupo

promedios16 <- turnout2016 %>% 
  group_by(competitivo) %>% 
  summarise(media = mean(Turnout)) %>% 
  mutate(year = 2016)


# TEST t de diferencia de medias. Rechazo 
t.test(turnout2016$Turnout ~ turnout2016$competitivo) 



## 2020
histogram( ~ Turnout | competitivo, data = turnout2020, layout = c(1,2), 
           col = "orange", breaks = seq(50, 80, by = 5),
           xlab = "Participación (%)")

boxplot(turnout2020$Turnout  ~  turnout2020$competitivo,
        xlab = "Competitividad", ylab = "Participación")


# Revisar medias de Turnout por grupo

promedios20 <- turnout2020 %>% 
  group_by(competitivo) %>% 
  summarise(media = mean(Turnout)) %>% 
  mutate(year = 2020)


# TEST t de diferencia de medias. Rechazo 
t.test(turnout2020$Turnout ~ turnout2020$competitivo) 


library(gt)

bind_rows(promedios16, promedios20) %>%
  mutate(media = round(media/100,3)) %>% 
  pivot_wider(names_from = year, values_from = media) %>% 
  mutate(crecimiento = (`2020` - `2016`)*100) %>% 
  gt::gt() %>%
  gt::cols_label(
    competitivo = md("*Competitividad*"), 
    crecimiento = "Crecimiento"
  ) %>% 
  gt::tab_header(title = md(" **Elecciones 2020** | Estados Unidos"))  %>% 
  gt::fmt_percent(columns = c(2, 3), decimals = 1)%>% 
  gt::tab_source_note(source_note =  md(" **NOTA:** Los datos de participación son del United States 
                      Electoral Project. Los datos de competitividad de FiveThirtyEight"))

