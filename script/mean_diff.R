# Visualizar distribucion

library(lattice)
library(tidyverse)

histogram( ~ Turnout | competitivo, data = turnout2016, layout = c(1,2), 
           col = "orange", breaks = seq(40, 80, by = 5),
           xlab = "Participación (%)")

boxplot(turnout2016$Turnout  ~  turnout2016$competitivo,
        xlab = "Competitividad", ylab = "Participación")


# Revisar medias de Turnout por grupo

turnout2016 %>% 
  group_by(competitivo) %>% 
  summarise(media = mean(Turnout))


# TEST t de diferencia de medias. Rechazo 
t.test(turnout2016$Turnout ~ turnout2016$competitivo) 

