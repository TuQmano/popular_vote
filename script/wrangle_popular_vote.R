

# Load pkgs
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(geofacet) # 'ggplot2' Faceting Utilities for Geographical Data, CRAN v0.2.0
library(readxl) # Read Excel Files, CRAN v1.3.1
library(ggtext)

# load data ####
presidential_year <- seq(from = 1980 , to = 2016, by = 4)

# TURNOUT 1980 - 2016
full_turnout_data <- read_delim("data/TurnoutLong.csv", delim = ";") %>% 
  filter(Year %in% presidential_year)

turnout <- full_turnout_data %>% 
  filter(! STATE %in% c("United States (Excl, Louisiana)", 
                     "United States"), 
         X4 == "VEP") %>% 
  select(-X4)

national <-  full_turnout_data %>% 
  filter( STATE == "United States", 
         X4 == "VEP") %>% 
  select(-X4)


# 2016 POLLS and RESULTS

polls_results <- read_delim("data/votes_polls.csv", delim = ";") %>%
  mutate(compete = clinton_polls - trump_polls,
         dif = cliton_votes - trump_votes) %>%
  arrange(desc(compete)) %>%
  rename(name = state) %>% 
  print(n = Inf)


## WRANGLE DATA ####

# Add State Code

codes <- us_state_grid1 %>% 
  select(code, name)

turnout_code <- turnout %>% 
  rename(name = STATE) %>% 
  mutate(name = case_when(name == 'District of Columbia' ~ "DC", TRUE ~ name)) %>% 
  left_join(codes, by = "name") %>% 
  left_join(polls_results) %>% 
  mutate(code = ifelse(is.na(name), "DC", name)) %>% 
  print()




### PLOTS ###

# Turnout  
ggplot(turnout_code) + 
  geom_point(aes(Year, Turnout), color = 'red') + # State turnout
  geom_line(aes(Year, Turnout), data = national, 
            color = "blue", alpha = .3, size = 2) +
  facet_geo(~ code, grid = us_state_grid1 %>% 
              mutate(name = case_when(
                name == 'District of Columbia' ~ 'DC', TRUE ~ name
              )), label = "name") +
  ggthemes::theme_fivethirtyeight() +
  scale_x_continuous(breaks = c(1980, 1988, 1996, 2004, 2012), 
                     labels = c("'80", "'88", "'96", "'04", "'12")) +
  scale_y_continuous(breaks = c(40, 60, 80), labels = c("40", "60", "80")) +
  labs(title = "Participación Electoral por Estado", 
       subtitle = "Elecciones Presidenciales (1980 - 2016)", 
       caption = "@TuQmano con datos de United States Electoral Project - http://www.electproject.org/")



#dev.off()
#
#ggsave(plot = last_plot(), "plots/turnount_ts.png", width = 30, height = 18, units = "cm")





turnout2016 <- turnout %>% 
  group_by(STATE) %>% 
  mutate(mean_turnout = mean(Turnout, na.rm = T), 
         dif_mean = Turnout - mean_turnout) %>% 
  filter(Year == 2016) %>%
  rename(name = STATE) %>% 
  mutate(name = case_when(name == 'District of Columbia' ~ "DC", TRUE ~ name)) %>% 
  left_join(polls_results) %>% 
# mutate(code = ifelse(is.na(name), "DC", name)) %>% 
  arrange(desc(dif_mean)) %>% 
  mutate(competitivo = case_when(
   abs(compete) < 5 ~ "FiveThirtyEight < 5 PUNTOS", TRUE ~ "FiveThirtyEight > 5 PUNTOS" 
  )) %>% 
   print()

ggplot(turnout2016) +
  facet_wrap(~competitivo) +
  geom_col(aes(mean_turnout, 
               fct_reorder(as.factor(name), mean_turnout)), 
           fill = "blue", color = "transparent",
           alpha = .2) +
  geom_point(aes(Turnout, 
               fct_reorder(as.factor(name), mean_turnout)), 
           color = "red") +
  geom_vline(xintercept = national %>% 
               filter(Year == 2016) %>% 
               pull(Turnout)) +
  ggthemes::theme_fivethirtyeight()  +
  labs(title = "Participación Electoral por Estado", 
       subtitle = "<span style='color:blue'>Promedio Historico</span> - Promedio 2016 - <span style='color:red'> Participación 2016</span> - _FiveThirtyEight_") +
  theme(plot.subtitle = element_markdown())

####3 ELECCIONES CERRADAS

elec2000 <- tibble::tribble(
          ~Estado, ~Diferencia,    ~Dem,    ~Rep,
        "Florida",      "0.0%", "48.8%", "48.9%",
      "Wisconsin",      "0.2%", "47.8%", "47.6%",
           "Iowa",      "0.3%", "48.5%", "48.2%",
         "Oregon",      "0.4%", "46.9%", "46.5%",
   "Nuevo Mexico",      "1.0%", "47.9%", "47.8%",
  "New Hampshire",      "1.2%", "46.8%", "48.0%",
       "Minesota",      "2.4%", "47.9%", "45.5%",
         "Misuri",      "3.3%", "47.1%", "50.4%",
           "Ohio",      "3.5%", "46.5%", "50.0%",
         "Nevada",      "3.5%", "46.0%", "49.5%"
  )%>% 
  mutate(eleccion = 2000)


elec2012 <- tibble::tribble(
               ~Estado, ~Diferencia,    ~Dem,    ~Rep,
             "Florida",      "0.6%", "49.9%", "49.3%",
                "Ohio",      "1.9%", "50.1%", "48.2%",
  "Carolina del Norte",      "2.2%", "48.4%", "50.6%",
            "Virginia",      "3.0%", "50.8%", "47.8%",
            "Colorado",      "4.7%", "51.2%", "46.5%",
         "Pensilvania",      "5.2%", "52.0%", "46.8%",
                "Iowa",      "5.6%", "52.1%", "46.5%",
       "New Hampshire",      "5.8%", "52.2%", "46.4%",
              "Nevada",      "6.6%", "52.3%", "45.7%",
           "Wisconsin",      "6.7%", "52.8%", "46.1%"
  )%>% 
  mutate(eleccion = 2012)

elec2016 <- tibble::tribble(
                           ~Estado, ~Diferencia,    ~Dem,    ~Rep,
                        "Michigan",      "0.3%", "47.6%", "47.3%",
                   "New Hampshire",      "0.4%", "47.6%", "47.2%",
                       "Wisconsin",      "1.0%", "46.9%", "47.9%",
                     "Pensilvania",      "1.2%", "47.6%", "48.8%",
                         "Florida",      "1.2%", "47.8%", "49.0%",
                        "Minesota",      "1.5%", "46.4%", "44.9%",
                          "Nevada",      "2.4%", "47.9%", "45.5%",
                           "Maine",      "2.7%", "47.9%", "45.2%",
              "Carolina del Norte",      "3.8%", "46.1%", "49.9%",
                         "Arizona",      "3.9%", "49.3%", "45.4%"
              ) %>% 
  mutate(eleccion = 2016)


### ADD TURNOUT

turnout_rename <- full_turnout_data %>% 
  filter(X4 == "VEP") %>% 
  select(-X4) %>%
  group_by(Year) %>% 
  mutate(turnout_mean = mean(Turnout)) %>% 
  rename(eleccion = Year, 
         Estado = STATE) 

library(gt)

datos_tablas <- rbind(elec2000, elec2012, elec2016) %>% 
  mutate(across(.cols = c(Diferencia, Dem, Rep), .fns = ~ str_remove_all(. ,pattern =  "%"))) %>% 
  mutate(across(.cols = c(Diferencia, Dem, Rep), ~ as.numeric(.))) %>% 
  mutate(Estado = case_when(
    Estado == "Carolina del Norte" ~ "North Carolina", 
    Estado == "Nuevo Mexico" ~ "New Mexico", 
    Estado == "Misuri" ~ "Missouri", 
    Estado == "Pensilvania" ~ "Pennsylvania", 
    Estado == "Minesota" ~ "Minnesota",  T ~ Estado
  )) %>% 
  left_join(turnout_rename) %>% 
  mutate(turnout = round(Turnout - turnout_mean,1), 
         year = eleccion) %>% 
  select(1:2, 8, eleccion, year, turnout_mean) %>% 
  arrange(year, desc(turnout)) %>% 
  group_by(eleccion) %>% 
  nest()  
  

Tablas <- datos_tablas %>% 
  mutate(tabla = map(data, ~ gt::gt(.) %>% 
                       tab_header(title = glue::glue("Elección Presidencial {eleccion}")) %>% 
                       cols_label(turnout = "Participación", 
                                  Diferencia = "|Dem - Rep|")  %>%
                       data_color(
    columns = vars(turnout),
    colors = scales::col_numeric(
      palette = c(
        "red",  "blue"),
      domain = c(-11, 15)))  %>%
      cols_hide(
        columns = vars(year, turnout_mean)
      )  %>%
      tab_footnote(
        footnote = "Distanica del promedio de participación inter estadual (puntos porcentuales)",
        locations = cells_column_labels(
          columns = vars(turnout))
      ) %>%
      tab_footnote(
        footnote = "Diferencia de votos en valor absoluto (puntos porcentuales)",
        locations = cells_column_labels(
          columns = vars(Diferencia)) 
      )  %>%
      cols_align(align = "center", columns = c("Diferencia", "turnout"))
    )) 

Tablas$tabla[[1]]# %>%  gtsave(filename = "2000.rtf")
Tablas$tabla[[2]]# %>% gtsave(filename = "2012.rtf")
Tablas$tabla[[3]]# %>% gtsave(filename = "2016.rtf")
unique(turnout_rename$Estado)

#### TABLAS GT
elec2000 %>% 
  gt::gt()




