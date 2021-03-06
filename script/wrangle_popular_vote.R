#
#
#   Voto Popular vs Colegio Electoral
#   Por Javier Caches y JP Ruiz Nicolini
#   1/10/2020
#   
#   UPDATE: 7/11/2020
#




# Load pkgs
library(tidyverse) # Easily Install and Load the 'Tidyverse', CRAN v1.3.0
library(geofacet) # 'ggplot2' Faceting Utilities for Geographical Data, CRAN v0.2.0
library(readxl) # Read Excel Files, CRAN v1.3.1
library(ggtext) # Improved Text Rendering Support for 'ggplot2', CRAN v0.1.0
library(gt) # Easily Create Presentation-Ready Display Tables, CRAN v0.2.2

# load data ####
presidential_year <- seq(from = 1980 , to = 2016, by = 4)

# TURNOUT 1980 - 2016
full_turnout_data <- read_delim("data/TurnoutLong.csv", delim = ";") %>% 
  filter(Year %in% presidential_year)


# 2016 POLLS and RESULTS

polls_results <- read_delim("data/votes_polls.csv", delim = ";") %>%
  mutate(compete = clinton_polls - trump_polls,
         dif = cliton_votes - trump_votes) %>%
  arrange(desc(compete)) %>%
  rename(name = state) %>% 
  print(n = Inf)


as_tibble(polls_results$name) %>% 
  print(n = Inf)

## WRANGLE DATA ####

turnout <- full_turnout_data %>% 
  filter(! STATE %in% c("United States (Excl, Louisiana)", 
                     "United States"), 
         X4 == "VEP") %>% 
  select(-X4)

national <-  full_turnout_data %>% 
  filter( STATE == "United States", 
         X4 == "VEP") %>% 
  select(-X4)


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

# Turnout TS States geofacet 

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



# 2016 turnout (vs promedio nacional y turnout historico por estado)

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
       subtitle = "<span style='color:blue'>Promedio Histórico</span> - Promedio 2016 - <span style='color:red'> Participación 2016</span> - _FiveThirtyEight_", 
       caption = "@TuQmano con datos de United States Electoral Project - http://www.electproject.org/ y FiveThirtyEight - https://fivethirtyeight.com/") +
  theme(plot.subtitle = element_markdown())



#dev.off()
#
#ggsave(plot = last_plot(), "plots/turnount_2016_facet.png", width = 30, height = 18, units = "cm")




#### Analisis de casos (años) 3 ELECCIONES CERRADAS

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


### TURNOUT - 3 CASOS (TABLAS)

turnout_rename <- full_turnout_data %>% 
  filter(X4 == "VEP") %>% 
  select(-X4) %>%
  group_by(Year) %>% 
  mutate(turnout_mean = mean(Turnout)) %>% 
  rename(eleccion = Year, 
         Estado = STATE) 

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
        footnote = "Distancia del promedio de participación inter estadual (puntos porcentuales)",
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



##### 2020
##### 

elec2020 <- tibble::tribble(
             ~name, ~turnount, ~compete,
         "Alabama",      0.62,       0L,
          "Alaska",       0.7,       0L,
         "Arizona",      0.66,       1L,
        "Arkansas",      0.56,       0L,
      "California",      0.63,       0L,
        "Colorado",      0.77,       0L,
     "Connecticut",      0.71,       0L,
        "Delaware",      0.71,       0L,
              "DC",      0.65,       0L,
         "Florida",      0.72,       1L,
         "Georgia",      0.68,       1L,
          "Hawaii",      0.58,       0L,
           "Idaho",      0.68,       0L,
        "Illinois",      0.68,       0L,
         "Indiana",      0.61,       0L,
            "Iowa",      0.79,       1L,
          "Kansas",      0.64,       0L,
        "Kentucky",      0.65,       0L,
       "Louisiana",      0.65,       0L,
           "Maine",      0.79,       1L,
        "Maryland",      0.72,       0L,
   "Massachusetts",      0.73,       0L,
        "Michigan",      0.74,       0L,
       "Minnesota",      0.79,       0L,
     "Mississippi",       0.6,       0L,
        "Missouri",      0.67,       0L,
         "Montana",      0.72,       0L,
        "Nebraska",      0.68,       0L,
          "Nevada",      0.64,       0L,
   "New Hampshire",      0.75,       0L,
      "New Jersey",      0.73,       0L,
      "New Mexico",      0.61,       0L,
        "New York",      0.65,       0L,
  "North Carolina",      0.74,       1L,
    "North Dakota",      0.65,       0L,
            "Ohio",       0.7,       1L,
        "Oklahoma",      0.55,       0L,
          "Oregon",      0.75,       0L,
    "Pennsylvania",      0.72,       1L,
    "Rhode Island",      0.65,       0L,
  "South Carolina",      0.64,       0L,
    "South Dakota",      0.66,       0L,
       "Tennessee",       0.6,       0L,
           "Texas",      0.61,       0L,
            "Utah",      0.62,       0L,
         "Vermont",      0.74,       0L,
        "Virginia",      0.71,       0L,
      "Washington",      0.75,       0L,
   "West Virginia",      0.57,       0L,
       "Wisconsin",      0.76,       0L,
         "Wyoming",      0.65,       0L
  )

turnout2020 <- elec2020 %>% 
  transmute(Year = 2020, 
            STATE = name, 
            Turnout = turnount*100)

turnout2020 <- turnout %>% 
  bind_rows(turnout2020)%>% 
  group_by(STATE) %>% 
  mutate(mean_turnout = mean(Turnout, na.rm = T), 
         dif_mean = Turnout - mean_turnout) %>% 
  filter(Year == 2020) %>%
  rename(name = STATE) %>% 
  mutate(name = case_when(name == 'District of Columbia' ~ "DC", TRUE ~ name)) %>% 
  left_join(elec2020) %>% 
  # mutate(code = ifelse(is.na(name), "DC", name)) %>% 
  arrange(desc(dif_mean)) %>% 
  mutate(competitivo = case_when(
    compete == 1 ~ "FiveThirtyEight < 5 PUNTOS", TRUE ~ "FiveThirtyEight > 5 PUNTOS" 
  )) %>% 
  print()




ggplot(turnout2020) +
  facet_wrap(~competitivo) +
  geom_col(aes(mean_turnout, 
               fct_reorder(as.factor(name), mean_turnout)), 
           fill = "blue", color = "transparent",
           alpha = .2) +
  geom_point(aes(Turnout, 
                 fct_reorder(as.factor(name), mean_turnout)), 
             color = "red") +
  geom_vline(xintercept = 66.5) +
  ggthemes::theme_fivethirtyeight()  +
  labs(title = "Participación Electoral por Estado", 
       subtitle = "<span style='color:blue'>Promedio (1980 - 2020)</span> - 
       <span style='color:red'> 2020</span>", 
       caption = "**@TuQmano con datos de *United States Electoral Project* y _FiveThirtyEight_**.
       \n(**NOTAS**: La línea vertical marca el promedio 2020 a nivel nacional
       y los paneles dividen nivel de competitividad antes de la elección).") +
  theme(plot.subtitle = element_markdown(), 
        plot.caption = element_markdown())





lbattlegrounds <- tibble::tribble(
               ~Estado, ~Diferencia,
             "Georgia",         "-",
         "Pensilvania",       "0.2",
           "Wisconsin",       "0.4",
             "Arizona",       "1.3",
  "Carolina del Norte",       "1.4",
              "Nevada",       "1.6",
            "Michigan",       "2.6",
             "Florida",       "3.4",
               "Texas",       "5.6",
            "Minesota",       "6.7"
  )




