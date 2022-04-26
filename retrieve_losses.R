library(dplyr)
library(tidyr)
library(purrr)
library(rvest)
library(stringr)
library(xml2)

require("pacman")

pacman::p_load(tidyverse, geomtextpath, lubridate, hrbrthemes, ggrepel)

tstamp <- Sys.time()

source("R/helpers.R")

raw_html <- read_html("https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html")

russia_list <- raw_html %>% 
  html_elements("article") %>% 
  html_children() %>% 
  html_children() %>%
  .[str_detect(as.character(.), "Russia -|Ukraine -")] %>% 
  html_children()

raw_html_ukraine <- read_html("https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-ukrainian.html")

ukraine_list <- raw_html_ukraine %>% 
  html_elements("article") %>% 
  html_children() %>% 
  html_children() %>%
  .[str_detect(as.character(.), "Ukraine -")] %>% 
  html_children() %>% 
  html_children()


# russia_start <- which(str_detect(as.character(full_list), "Russia -"))
# 
# ukraine_start <- which(str_detect(as.character(full_list), "Ukraine -"))
# 
# russia_list <- full_list[(russia_start+1):length(full_list)]
# 
# ukraine_end <- which(str_detect(as.character(full_list), "Special thanks"))
# 
# ukraine_list <- full_list[(ukraine_start+1):ukraine_end]



nested_lists <- which(str_count(as.character(ukraine_list), "<h3>") > 2)

# nested_lists <- ukraine_list %>% str_which("Trucks, ")


list1 <- ukraine_list[-nested_lists]

list2 <- ukraine_list[nested_lists] %>%
  html_children()

for (node in seq_along(list2)) {
  end_of_list <- length(list1)
  
  list1[end_of_list+1] <- list2[node]  
}


ukraine_list <- list1


nested_lists <- ukraine_list %>% str_which("Trucks, ")


list1 <- ukraine_list[-nested_lists:-length(ukraine_list)]

list2 <- ukraine_list[nested_lists:length(ukraine_list)] %>%
  html_children()

for (node in seq_along(list2)) {
  end_of_list <- length(list1)

  list1[end_of_list+1] <- list2[node]
}


ukraine_list <- list1

# ukraine_list %>% str_which("Trucks, ")
# 
# 
# ukraine_list[72:length(ukraine_list)] %>%
#   html_children() %>% as.character()


# ukraine_list[74:length(ukraine_list)] %>% 
#   html_text()

russia_titles <- which(str_detect(as.character(russia_list), "<h3>") & str_detect(as.character(russia_list), "<h3>\n</h3>", negate = T))
ukraine_titles <- c(which(str_detect(as.character(ukraine_list), "<h3>") & str_detect(as.character(ukraine_list), "<h3>\n</h3>", negate = T) | str_detect(as.character(ukraine_list), "Trucks, Vehicles and Jeeps")), length(ukraine_list))

russia_intervals <- splitInts(russia_titles) 

russia_intervals <- russia_intervals[-length(russia_intervals)]

ukraine_intervals <- splitInts(ukraine_titles) 

ukraine_intervals <- ukraine_intervals[-length(ukraine_intervals)]


# debugging
# debugonce(retrieve_lists)
# 
# retrieve_lists2(russia_list, russia_intervals[[1]]) %>% 
# retrieve_lists2(ukraine_list, ukraine_intervals[[19]]) %>% View
# 
# russia_list[russia_titles] %>% 
#   html_text()
# 
# 
# 
# ukraine_list[ukraine_titles] %>% 
#   html_text()
# 
# 
# retrieve_lists <- possibly(retrieve_lists, otherwise = NULL, quiet = F)



russia_data <- russia_intervals %>%
  map_dfr(~{retrieve_lists2(russia_list, .x)}) %>%
  mutate(owner = "Russia")

ukraine_data <- ukraine_intervals %>% 
  # .[1] %>%
  map_dfr(~{retrieve_lists2(ukraine_list, .x)}) %>% 
  mutate(owner = "Ukraine")

# raw_html_ukraine %>% 
#   html_elements("#post-body-510263267229750131 > div > div:nth-child(5) > div:nth-child(47) > ul:nth-child(18)") %>% 
#   html_children() -> yo
# 
# debugonce(retrieve_lists2)
# 
# retrieve_lists2(yo, 1: length(yo)) %>% View



oryx_data <- russia_data %>% 
  bind_rows(ukraine_data) %>%
  mutate(type = str_replace_all(type, "-tank", "-Tank")) %>% 
  mutate(totals = str_extract_all(type, " \\s*\\([^\\)]+\\)")) %>%
  mutate(total_equipment_type_oryx = str_extract(title, "\\d+") %>% parse_number) %>% 
  mutate(total_system_oryx = str_extract(totals, "\\d+") %>% parse_number) %>% 
  mutate(total_destroyed_oryx = str_extract(type, "(?<=destroyed:\\s)\\w+") %>% parse_number) %>% 
  mutate(total_abandoned_oryx = str_extract(type, "(?<=abandoned:\\s)\\w+") %>% parse_number) %>% 
  mutate(total_captured_oryx = str_extract(type, "(?<=captured:\\s)\\w+") %>% parse_number) %>% 
  mutate(total_damaged_oryx = str_extract(type, "(?<=damaged:\\s)\\w+") %>% parse_number) %>% 
  mutate(type = str_replace_all(type, " \\s*\\([^\\)]+\\)", "")) %>% 
  mutate(state = case_when(
    str_detect(state, "destroyed|sunk|scuttled|stripped") ~ "destroyed",
    str_detect(state, "abandoned|aboned") ~ "abandoned",
    str_detect(state, "damaged") ~ "damaged",
    str_detect(state, "captured") ~ "captured",
    T ~ state
  ))  %>%
  select(equipment_type = type, cntry_army = owner, flag, system = equipment, status = state, image_link, total_equipment_type_oryx:total_damaged_oryx) %>% 
  mutate(timestamp = tstamp) 

# ryx_data %>% count(equipment_type)
# 
# oryx_data %>% filter(equipment_type == "Trucks, Vehicles and Jeeps")

saveRDS(oryx_data, file = "data/oryx_data.rds")
write_csv(oryx_data, file = glue::glue("data/daily/{Sys.Date()}_oryx_data.csv"))

source("dataviz.R")



# oryx_data %>% 
#   filter(equipment_type == "Tanks") %>% 
#   filter(cntry_army == "Russia") %>% 
#   count(status, sort = T)

# oryx_data %>% 
#   # filter(type != "") %>%
#   count(cntry_army, equipment_type) %>% 
#   # .[-21,] %>% 
#   # .[12:30,] %>% 
#   mutate(equipment_type = fct_reorder(equipment_type, n)) %>% 
#   ggplot(aes(equipment_type, n)) +
#   geom_col(aes(fill = cntry_army), position = position_dodge2(width = 0.9)) +
#   geom_label(aes(label = n),  position = position_dodge2(width = 0.9)) +
#   coord_flip() +
#   theme_minimal() +
#   scale_fill_manual(values = c("darkred", "darkblue")) +
#   theme(legend.position = "top") +
#   ggtitle("Losses in Ukraine-Russia conflict") +
#   labs(x = "Equipment Type", y = "Losses (captured, damaged, abandoned and/or destroyed)", caption = "Source: https://www.oryxspioenkop.com/2022/02/attack-on-europe-documenting-equipment.html") 
# 
# ggsave("img/count_per_type.png", width = 8, height = 14, bg = "white")

