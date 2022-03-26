

russia_data %>% 
  bind_rows(ukraine_data) %>% 
  mutate(total = str_extract(title, "\\d+")) %>% 
  # filter(str_detect(type, "Infantry Fighting Vehicles")) %>%
  group_by(equipment, owner, type) %>% 
  summarize(n = n(),
            total = total[1]) %>% 
  ungroup()  %>% 
  filter(n != total) %>% View


state_counts_emp <- russia_data %>% 
  bind_rows(ukraine_data) %>%
  mutate(type = str_replace_all(type, " \\s*\\([^\\)]+\\)", "")) %>%
  mutate(state = case_when(
    str_detect(state, "destroyed|sunk|scuttled|stripped") ~ "destroyed",
    str_detect(state, "abandoned|aboned") ~ "abandoned",
    str_detect(state, "damaged") ~ "damaged",
    T ~ state
  )) %>% 
  count(owner, type, state, sort  = T, name = "empirical_count") 

state_counts_oryx <- russia_data %>% 
  bind_rows(ukraine_data) %>% 
  # separate(type, into = c("totals", "destroyed"))
  mutate(destroyed = str_extract(type, "(?<=destroyed:\\s)\\w+")) %>% 
  mutate(abandoned = str_extract(type, "(?<=abandoned:\\s)\\w+")) %>% 
  mutate(captured = str_extract(type, "(?<=captured:\\s)\\w+"))%>% 
  mutate(damaged = str_extract(type, "(?<=damaged:\\s)\\w+")) %>% 
  mutate(type = str_replace_all(type, " \\s*\\([^\\)]+\\)", "")) %>%
  select(owner, type, destroyed:damaged) %>% 
  distinct(owner, type, .keep_all = T) %>% 
  gather(state, oryx_count, -owner, -type) %>% 
  drop_na()

state_counts_emp %>% 
  left_join(state_counts_oryx) %>% 
  filter(empirical_count != oryx_count) %>% View



russia_data %>% 
  bind_rows(ukraine_data) %>%  
  mutate(totals = str_extract_all(type, " \\s*\\([^\\)]+\\)")) %>% 
  mutate(type = str_replace_all(type, " \\s*\\([^\\)]+\\)", "")) %>%
  mutate(total = str_extract(totals, "\\d+")) %>% 
  group_by(type, owner) %>% 
  summarize(n = n(),
            total = total[1]) %>% 
  ungroup() %>% 
  filter(n != total) %>% View





russia_data %>% 
  bind_rows(ukraine_data) %>%
  mutate(totals = str_extract_all(type, " \\s*\\([^\\)]+\\)")) %>% 
  mutate(type = str_replace_all(type, " \\s*\\([^\\)]+\\)", "")) %>%
  mutate(state = case_when(
    str_detect(state, "destroyed|sunk|scuttled|stripped") ~ "destroyed",
    str_detect(state, "abandoned|aboned") ~ "abandoned",
    str_detect(state, "damaged") ~ "damaged",
    T ~ state
  ))  %>%
  mutate(equipment_type_total = str_extract(totals, "\\d+")) 