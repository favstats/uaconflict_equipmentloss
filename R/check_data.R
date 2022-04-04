

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


asda <- "7 T-64BV: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, damaged) (6, captured) (7, captured)
8 T-72A: (1, destroyed) (2, destroyed) (3, captured) (4, captured) (5, captured) (6, captured) (7, captured) (8, captured)
3 T-72AV: (1, destroyed) (2, destroyed) (3, captured)
49 T-72B: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, destroyed) (6, captured and later destroyed) (7, destroyed) (8, destroyed) (9, destroyed) (10, destroyed) (11, destroyed) (12 and 13, destroyed) (14, destroyed) (15, destroyed) (16, destroyed) (17, destroyed) (18, destroyed) (19, destroyed) (20, damaged) (21, damaged and abandoned) (22, abandoned) (23, abandoned) (24, abandoned) (25, abandoned) (26, abandoned) (27, damaged and captured) (28, captured) (29, captured) (30, captured) (31, captured) (32, captured and stripped) (33, captured) (34, captured) (35, captured) (36, captured) (37, captured) (38, captured) (39, captured) (40, captured) (41, captured) (42, captured) (43, captured) (44, captured) (45, captured) (46, captured) (47, captured) (48, captured) (49, captured)
15 T-72B Obr. 1989: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, destroyed) (6, damaged) (7, abandoned) (8, abandoned) (9, abandoned) (10, abandoned and destroyed) (11, damaged and captured) (12, captured) (13, captured) (14, captured) (15, captured)
35 T-72B3: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, destroyed) (6, destroyed) (7, destroyed) (8 and 9, destroyed) (10, destroyed) (11, destroyed) (12, destroyed) (13, destroyed) (14, destroyed) (15, destroyed) (16, damaged) (17, damaged) (18, abandoned) (19, abandoned) (20, abandoned) (21, abandoned) (22, damaged and captured) (23, captured) (24, captured) (25, captured) (26, captured) (27, captured) (28, captured) (29, captured) (30, captured) (31, captured) (32, captured) (33, captured) (34, captured) (35, captured)
72 T-72B3 Obr. 2016: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, destroyed) (6, destroyed) (7, destroyed) (8, destroyed) (9 and 10, destroyed) (11, destroyed) (12, destroyed) (13, destroyed) (14, destroyed) (15, destroyed) (16, destroyed) (17, destroyed) (18, destroyed) (19, destroyed) (20, destroyed) (21, destroyed) (22, destroyed) (23, destroyed) (24, destroyed) (25, destroyed) (26, destroyed) (27, destroyed) (28, destroyed) (29, destroyed) (30, destroyed) (31, destroyed) (32, abandoned and later destroyed) (33, abandoned) (34, abandoned) (35, abandoned) (36, abandoned) (37, abandoned) (38, abandoned) (39, damaged and captured) (40, damaged and captured) (41, captured) (42, captured) (43, captured) (44, captured) (45, captured) (46, captured) (47, captured) (48, captured) (49, captured) (50, captured) (51, captured) (52, captured) (53, captured) (54, captured) (55, captured) (56, captured) (57, captured) (58, captured) (59, captured) (60, captured) (61, captured) (62, captured) (63, captured) (64, captured) (65, captured) (66, captured) (67, captured) (68, captured) (69, captured) (70, captured) (71, captured) (72, captured)
10 T-80BV: (1, destroyed) (2, destroyed) (3, destroyed) (4, abandoned) (5, abandoned) (6, captured) (7, and 8, captured) (9, captured) (10, captured)
46 T-80U: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, abandoned and later destroyed) (6, destroyed) (7, abandoned) (8, abandoned) (9, abandoned) (10, abandoned) (11, abandoned) (12, abandoned) (13, abandoned) (14, abandoned) (15, abandoned) (16 and 17, abandoned) (18, abandoned) (19, abandoned) (20, abandoned and destroyed) (21, damaged and captured) (22, damaged and captured) (23, captured and stripped) (24, captured) (25, captured) (26, captured) (27, captured) (28, captured) (29, captured) (30, captured) (31, captured) (32, captured) (33, captured) (34, captured) (35 and 36, captured) (37, captured) (38, captured) (39 and 40, captured) (41 and 42, captured) (43 and 44, captured) (45, captured) (46, captured)
2 T-80UK: (1, abandoned and later destroyed) (2, captured)
1 T-80UM2: (1, destroyed)  
18 T-80BVM: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, destroyed) (6, abandoned) (7, captured) (8, captured) (9, captured) (10, captured) (11, captured) (12, captured) (13, captured) (14, captured) (15, captured) (16, captured) (17, captured) (18, captured)
17 T-90A: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, destroyed) (6, destroyed) (7, abandoned) (8, abandoned and later destroyed) (9, abandoned and later destroyed) (10, abandoned) (11, captured) (12, captured) (13, captured) (14 and 15, captured) (16, captured) (17, captured)
33 Unknown tank: (1, destroyed) (2, destroyed) (3, destroyed) (4, destroyed) (5, destroyed) (6, destroyed) (7, destroyed) (8, destroyed) (9, destroyed) (10, destroyed) (11, destroyed) (12, destroyed) (13, destroyed) (14, destroyed) (15, destroyed) (16, destroyed) (17, destroyed) (18, destroyed) (19, destroyed) (20, destroyed) (21, destroyed) (22, destroyed) (23, destroyed) (24, destroyed) (25, destroyed) (26, destroyed) (27, destroyed) (28, destroyed) (29, damaged) (30, damaged) (31, damaged) (32, abandoned) (33, captured)"

str_count(asda, "captured")


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
