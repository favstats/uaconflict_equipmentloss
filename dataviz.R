

pacman::p_load(tidyverse, geomtextpath, lubridate, hrbrthemes, ggrepel, jsonlite)

ocr_data_new <- T

oryx_data <- readRDS("data/oryx_data.rds")
oryx_data_dates <- read_csv("data/oryx_data_dates.csv")

oryx_data %>% 
  # filter(type != "") %>%
  count(cntry_army, equipment_type) %>% 
  arrange(desc(n)) %>% 
  # filter(n > 10) %>% 
  # slice(1:20) %>% 
  # .[-21,] %>% 
  # .[12:30,] %>% 
  filter(str_detect(equipment_type, "Tank|Fighting|Personnel|Infantry|Artillery")) %>%
  filter(!str_detect(equipment_type, "Self-Propelled Anti-Tank")) %>%
  mutate(equipment_type = fct_reorder(equipment_type, n, .desc = T)) %>% 
  ggplot(aes(cntry_army, n)) +
  geom_col(aes(fill = cntry_army), position = position_dodge2(width = 0.9)) +
  geom_label(aes(label = n),  position = position_dodge2(width = 0.9)) +
  # coord_flip() +
  facet_wrap(~equipment_type, nrow = 1, scales = "free_x") +
  hrbrthemes::theme_ipsum() +
  theme(legend.key.size = unit(0.75, 'cm'), #change legend key size
        legend.key.height = unit(0.75, 'cm'), #change legend key height
        legend.key.width = unit(0.75, 'cm'), #change legend key width
        legend.title = element_text(size=21), #change legend title font size
        legend.text = element_text(size=17),
        legend.position = "none", 
        axis.text.x = element_text(face = "bold"),
        strip.background = element_rect(fill = "lightgrey", color = NA),
        plot.title = element_text(size = 40),
        plot.caption = element_text(size = 18),
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15)) +
  scale_fill_manual(name = "Country that lost equipment", values = c("darkred", "darkblue")) +
  ggtitle("Losses in Ukraine-Russia conflict") +
  labs(x = "", y = "Lost Equipment", title = "Vehicle and Equipment Losses in Russia-Ukraine War 2022 by Type", subtitle = str_wrap("Lost equipment means: captured, damaged, abandoned and/or destroyed. The data only records vehicle and equipment losses with photographic or videographic evidence. The quantity of actually lost equipment is therefore much higher and the data presented here can be seen as a 'lower bound' estimate for losses. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 158), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))  +
  scale_color_manual(values = c("darkred", "darkblue")) 

ggsave("img/overall_losses.png", width=19, height=8, dpi = 600, bg = "white")




oryx_data %>% 
  # filter(type != "") %>%
  count(cntry_army, equipment_type) %>% 
  arrange(desc(n)) %>% 
  # filter(n > 10) %>% 
  # slice(1:20) %>% 
  # .[-21,] %>% 
  # .[12:30,] %>% 
  # filter(str_detect(equipment_type, "Tank|Fighting|Personnel|Infantry|Artillery")) %>%
  mutate(equipment_type = fct_reorder(equipment_type, n, .desc = F)) %>% 
  ggplot(aes(equipment_type, n)) +
  geom_col(aes(fill = cntry_army), position = position_dodge2(width = 0.9)) +
  geom_label(aes(label = n),  position = position_dodge2(width = 0.9)) +
  coord_flip() +
  hrbrthemes::theme_ipsum() +
  theme(legend.key.size = unit(0.75, 'cm'), #change legend key size
        legend.key.height = unit(0.75, 'cm'), #change legend key height
        legend.key.width = unit(0.75, 'cm'), #change legend key width
        legend.title = element_text(size=21), #change legend title font size
        legend.text = element_text(size=17),
        legend.position = c(0.87, 0.78), 
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 40),
        plot.caption = element_text(size = 18),
        plot.subtitle = element_text(size = 20), 
        axis.title.x = element_text(size = 15), 
        axis.title.y = element_text(size = 15)) +
  scale_fill_manual(name = "Country that lost equipment", values = c("darkred", "darkblue")) +
  ggtitle("Losses in Ukraine-Russia conflict") +
  labs(x = "", y = "Lost Equipment", title = "Vehicle and Equipment Losses in Russia-Ukraine War 2022 by Type", subtitle = str_wrap("Lost equipment means: captured, damaged, abandoned and/or destroyed. The data only records vehicle and equipment losses with photographic or videographic evidence. The quantity of actually lost equipment is therefore much higher and the data presented here can be seen as a 'lower bound' estimate for losses. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 158), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))  +
  scale_color_manual(values = c("darkred", "darkblue"))  

ggsave("img/overall_losses_long.png", width=12, height=15, dpi = 600, bg = "white")


oryx_data %>% 
  # filter(type != "") %>%
  filter(str_detect(equipment_type, "Tanks")) %>% 
  count(cntry_army, status) %>% 
  mutate(status = ifelse(status == "captured", "lost by capture", status)) %>% 
  arrange(desc(n)) %>% 
  # filter(n > 10) %>% 
  # slice(1:20) %>% 
  # .[-21,] %>% 
  # .[12:30,] %>% 
  mutate(status = fct_reorder(status, n, .desc = T)) %>% 
  ggplot(aes(cntry_army, n)) +
  geom_col(aes(fill = cntry_army), position = position_dodge2(width = 0.9)) +
  geom_label(aes(label = n),  position = position_dodge2(width = 0.9)) +
  # coord_flip() +
  hrbrthemes::theme_ipsum() +
  theme(legend.key.size = unit(0.55, 'cm'), #change legend key size
        legend.key.height = unit(0.55, 'cm'), #change legend key height
        legend.key.width = unit(0.55, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=13),
        legend.position = "none", 
        strip.background = element_rect(fill = "lightgrey", color = NA),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 30),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 13), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12)) +
  scale_fill_manual(name = "Country that lost equipment", values = c("darkred", "darkblue")) +
  ggtitle("Losses in Ukraine-Russia conflict") +
  labs(x = "", y = "Lost Tanks", title = "Tank Losses in Russia-Ukraine War 2022 by Status", subtitle = str_wrap("The data only records tank losses with photographic or videographic evidence. The quantity of actually lost tanks is therefore likely higher and the data presented here can be seen as a 'lower bound' estimate for losses. Many of the entries listed as 'abandoned' will likely end up captured or destroyed and will only be reflected here if confirmed. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 158), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))  +
  scale_color_manual(values = c("darkred", "darkblue")) +
  facet_wrap(~status, scales = "free_x", nrow = 1)

ggsave("img/tank_losses.png", width=12, height=8, dpi = 600, bg = "white")


oryx_data %>% #count(equipment_type, sort = T) %>% #View
  # filter(type != "") %>%
  filter(str_detect(equipment_type, "Tanks|Armour|Infantry Fighting Vehicles")) %>% #count(equipment_type, sort = T) 
  count(cntry_army, status) %>% 
  mutate(status = ifelse(status == "captured", "lost by capture", status)) %>% 
  arrange(desc(n)) %>% 
  # filter(n > 10) %>% 
  # slice(1:20) %>% 
  # .[-21,] %>% 
  # .[12:30,] %>% 
  mutate(status = fct_reorder(status, n, .desc = T)) %>% 
  ggplot(aes(cntry_army, n)) +
  geom_col(aes(fill = cntry_army), position = position_dodge2(width = 0.9)) +
  geom_label(aes(label = n),  position = position_dodge2(width = 0.9)) +
  # coord_flip() +
  hrbrthemes::theme_ipsum() +
  theme(legend.key.size = unit(0.55, 'cm'), #change legend key size
        legend.key.height = unit(0.55, 'cm'), #change legend key height
        legend.key.width = unit(0.55, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=13),
        legend.position = "none", 
        strip.background = element_rect(fill = "lightgrey", color = NA),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 30),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 13), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12)) +
  scale_fill_manual(name = "Country that lost equipment", values = c("darkred", "darkblue")) +
  # ggtitle("Losses in Ukraine-Russia conflict") +
  labs(x = "", y = "Lost Armored Vehicles", title = "Armored Vehicle Losses in Russia-Ukraine War 2022 by Status", 
       subtitle = str_wrap("Armored vehicles include: tanks, APCs, IFVs and AFVs. The data only records vehicle losses with photographic or videographic evidence. The quantity of actually lost vehicles is therefore likely higher and the data presented here can be seen as a 'lower bound' estimate for losses. Many of the entries listed as 'abandoned' will likely end up captured or destroyed and will only be reflected here if confirmed. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 158), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))  +
  scale_color_manual(values = c("darkred", "darkblue")) +
  facet_wrap(~status, scales = "free_x", nrow = 1)

ggsave("img/armor_losses.png", width=12, height=8, dpi = 600, bg = "white")

oryx_data %>% #count(equipment_type, sort = T) %>% #View
  # filter(type != "") %>%
  filter(str_detect(equipment_type, "Artillery")) %>% #count(equipment_type, sort = T) 
  filter(status != "") %>% 
  count(cntry_army, status) %>% 
  mutate(status = ifelse(status == "captured", "lost by capture", status)) %>% 
  arrange(desc(n)) %>% 
  # filter(n > 10) %>% 
  # slice(1:20) %>% 
  # .[-21,] %>% 
  # .[12:30,] %>% 
  mutate(status = fct_reorder(status, n, .desc = T)) %>% 
  ggplot(aes(cntry_army, n)) +
  geom_col(aes(fill = cntry_army), position = position_dodge2(width = 0.9)) +
  geom_label(aes(label = n),  position = position_dodge2(width = 0.9)) +
  # coord_flip() +
  hrbrthemes::theme_ipsum() +
  theme(legend.key.size = unit(0.55, 'cm'), #change legend key size
        legend.key.height = unit(0.55, 'cm'), #change legend key height
        legend.key.width = unit(0.55, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=13),
        legend.position = "none", 
        strip.background = element_rect(fill = "lightgrey", color = NA),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 30),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 13), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12)) +
  scale_fill_manual(name = "Country that lost equipment", values = c("darkred", "darkblue")) +
  # ggtitle("Losses in Ukraine-Russia conflict") +
  labs(x = "", y = "Lost Equipment", title = "Artillery Losses in Russia-Ukraine War 2022 by Status", 
       subtitle = str_wrap("Artillery includes mortars, self-propelled and towed artillery. The data only records losses with photographic or videographic evidence. The quantity of actually lost equipment is therefore likely higher and the data presented here can be seen as a 'lower bound' estimate for losses. Many of the entries listed as 'abandoned' will likely end up captured or destroyed and will only be reflected here if confirmed. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 158), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))  +
  scale_color_manual(values = c("darkred", "darkblue")) +
  facet_wrap(~status, scales = "free_x", nrow = 1)

ggsave("img/artillery_losses.png", width=12, height=8, dpi = 600, bg = "white")


oryx_data %>% #count(equipment_type, sort = T) %>% View
  # filter(type != "") %>%
  filter(str_detect(equipment_type, "Aircraft|Helicopter")) %>%
  filter(str_detect(equipment_type, "Anti", negate = T)) %>% 
  # count(equipment_type, sort = T) 
  filter(status != "") %>% 
  count(cntry_army, status) %>% 
  mutate(status = ifelse(status == "captured", "lost by capture", status)) %>% 
  arrange(desc(n)) %>% 
  # filter(n > 10) %>% 
  # slice(1:20) %>% 
  # .[-21,] %>% 
  # .[12:30,] %>% 
  mutate(status = fct_reorder(status, n, .desc = T)) %>% 
  ggplot(aes(cntry_army, n)) +
  geom_col(aes(fill = cntry_army), position = position_dodge2(width = 0.9)) +
  geom_label(aes(label = n),  position = position_dodge2(width = 0.9)) +
  # coord_flip() +
  hrbrthemes::theme_ipsum() +
  theme(legend.key.size = unit(0.55, 'cm'), #change legend key size
        legend.key.height = unit(0.55, 'cm'), #change legend key height
        legend.key.width = unit(0.55, 'cm'), #change legend key width
        legend.title = element_text(size=15), #change legend title font size
        legend.text = element_text(size=13),
        legend.position = "none", 
        strip.background = element_rect(fill = "lightgrey", color = NA),
        axis.text.x = element_text(face = "bold"),
        plot.title = element_text(size = 30),
        plot.caption = element_text(size = 14),
        plot.subtitle = element_text(size = 13), 
        axis.title.x = element_text(size = 12), 
        axis.title.y = element_text(size = 12)) +
  scale_fill_manual(name = "Country that lost equipment", values = c("darkred", "darkblue")) +
  # ggtitle("Losses in Ukraine-Russia conflict") +
  labs(x = "", y = "Lost Equipment", title = "Aircraft Losses in Russia-Ukraine War 2022 by Status", 
       subtitle = str_wrap("Aircraft includes fighter jets and helicopters. The data only records losses with photographic or videographic evidence. The quantity of actually lost equipment is therefore likely higher and the data presented here can be seen as a 'lower bound' estimate for losses. Many of the entries listed as 'abandoned' will likely end up captured or destroyed and will only be reflected here if confirmed. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 158), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))  +
  scale_color_manual(values = c("darkred", "darkblue")) +
  facet_wrap(~status, scales = "free_x", nrow = 1)

ggsave("img/aircraft_losses.png", width=12, height=8, dpi = 600, bg = "white")


if(ocr_data_new){
  
  Sys.setlocale("LC_TIME", "en_GB.UTF-8")
  
  json_source <- jsonlite::fromJSON("https://invasion.pages.dev/data.json") %>% 
    tibble::as_tibble() %>% 
    mutate(date2 = lubridate::ymd(createdAt)) %>% 
    select(image_link = url, date2)  
  
  # read_csv("https://github.com/favstats/uaconflict_equipmentloss/raw/147c6857f11292455bbabc48029e8830b0a5c987/data/daily/2022-03-28_oryx_data.csv") %>% 
    # write_csv(file = "data/daily/2022-03-28_oryx_data.csv")
  
  daily_dat <- dir("data/daily", full.names = T) %>% 
    map_dfr(~{.x %>% read_csv() %>% mutate(path = .x)}) %>% 
    mutate(timestamp = ymd_hms(timestamp) %>% as.Date) %>% 
    mutate(date = lubridate::floor_date(timestamp, "day")) %>% 
    arrange(date) %>% 
    # filter(date >= ymd("2022-03-26")) %>% #View()
    # filter(date == ymd("2022-03-28")) %>% 
    distinct(image_link, equipment_type, cntry_army, status, .keep_all = T) %>% 
    # filter(date >= ymd("2022-04-29")) %>% 
    mutate(status = case_when(
      str_detect(status, "destroyed|sunk|scuttled|stripped") ~ "destroyed",
      str_detect(status, "abandoned|aboned") ~ "abandoned",
      str_detect(status, "damaged") ~ "damaged",
      str_detect(status, "captured") ~ "captured",
      T ~ status
    )) %>% 
    filter(!(image_link %in% oryx_data_dates$image_link))
  
  # unique(daily_dat$image_link)
  
  
  # daily_dat %>% count(date) %>% View

  # oryx_data_dates_com %>% 
  #   filter(is.na(date)) %>% View
  # oryx_data_dates %>% 
  # filter(max(date, na.rm = T) == date) %>% View
  
  oryx_data_dates_com <- oryx_data %>% 
    # bind_rows(oryx_data %>% filter(timestamp > as.Date("2022-04-29"))) %>% 
    left_join(json_source) %>% 
    rename(date = date2)
    # mutate(date = if_else(is.na(date), date2, lubridate::as_date(date)))# %>%
    # filter(max(date, na.rm = T) == date)
    # bind_rows(daily_dat) %>%
    # mutate(date = if_else(is.na(date), lubridate::as_date(timestamp), lubridate::as_date(date)))
  
  # oryx_data_dates_com %>% #View
  #   filter(is.na(date)) %>% View
  
  saveRDS(oryx_data_dates_com, file = "data/oryx_data_dates_com.rds")
  
  date_vec <- seq.Date(as.Date("2022-02-24"), today(), by = "week")
  
  
  
  week_labs <- date_vec %>% 
    format(format="%b %d", locale = locale("en")) %>% 
    paste0("Week ", 1:length(date_vec), " (", ., ")") %>% 
    .[-length(.)]
  
  week_labs2 <- date_vec %>% 
    format(format="%b %d", locale = locale("en")) %>% 
    paste0("Week ", 1:length(date_vec), "\n(", ., ")") %>% 
    .[-length(.)]
  
  # mean(oryx_data_dates_com$n)
  
  nudger <- oryx_data_dates_com %>% 
    mutate(date = lubridate::floor_date(date, "week", week_start = getOption("lubridate.week.start", 4))) %>% 
    count(cntry_army, date) %>% 
    summarize(mean_n = mean(n)) %>% 
    mutate(nudgey = 0.06*mean_n) %>% 
    pull(nudgey)
  
  # oryx_data_dates_com %>% 
    # filter(str_detect(equipment_type, "Tanks|Fighting|Personnel")) %>%
    # mutate(date = lubridate::floor_date(date, "week", week_start = getOption("lubridate.week.start", 4))) %>% count(date)
  
  
  date_vec_fin <- date_vec[-length(date_vec)]
  
  date_vec_fin <- date_vec_fin[c(TRUE, FALSE)]
  
  week_labs2_fin <- week_labs2[c(TRUE, FALSE)]
  
  overall_losses_time <- oryx_data_dates_com %>% 
    # filter(str_detect(equipment_type, "Tanks|Fighting|Personnel")) %>%
    mutate(date = lubridate::floor_date(date, "week", week_start = getOption("lubridate.week.start", 4))) %>% 
    # filter(date != max(date, na.rm = T)) %>%
    drop_na(cntry_army) %>% 
    count(cntry_army, date) %>% 
    ggplot(aes(date, n, color = cntry_army)) +
    geom_textline(size = 4.4, aes(label = cntry_army), hjust = 0.05) +
    ggrepel::geom_text_repel(aes(label = n), seed = 2410191, size = 2.8, nudge_y = nudger) +
    geom_point(size = 0.8) +
    ylim(0, NA)  +
    scale_x_date(breaks = date_vec_fin, minor_breaks = NULL, labels = week_labs2_fin) +
    # facet_wrap(~equipment_type) +
    # theme_minimal() + 
    hrbrthemes::theme_ipsum() +
    theme(legend.position = "none", 
          plot.title = element_text(size = 20),
          plot.subtitle = element_text(size = 10), 
          axis.title.x = element_text(size = 12), 
          axis.title.y = element_text(size = 12)) +
    labs(x = "Report Week", y = "Lost Equipment per Week",
         title = "Vehicle and Equipment Losses in Russia-Ukraine War 2022", 
         subtitle = str_wrap("Lost equipment means: captured, damaged, abandoned and/or destroyed. The data only records vehicle and equipment losses with photographic or videographic evidence. The quantity of actually lost equipment is therefore much higher and the data presented here can be seen as a 'lower bound' estimate for losses. Small guns, ammo, civilian cars and trailers are not included. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 150), 
         caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))  +
    scale_color_manual(values = c("darkred", "darkblue")) 
  
  
  ggsave(plot = overall_losses_time, "img/overall_losses_time.png", width=9, height=6, dpi = 600, bg = "white")
  
  
  
  vehicle_losses_time <- oryx_data_dates_com %>% 
    # filter(str_detect(equipment_type, "Tanks|Fighting|Personnel")) %>%
    mutate(date = lubridate::floor_date(date, "week", week_start = getOption("lubridate.week.start", 4))) %>% 
    filter(date != max(date, na.rm = T)) %>% 
    drop_na(cntry_army) %>% 
    count(cntry_army, date, equipment_type) %>% 
    filter(str_detect(equipment_type, "Tanks|Fighting|Personnel")) %>%
    ggplot(aes(date, n, color = cntry_army)) +
    geom_textline(size = 4.4, aes(label = cntry_army), hjust = 0.08) +
    ggrepel::geom_text_repel(aes(label = n), seed = 2410191, size = 2.8, nudge_y = 1) +
    geom_point(size = 0.8) +
    facet_wrap(~equipment_type) +
    # theme_minimal() + 
    hrbrthemes::theme_ipsum() +
    theme(legend.position = "none", 
          plot.title = element_text(size = 36),
          plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 12),
          strip.text = element_text(size = 15, face = "italic"), 
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14)) +
    scale_x_date(breaks = date_vec_fin, minor_breaks = NULL, labels = week_labs2_fin) +
    labs(x = "Report Week", y = "Lost Vehicles per Week", title = "Vehicle Losses in Russia-Ukraine War 2022", subtitle = str_wrap("Lost vehicle means: captured, damaged, abandoned and/or destroyed. The data only records vehicle losses with photographic or videographic evidence. The quantity of actually lost equipment is therefore much higher and the data presented here can be seen as a 'lower bound' estimate for losses. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 145), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))   +
    scale_color_manual(values = c("darkred", "darkblue")) 
  
  
  ggsave(plot = vehicle_losses_time, "img/vehicle_losses_time.png", width=14, height=9, dpi = 600, bg = "white")
  
  
  tank_losses_time <- oryx_data_dates_com %>% 
    filter(str_detect(equipment_type, "Tanks")) %>% 
    mutate(date = lubridate::floor_date(date, "week", week_start = getOption("lubridate.week.start", 4))) %>% 
    filter(date != max(date, na.rm = T)) %>% 
    drop_na(cntry_army) %>% 
    count(cntry_army, date, status)%>%
    filter(str_detect(status, "capture|estroye|aban")) %>%
    ggplot(aes(date, n, color = cntry_army)) +
    geom_textline(size = 4.4, aes(label = cntry_army), hjust = 0.07) +
    ggrepel::geom_text_repel(aes(label = n), seed = 2410191, size = 2.8, nudge_y = 1.75) +
    geom_point(size = 0.8) +
    facet_wrap(~status, ncol = 2) +
    # theme_minimal() + 
    hrbrthemes::theme_ipsum() +
    theme(legend.position = "none", 
          plot.title = element_text(size = 24),
          plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 12),
          strip.text = element_text(size = 15, face = "italic"), 
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14)) +
    scale_x_date(breaks = date_vec_fin, minor_breaks = NULL, labels = week_labs2_fin) +
    labs(x = "Report Week", y = "Lost Tanks per Week", title = "Tank Losses in Russia-Ukraine War 2022 by Tank Status", subtitle = str_wrap("The data only records tank losses with photographic or videographic evidence. The quantity of actually lost tanks is therefore likely higher and the data presented here can be seen as a 'lower bound' estimate for losses. Many of the entries listed as 'abandoned' will likely end up captured or destroyed and will only be reflected here if confirmed. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 145), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))   +
    scale_color_manual(values = c("darkred", "darkblue")) 
  
  ggsave(plot = tank_losses_time, "img/tank_losses_time.png", width=12, height=7, dpi = 600, bg = "white")
  
  
  tank_losses_time_cum <- oryx_data_dates_com %>% 
    filter(str_detect(equipment_type, "Tanks")) %>%
    mutate(date = lubridate::floor_date(date, "week", week_start = getOption("lubridate.week.start", 4))) %>% 
    filter(date != max(date, na.rm = T)) %>% 
    drop_na(cntry_army) %>% 
    count(cntry_army, date, status)%>%
    filter(str_detect(status, "capture|estroye|aban")) %>%
    arrange(date) %>% 
    group_by(cntry_army, status) %>% 
    mutate(n = cumsum(n)) %>% 
    ggplot(aes(date, n, color = cntry_army)) +
    geom_textline(size = 4.4, aes(label = cntry_army), hjust = 0.086) +
    ggrepel::geom_text_repel(aes(label = n), seed = 2410191, size = 2.8, nudge_y = 28.1) +
    geom_point(size = 0.8) +
    facet_wrap(~status) +
    # theme_minimal() + 
    hrbrthemes::theme_ipsum() +
    theme(legend.position = "none", 
          plot.title = element_text(size = 24),
          plot.subtitle = element_text(size = 15), plot.caption = element_text(size = 12),
          strip.text = element_text(size = 15, face = "italic"), 
          axis.title.x = element_text(size = 14), 
          axis.title.y = element_text(size = 14)) +
    scale_x_date(breaks = date_vec_fin, minor_breaks = NULL, labels = week_labs2_fin) +
    labs(x = "Report Week", y = "Lost Tanks per Week (cumulative)", title = "Tank Losses in Russia-Ukraine War 2022 by Tank Status", subtitle = str_wrap("The data only records tank losses with photographic or videographic evidence. The quantity of actually lost tanks is therefore likely higher and the data presented here can be seen as a 'lower bound' estimate for losses. Many of the entries listed as 'abandoned' will likely end up captured or destroyed and will only be reflected here if confirmed. Note: since this relies on publicly shared data there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 145), caption = glue::glue("Source: Oryxspioenkop. Data available here: https://github.com/favstats/uaconflict_equipmentloss.\nLast updated: {today()}.  Data scraping and visualization: Fabio Votta (@favstats)."))   +
    scale_color_manual(values = c("darkred", "darkblue")) 
  
  ggsave(plot = tank_losses_time_cum, "img/tank_losses_time_cum.png", width=12, height=7, dpi = 600, bg = "white")
  
  
}


oryx_data %>% 
  mutate(type = case_when(
    str_detect(equipment_type, "Artillery|Mortar|Multiple Rocket Launchers") ~ "Artillery",
    str_detect(equipment_type, "Anti-Aircraft|Surface-To-Air") ~ "Anti-Aircraft Systems",
    str_detect(equipment_type, "Aircraft|Helicopter|Unmanned Aerial Vehicles") ~ "Aircraft",
    str_detect(equipment_type, "Radar|Jammer|Communication") ~ "Radio & Communications",
    str_detect(equipment_type, "Engineering") ~ "Engineering Vehicles",
    T ~ equipment_type
  )) %>% 
  # count(equipment_type)
  # filter(str_detect(equipment_type, "Tanks|Armour|Infantry Fighting Vehicles")) %>% #count(equipment_type, sort = T) 
  count(type, cntry_army) %>%
  group_by(type) %>% 
  mutate(total = sum(n),
         perc = n/total*100) %>% 
  ungroup() %>% 
  # filter(perc < 80 & perc > 20) %>%
  filter(perc != 100) %>%
  filter(!str_detect(type, "Ship") ) %>%
  filter(!str_detect(type, "Self-Propelled Anti-Tank")) %>%
  mutate(n = ifelse(cntry_army == "Ukraine", n*-1, n)) %>% 
  mutate(perc_order = abs(n)) %>% 
  mutate(perc = ifelse(cntry_army == "Ukraine", perc*-1, perc)) %>% 
  mutate(type = case_when(
    str_detect(type, "Tanks") ~ "Tanks",
    str_detect(type, "Infantry Fighting") ~ "Infantry\nFighting\nVehicles\n(IFVs)",
    str_detect(type, "Armoured Fighting") ~ "Armoured\nFighting\nVehicles\n(AFVs)",
    str_detect(type, "Artillery") ~ "Artillery",
    str_detect(type, "Infantry Mobility") ~ "Infantry\nMobility\nVehicles\n(IMVs)",
    str_detect(type, "Armoured Personnel") ~ "Armoured\nPersonnel\nCarriers\n(APCs)",
    str_detect(type, "Anti-Aircraft") ~ "Anti-Aircraft\nSystems",
    str_detect(type, "Aircraft") ~ "Aircraft",
    str_detect(type, "Engineering") ~ "Engineering\nVehicles",
    str_detect(type, "Radio") ~ "Radio &\nComms",
    T ~type,
  )) %>% 
  mutate(type = fct_reorder(type, perc_order, .fun = sum, .desc = T)) %>% 
  ggplot(aes(type, n)) +
  geom_col(aes(fill = cntry_army), position = position_stack(), alpha = 0.9) +
  scale_fill_manual(name = "Army that lost\nequipment",values = c("#005bbb", "#ffd500")) +
  scale_color_manual(name = "Army that\nlost equipment",values = c("#005bbb", "#ffd500")) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  geom_label(aes(label = abs(n)), fill = "white", show.legend = F, 
             size = 3.5,
             label.size = 0.15, fontface = "bold")  +
  scale_y_continuous(labels = abs, minor_breaks = NULL) +
  hrbrthemes::theme_ipsum() +
  # geom_label(aes(label = type), angle = 90) +
  # ggthemes::theme_hc() +
  labs(x = "", y = "<<< Ukrainian Losses                                       Russian Losses >>> ", title = "Equipment Losses in Russia-Ukraine War 2022", 
       subtitle = str_wrap("The data shown here only records losses with photographic or videographic evidence. The quantity of actually lost equipment is therefore likely higher and the data presented here can be seen as a 'lower bound' estimate for losses. Note: since this relies on publicly shared media there may also be a bias where losses for Ukraine and Russia are underreported or overreported, respectively.", width = 138), caption = c("Source: Oryxspioenkop.\nData available here: https://github.com/favstats/uaconflict_equipmentloss.", glue::glue("Last updated: {today()}.\nData scraping and visualization: Fabio Votta (@favstats).")))  +
  # scale_x_discrete(labels = c("Tanks", "Infantry\nFighting\nVehicles\n(IFVs)", 
  #                             "Armoured\nFighting\nVehicles\n(AFVs)", "Artillery", 
  #                             "Infantry\nMobility\nVehicles\n(IMVs)", 
  #                             "Armoured\nPersonnel\nCarriers\n(APCs)", "Aircraft", 
  #                             "Anti-Aircraft\nSystems", "Engineering\nVehicles", 
  #                             "Radio &\nComms")) +
  theme(axis.text.x = element_text(size = 9.2, face = "bold"), 
        plot.title = element_text(size = 28), 
        panel.grid.major.x = element_blank() ,
        plot.caption = element_text(hjust = c(0, 1)),
        plot.subtitle = element_text(size = 11), 
        legend.position = c(0.8, 0.768)) + 
  guides(fill = guide_legend(title.position = "left", 
                             # hjust = 0.5 centres the title horizontally
                             title.hjust = 0,
                             label.position = "right"))
# coord_flip() 
ggsave("img/flag_plot.png", width=9, height=6, dpi = 600, bg = "white")

