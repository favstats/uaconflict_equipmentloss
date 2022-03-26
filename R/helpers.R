splitInts <- function(x){
  x1 <- seq(x[1], x[length(x)])
  unname(split(x1, findInterval(x1, x[-1])))
}



retrieve_lists2 <- function(data, the_intervals) {
  
  interval_dat <<- data[the_intervals] 
  
  the_title <- interval_dat %>%
    # html_nodes("span") %>% 
    html_text() %>% 
    .[1]
  
  interval_dat %>% 
    html_children() %>%
    # html_children() %>% #html_attr("src")
    # .[1:10] %>% 
    map_dfr(~{
      
      # print(html_children(.x) %>% html_text())
      
      the_title <- html_text(.x)
      
      df <- .x %>% 
        html_children() %>% 
        map_dfr(~{
          links <- .x %>% html_attr("href") 
          entities <- .x %>% html_text() 
          origin <- .x %>% html_attr("src")
          
          tibble(links, entities, origin)
        })
      
      
      
      tibble(title = the_title) %>% bind_cols(df)
      
    }) %>% 
    rowwise() %>%
    mutate(equipment = str_split(title, ":") %>% unlist() %>% .[1] %>% str_remove("\\w+\\s"),
           state = str_split(entities, ",") %>% unlist() %>% .[2] %>% str_remove("\\)")) %>%
    ungroup() %>% 
    # filter(str_detect(equipment, "\\bT-80BV\\b")) %>% 
    mutate(state = str_extract(entities %>% str_remove_all("\\)|\\("), "[^,]+$")) %>% 
    # mutate(entities2 = str_remove(entities %>% str_remove_all("\\)|\\("), "[^,]+$")) %>% 
    mutate(entities2 = str_replace_all(entities, "\\b and\\b", ",") %>% str_remove_all(" ")) %>%
    # mutate(entities2 = str_remove_all(entities2, "[^\\b[:digit:],\\b]")) %>% 
    # View
    tidyr::separate_rows(entities2, sep = ",") %>% 
    mutate(entities2 = entities2 %>% str_remove_all("\\)|\\(|destroyed|abandoned|destroyedbyBayraktarTB2|captured|damaged|and")) %>% 
    mutate(entities2 = str_trim(entities2) %>% str_squish()) %>% 
    mutate(entities2 = ifelse(entities2 == "", NA, suppressWarnings(as.numeric(entities2)))) %>% 
    # View
    mutate(type = the_title) %>% 
    fill(origin, .direction = "down") %>% 
    drop_na(entities2) %>% 
    # mutate(type = title[1]) %>% 
    select(type, equipment, state, image_link = links, flag = origin, title) %>% 
    mutate_all(~str_trim(.x) %>% str_squish())
}



retrieve_lists <- function(data, the_intervals) {
  
  interval_dat <<- data[the_intervals] 
  
  the_title <- interval_dat %>%
    # html_nodes("span") %>% 
    html_text() %>% 
    .[1]
  
  interval_dat %>% 
    html_children() %>%
    # html_children() %>% #html_attr("src")
    # .[1:10] %>% 
    map_dfr(~{
      
      # print(html_children(.x) %>% html_text())
      
      the_title <- html_text(.x)
      
      the_children <- html_children(.x) %>% html_text() %>% as.character()
      the_links <- html_children(.x) %>% html_attr("href") %>% paste0(collapse = "QQQ")
      origins <- html_children(.x) %>% html_attr("src")
      
      if(length(the_children)==0){
        the_children <- NA
        the_links <- NA
        origins <- NA
      }
      
      
      
      tibble(title = the_title, entities = the_children, links = the_links, origin = origins)
      
    }) %>% View
  rowwise() %>%
    mutate(equipment = str_split(title, ":") %>% unlist() %>% .[1] %>% str_remove("\\w+\\s"),
           state = str_split(entities, ",") %>% unlist() %>% .[2] %>% str_remove("\\)")) %>%
    ungroup() %>%
    mutate(state = str_extract(entities %>% str_remove_all("\\)|\\("), "[^,]+$")) %>% 
    mutate(entities2 = str_remove(entities %>% str_remove_all("\\)|\\("), "[^,]+$")) %>% 
    tidyr::separate_rows(entities2, sep = ",|and") %>% 
    View
  mutate(type = the_title) %>% 
    # mutate(type = title[1]) %>% 
    fill(origin, .direction = "down") %>% 
    select(type, equipment, state, image_link = links, flag = origin) %>% 
    drop_na() %>% View()
  mutate_all(~str_trim(.x) %>% str_squish())
}