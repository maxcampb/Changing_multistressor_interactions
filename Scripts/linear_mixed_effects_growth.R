



files <- list.files(path = "Data", pattern = "diuron_light_rep", full.names = TRUE)

xdiuron_light <- lapply(files, function(x) read.csv(x)) %>%
  bind_rows(.id = "id") %>% within(hours[hours == 0.3] <- 0.33)


xdiuron_light2 <- xdiuron_light  %>% 
  filter(Replicate == "AVERAGE", Diuron != "Blank", hours >= 0) %>%
  group_by(hours, Light, Diuron, id) %>% 
  summarise(Absorbance = mean(Absorbance), X = paste(X, collapse = " ")) %>% ungroup() %>% 
  within({
    
    Diuron_num <-  case_when(Diuron == "AlgaeControl" ~ 0,
                             Diuron == "MeOH" ~ 0,
                             TRUE ~ as.numeric(Diuron)
    )
    
    Light_num <-  as.numeric(Light)
    
    block <-  factor(id)
    
    hours_fact <- factor(hours)
    
    celld <- 4179.6 * Absorbance - 172.48
    t0 <- ifelse(hours == 0, celld, NA)
    
  }) %>% group_by(Light, Diuron, id ) %>% fill(t0) %>% 
  ungroup() %>%  
  mutate(sample_id = factor(gsub("."," ", x = paste(Diuron, Light, block), fixed = TRUE))) %>% 
  filter(hours != 0, Diuron != "MeOH") %>% 
  mutate(Diuron_fact = factor(ifelse(Diuron == "AlgaeControl", "0", Diuron)))
