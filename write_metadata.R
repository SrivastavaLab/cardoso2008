## collect column headers for metadata writing

dir("data", full.names = TRUE) %>% 
  set_names(basename(.)) %>% 
  map(read_csv) %>% 
  map_df(~ .x %>% names %>% data_frame(variable_name = .), .id = "dataset_name") %>% 
  write_csv("metadata_blank.csv")
  

