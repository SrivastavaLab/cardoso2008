## get data

library(rdrop2)

drop_auth()



drop_get("CommunityAnalysis/Data/Cardoso.2008_bromeliad_closed/Cardoso2008_WGformat_correct.xls",
         local_file = "raw-data/cardoso2008.xls")


library(readxl)

read_excel("raw-data/Cardoso2008_WGformat_correct.xlsx", sheet = 2)

dat <- rexcel::rexcel_read_workbook("raw-data/Cardoso2008_WGformat_correct.xlsx") ##ummm this does nothing???


dat %>% class



# environmental values ----------------------------------------------------

## look at it
dat$sheets[[3]]

## values?

library(purrr)
library(dplyr)
library(tidyr)
library(stringr)

dat$sheets$`Environmental data`$cells %>% select(ref, value) %>% mutate(val = map(value, as.numeric)) %>% unnest(val) %>% View


env_df <- dat$sheets$`Environmental data`$values()

env_df_long <- env_df %>%
  ## variables in columns
  t %>% 
  {list(obs = .[,1:11], notes = .[,12:15])} %>% 
  ## extract notes into an attractive character vector
  map_at("notes", ~ .x %>% .[!is.na(.)] %>% paste(collapse = "; ")) %>% 
  ## get the real data into a data.frame
  map_at("obs", ~ .x %>% 
           ## drop blank lines
           .[-(2:3),] %>% 
           ## cut columns into lists
           apply(2, function(s) list(s)) %>% flatten %>% map(flatten_chr) %>% 
           ## set names to the first element of the list
           {set_names(x = map(., ~ .x[-1]),
                      nm = map_chr(., 1))} %>% 
           ## create data.frames
           as.data.frame
  )


env_df_long$obs %>% map(flatten_chr) %>% as.data.frame() %>% glimpse

%>% as.data.frame)

matrix(1:4, nrow = 2) %>% apply(2, function(s) list(s)) %>% map(flatten_chr) %>% {set_names(x = map(., ~ .x[-1]),
                                                                                   nm = map_chr(., 1))}
  {split(1:2)
