
library(purrr)
library(dplyr)
library(tidyr)
library(stringr)
library(lubridate)
library(readr)

## get data

# library(rdrop2)
# 
# drop_auth()
# 
# 
# 
# drop_get("CommunityAnalysis/Data/Cardoso.2008_bromeliad_closed/Cardoso2008_WGformat_correct.xls",
#          local_file = "raw-data/cardoso2008.xls")


dat <- rexcel::rexcel_read_workbook("raw-data/Cardoso2008_WGformat_correct.xlsx") ##ummm this does nothing???


# helper functions --------------------------------------------------------

matrix_to_df_firstline_header <- function(mat){
  requireNamespace("purrr")
  
  mat %>% 
    ## cut columns into lists
    apply(2, function(s) list(s)) %>% 
    flatten %>% 
    map(flatten_chr) %>% 
    ## set names to the first element of the list
    {set_names(x = map(., ~ .x[-1]),
               nm = map_chr(., 1))} %>% 
    ## create data.frames
    as.data.frame(stringsAsFactors = FALSE)
}



# environmental values ----------------------------------------------------

## look at it
dat$sheets[[3]] 

## values?

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
           matrix_to_df_firstline_header)


## where are the cells
dat$sheets$`Environmental data`$cells 

dat$sheets$`Environmental data`$lookup

## cut out the values that are giving me greif:
dat$sheets$`Environmental data`$values()[2,2:28] ## looks good
positions_of_bad_dates <- dat$sheets$`Environmental data`$lookup[2,2:28]

dat$sheets$`Environmental data`$cells[positions_of_bad_dates,]$value

## date therapy

env_df_long_dates <- env_df_long$obs %>% 
  mutate(Date = ifelse(Date == "17 Jan 207", "17 Jan 2008", Date),
         date_lubr = dmy(Date) ,
         date_exel = ifelse(is.na(date_lubr), as.numeric(Date), NA),
         from_exel = ymd("1899-12-30") + date_exel,
         collection_date = if_else(is.na(date_lubr), ymd(from_exel), ymd(date_lubr))) %>% 
  ## some dates are in the wrong year
  mutate(collection_date_fix_yr = if_else(collection_date < ymd("2008-01-01"), collection_date + years(1), collection_date)) %>% 
  ## and some are written in an incorrect format
  mutate(collection_date_corrected = if_else(collection_date_fix_yr > ymd("2008-03-01"), ydm(as.character(collection_date_fix_yr)), collection_date_fix_yr)) %>% 
  select(-date_lubr, -date_exel, -from_exel, -collection_date, -collection_date_fix_yr, -Date)


# fix column names & types --------------------------------------------------------

env_final <- env_df_long_dates %>% 
  rename(Bromeliad = NA.,
         Spider_survey  = Spider.survey.) %>%
         {set_names(., names(.) %>% str_replace("\\.\\..*\\.", ""))}%>% 
         {set_names(., names(.) %>% str_replace_all("\\.", "_"))} %>% 
  mutate_each(funs(as.numeric), Actual_volume:Plant_diameter)

write_csv(env_final, "data/environmental_variables.csv")



# insects -----------------------------------------------------------------

## look at it
dat$sheets$`Fauna - full`$sheet

## only to 151 is any good

insect_df <- dat$sheets$`Fauna - full`$values() %>% 
  .[1:151,] %>% 
  matrix_to_df_firstline_header() %>% 
  mutate(sp_code = paste0("sp_",seq_along(Biomass.avg.sp)))


## combine comments with species names

insect_comments <- dat$sheets$`Fauna - full`$comments %>% 
  mutate(text = text %>% str_replace("Auteur importé:\\nDiane Srivastava:\\n", ""))

## pull out, then slap on, the positions of the commented cells
insect_df_comments <- insect_df %>% 
  select(sp_code, NA.:Biomass.avg.sp) %>% 
  mutate(Dcol_pos = dat$sheets$`Fauna - full`$lookup[2:151,4]) %>% 
  ## put in cell refs
  mutate(ref = dat$sheets$`Fauna - full`$cells$ref[Dcol_pos],
         form = dat$sheets$`Fauna - full`$cells[["formula"]][Dcol_pos]) %>% 
  #left join comments
  left_join(insect_comments %>% select(-author, -shape_id), by = "ref")


insect_df_filled_formulae <- insect_df_comments %>% 
  replace_na(list(form = "blank")) %>% 
  mutate(filled_form = if_else(form == "", NA_character_, form)) %>% 
  fill(filled_form) %>% 
  mutate(estimating_formula = if_else(form == "blank", NA_character_, filled_form)) %>% 
  # clean up
  select(-form, -filled_form)

insect_renamed <- insect_df_filled_formulae %>% 
  rename(long_name = NA.,
         short_name = NA..1,
         life_stage_size = NA..2,
         comment = text,
         average_percapita_biomass = Biomass.avg.sp) %>% 
  select(-Dcol_pos, -ref) %>% 
  mutate(average_percapita_biomass = as.numeric(average_percapita_biomass)) %>% 
  fill(long_name)


## pull out numbers
insect_size_life <- insect_renamed %>% 
  ## create numbers (and correct any mm to cm)
  mutate(body_length  = parse_number(life_stage_size),
         is_mm = str_detect(life_stage_size, ".*mm"),
         body_length = if_else(is_mm, body_length / 10, body_length)) %>% 
  ## pull out the categories (by ignoring numbers)
  mutate(body_category = str_replace(life_stage_size, "[0-9\\.]+[\\scm]{0,3}", ""),
         body_category = str_replace(body_category, "larvae|pupae", ""),
         body_category = str_replace_all(body_category, "\\(|\\)", ""),
         body_category = body_category %>% str_trim %>% tolower,
         body_category = if_else(body_category == "", NA_character_, body_category)) %>% 
  ## finally, yank out the life history stage
  mutate(life_stage = str_extract(life_stage_size, "larv\\w+|pup\\w+")) %>% 
  tbl_df %>% 
  select(-is_mm, -life_stage_size) %>% 
  select(sp_code, long_name, short_name,
         body_length, body_category, life_stage, 
         average_percapita_biomass, estimating_formula, comment)

insect_size_life %>% 
  write_csv("data/insect_names_final.csv")

# finally the abundance data ----------------------------------------------

insect_abundances <- insect_df %>% 
  select(sp_code, starts_with("Brom")) %>% 
  gather(Bromeliad, abundance, starts_with("Brom")) %>% 
  filter(!is.na(abundance)) %>% 
  mutate(abundance = as.numeric(abundance)) %>% 
  filter(abundance > 0)

insect_abundances %>% 
  write_csv("data/abundance.csv")
