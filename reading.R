## get data

library(rdrop2)

drop_auth()



drop_get("CommunityAnalysis/Data/Cardoso.2008_bromeliad_closed/Cardoso2008_WGformat_correct.xls",
         local_file = "raw-data/cardoso2008.xls")


library(readxl)

read_excel("raw-data/cardoso2008.xls", sheet = 2)

jailbreakr::split_headers(temp_dl) ##ummm this does nothing???
