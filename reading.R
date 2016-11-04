## get data

library(rdrop2)

drop_auth()

temp_dl <- tempfile(fileext = ".xls")

drop_get("CommunityAnalysis/Data/Cardoso.2008_bromeliad_closed/Cardoso2008_WGformat_correct.xls",
         local_file = temp_dl)


library(readxl)

read_excel(temp_dl, sheet = 2)

jailbreakr::split_headers(temp_dl) ##ummm this does nothing???
