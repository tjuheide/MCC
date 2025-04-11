## code to prepare `mccdata` dataset goes here
## code to prepare `events` dataset goes here
mcc_events <- data.table::fread("./data-raw/mccdata.csv")
mcc_events$grp <- factor(mcc_events$grp, levels = c(1,0), labels = c("Control", "Exposed"))
mcc_events$id <- factor(mcc_events$id)

usethis::use_data(mcc_events, overwrite = TRUE)

mcc_events_bs <- data.table::fread("./data-raw/mccbsdata.csv")


mcc_events_bs$grp <- factor(mcc_events_bs$grp, levels = c(1,0), labels = c("Control", "Exposed"))
mcc_events_bs$BootstrapSample <- factor(mcc_events_bs$BootstrapSample)
mcc_events_bs$id <- factor(mcc_events_bs$id)

usethis::use_data(mcc_events_bs, overwrite = TRUE)

