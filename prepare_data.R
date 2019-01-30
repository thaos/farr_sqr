library(magrittr)

load(file = "SQR_RR_avant1961_PourtourMedit_jan2015.RData")
station_metadata <- liste
saveRDS(station_metadata, file = "station_metadata.rds")
station_list <- read.table("list_stations.txt")
names(station_list) <- "station_id"

subset(station_metadata, NUM_POSTE %in% station_list$station_id)


get_station_files <- function(id){
  folders <- c("SQR_RR_avant1961/", "SQR_RR_additif_20141231/")
  sapply(folders, list.files, pattern = as.character(id), full.names = TRUE)
}

station_list$station_id[1] %>% get_station_files()

get_station_data <- function(id){
  files <- get_station_files(id)
  df <- lapply(files, read.table, sep = ";") %>% 
    do.call(rbind.data.frame, args = .) %>%
    "["(., c(2,3))
  rownames(df) <- NULL
  names(df) <- c(
    "date", 
    paste("rr", id, sep = "_")
  )
  return(df)
}

station_list$station_id[1] %>% get_station_data()


merge_recursion <- function(df_list){
  length_df_list <- length(df_list)
  if(length_df_list > 2){
    merged <- merge_recursion(df_list[-1])
    tmp <- list(df_list[1], merged)
    ans <- merge_recursion(tmp)
  } else {
    merged <- merge(
      df_list[1], df_list[2],
      by = "date", all = TRUE,
      suffixes = rep("", 2)
    )
    ans <- merged
  }
  return(ans)
}
combine_station_data <- function(ids){
  lapply(ids, get_station_data) %>%
    merge_recursion()
}
remove_rows_with_na <- function(df){
  i_row <- apply(df, 1, function(x) all(!is.na(x)))
  df[i_row, ]
}
station_df <- combine_station_data(station_list$station_id) %>%
  remove_rows_with_na()

saveRDS(station_df, file = "station_df.rds")

get_yyyy <- function(yyyymmdd){
  yyyymmdd %/% 10000
}
get_yyyy(station_df$date)

get_mm <- function(yyyymmdd){
  (yyyymmdd %/% 100) %% 100
}
get_mm(station_df$date)

get_dd <- function(yyyymmdd){
  yyyymmdd %% 100
}
get_dd(station_df$date)

compute_rrmax_bymonth <- function(station_df){
  by(
    station_df, 
    INDICES = list(
      get_mm(station_df$date),
      get_yyyy(station_df$date)
    ),
    function(df){
      data.frame(
        yyyy = get_yyyy(df$date[1]),
        mm = get_mm(df$date[1]),
        rr_max =max(df[, -1])
      )
    }
) %>% do.call(rbind.data.frame, args = .)
}
rrmax_bymonth_df <- compute_rrmax_bymonth(station_df)

saveRDS(rrmax_bymonth_df, file = "rrmax_bymonth_df.rds")
