#' Fill missing timestamp
#'
#' Thsi function fills the missing timestamp that are not available in the data
#' due to "no precipitation"
#' @param df (data.frame) data.frame with missing timestamp
#' @export

fill_time<-function(df){
  ts<-suppressWarnings(seq.POSIXt(as.POSIXct(min(df$Timestamp),'%Y-%m-%d %H:%M'), as.POSIXct(max(df$Timestamp),'%m/%d/%y %H:%M'), by="hour"))
  ts<-data.frame(Timestamp=ts)
  dt<- df %>%
    dplyr::bind_rows(ts) %>%
    tidyr::complete(Id, Timestamp,
                    fill = list(Prcp = 0))
}
