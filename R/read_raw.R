#' Reading downloaded hourly(DSI3240)/15-minute (DSI-3260) Precipitation Data
#'
#' This function reads downloaded and unzipped raw data.
#' @param type (character) Data type. "dsi3240" for hourly or "dsi3260" for 15-minute. 
#' @param state (character) state name abbreviation. Required if state.code =NULL
#' @param state.code  (Numerical) State code according to the documentation
#' of DSI-3240 and DSI-3260.  Required if state =NULL
#' @references    ftp://ftp.ncdc.noaa.gov/pub/data/hourly_precip-3240/dsi3240.pdf
#' @param dirc (character) path to the downloaded files. 
#' @param station_id (character) Station id of for specific station data otherwise will read all the 
#' station data with in the state specified.Default "all".
#' @export
#' @import  dplyr

read_raw<-function(type,state.code,dirc=getwd(),station_id="all"){
   if(!type %in% c("dsi3240","dsi3260")){
    stop("Invalid type of data mentioned")
  }
  
  if(is.null(state) & is.null(state.code)){
    stop("Please specify state or state code")
  }
  if(is.null(state.code)){
    state.code <- state_code(state)
  }
  files<-suppressWarnings(data.frame(sr=list.files(dirc)) %>% 
                            dplyr::filter(grepl(paste0(type,"_"),sr))%>% 
                            dplyr::group_by(sr)%>%
                            do(data.frame(fl=list.files(paste0(dirc,"/",.$sr,"/")))))%>%
    dplyr::mutate(flp=file.path(dirc,sr,fl))%>%
    dplyr::ungroup()
  
  if(station_id!="all"){
    files<-files %>% 
      dplyr::mutate(id = regmatches(fl,gregexpr(paste0("(?<=",type,"_).*?(?=_)"), fl, perl=TRUE)))%>%
      dplyr::filter(id %in% station_id | grepl(".dat",fl))
  }
  print("Reading Raw files............")
  df<-files%>%
    dplyr::group_by(sr,fl)%>%
    do(read_f(.$flp)) %>%           
    dplyr::filter(as.numeric(substr(X2,1,2))==state.code)%>%     
    dplyr::group_by(X2) %>% dplyr::select(-sr,-fl)
  
  print("Tidying up dataframe............")
  df<-df %>%
    do(tidy_df(.)) %>% dplyr::ungroup() %>% dplyr::select(-X2) 
  
  if(station_id!="all"){
    df<-df %>%
      dplyr::filter(Id %in% station_id) %>% data.frame(.)
  }
  
  df
}

