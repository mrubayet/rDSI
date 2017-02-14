#' Download Hourly(DSI3240)/15-minute (DSI-3260) Precipitation Data
#'
#' This function downloads digital data set DSI-3240 and DSI-3260 which is hourly and 15-minute precipitation respectively
#' archived at the National Climatic Data Center (NCDC).Data will be downloaded State wise
#' User will need to install 7zip to unzip compressed "tar.Z" raw files.
#' @param type (character) Data type. "dsi3240" for hourly or "dsi3260" for 15-minute. Default: "dsi3240".
#' @param state (character) state name abbreviation. Required if state.code =NULL
#' @param state.code  (Numerical) State code according to the documentation
#' of DSI-3240 and DSI-3260.  Required if state =NULL
#' @references    ftp://ftp.ncdc.noaa.gov/pub/data/hourly_precip-3240/dsi3240.pdf
#' @param dirc (character) path to store downloaded files. Default: current diretectory.
#' @param df (Logical) If TRUE A data.frame containing site id , timestamp , precipitation in inch and flags
#' will be returned. Default: FALSE.
#' @export
#' @import  dplyr
#' @import  readr
#' @importFrom xml2 read_html
#' @importFrom rvest html_text

get_data <- function(type = "dsi3240",state=NULL,state.code=NULL,dirc=getwd(),df = FALSE){
  if(!type %in% c("dsi3240","dsi3260")){
    stop("Invalid type of data mentioned")
  }
  
  if(is.null(state) & is.null(state.code)){
    stop("Please specify state or state code")
  }
  if(is.null(state.code)){
    state.code <- state_code(state)
  }
  
  if (type == "dsi3240"){
    base_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/hourly_precip-3240/"
  }else{
    base_url <- "ftp://ftp.ncdc.noaa.gov/pub/data/15min_precip-3260/"
  }
  if(df){
    dt1<-dsi_1998(type =type,state.code=state.code,dirc=dirc,base_url = base_url,df=df)
    dt2<-dsi_1999_2011(type =type,state.code=state.code,dirc=dirc,base_url = base_url,df=df)
    dt3<-dsi_2012(type =type,state.code=state.code,dirc=dirc,base_url = base_url,df=df)
    dt<-rbind(dt1,dt2,dt3)%>% group_by(Id)%>%arrange(Timestamp)%>%as.data.frame(.)
    return(dt)
  }else{
  dsi_1998(type =type,state.code=state.code,dirc=dirc,base_url = base_url,df=df)
  dsi_1999_2011(type =type,state.code=state.code,dirc=dirc,base_url = base_url,df=df)
  dsi_2012(type =type,state.code=state.code,dirc=dirc,base_url = base_url,df=df)
  }
}

#Downloads and unzip raw "tar.Z"  for pre-1999 files in a specified directory
# @param state.code  (Numerical). State code.  required
# @param dirc (character) path to store downloaded files. Default: current diretectory.
# @param delete (logical) whether or not to delete the raw compressed files.
# @param base_url (character) "ftp://ftp.ncdc.noaa.gov/pub/data/hourly_precip-3240/"
# @export


dsi_1998<-function(type,state.code,dirc=getwd(),base_url,df){
  
  xx=read_html(paste0(base_url,sprintf("%02d",state.code),"/")) %>%
    rvest::html_text() %>%
    regmatches(gregexpr(paste0('(?<=',substr(type,4,7),'_',sprintf("%02d",state.code),'_).*?(?=-1998)'), ., perl=TRUE))
  
  urlpath<- paste0(substr(type,4,7),'_',sprintf("%02d",state.code),"_",as.character(xx),"-",sprintf("%04d",1998))
  datpath<-paste0(dirc,"/",urlpath,".tar.Z")
  
  if (!file.exists(file.path(dirc,urlpath)) & !length(list.files(file.path(dirc,urlpath)))>0){
    url<-paste0(base_url,sprintf("%02d",state.code), "/",urlpath,".tar.Z")
    suppressWarnings(download.file(url,destfile=datpath,mode="wb"))
    suppressWarnings(Decompress7Zip(datpath,file.path(dirc),delete = T))
    suppressWarnings(Decompress7Zip(tools::file_path_sans_ext(datpath),file.path(dirc,urlpath),delete = T))
  }
  if(df){
    files<-data.frame(sr=file.path(dirc,urlpath,list.files(file.path(dirc,urlpath))))
    dt<-files %>% group_by(sr) %>% do(read_f(.$sr)) %>% ungroup() %>% select(-sr) %>%
    group_by(X2) %>% do(tidy_df(.)) %>% ungroup() %>% select(-X2)
    
    return(dt)
  }
}

#Downloads and unzip raw "tar.Z"  for 1999-2011 files in a specified directory
# @param state.code  (Numerical). State code.  required
# @param dirc (character) path to store downloaded files. Default: current diretectory.
# @param delete (logical) whether or not to delete the raw compressed files.
# @param base_url (character) "ftp://ftp.ncdc.noaa.gov/pub/data/hourly_precip-3240/"
# @export

dsi_1999_2011<-function(type,state.code,dirc=getwd(),delete = T,base_url){
  y = data.frame(yr=1999:2011)
  y$urlpath<- paste0(substr(type,4,7),'_',sprintf("%02d",state.code),"_",y$yr,"-",y$yr)
  y$datpath<-paste0(dirc,"/",y$urlpath,".tar.Z")
  y$url<-paste0(base_url,sprintf("%02d",state.code), "/",y$urlpath,".tar.Z")
  
  if (type == "dsi3260"){
    y=y[-nrow(y),]
  }
  
  tryCatch({y%>% group_by(yr)%>% do(suppressMessages(suppressWarnings(download.file(.$url,destfile=.$datpath,mode="wb"))))}, error=function(e){})
  tryCatch({y%>% group_by(yr)%>% do(suppressMessages(suppressWarnings(Decompress7Zip(.$datpath,file.path(dirc),delete = T))))}, error=function(e){})
  tryCatch({y%>% group_by(yr)%>%do(suppressMessages(suppressWarnings(Decompress7Zip(tools::file_path_sans_ext(.$datpath),file.path(dirc,.$urlpath),delete = T))))}, error=function(e){})
  
  if(df){
    files<-y %>% group_by(yr)%>%do(as.data.frame(file.path(dirc,.$urlpath,list.files(file.path(dirc,.$urlpath)))))
    names(files)[c(2)]="sr"
    dt<-files %>% group_by(sr) %>% do(read_f(.$sr)) %>% ungroup() %>% select(-sr) %>%
     group_by(X2) %>% do(tidy_df(.)) %>% ungroup() %>% select(-X2)%>% as.data.frame(.)
   
    return(dt)
  }
}

#Downloads and unzip raw "tar.Z"  for 2012-present files in a specified directory
# @param state.code  (Numerical). State code.  required
# @param dirc (character) path to store downloaded files. Default: current diretectory.
# @param delete (logical) whether or not to delete the raw compressed files.
# @param base_url (character) "ftp://ftp.ncdc.noaa.gov/pub/data/hourly_precip-3240/"
# @export

dsi_2012<-function(type,state.code,dirc=getwd(),base_url){
  y <- data.frame(yr=2011:2013) %>% group_by(yr) %>% do(data.frame(mon=tolower(month.abb))) %>%
    mutate(fold=paste0(dirc,'/',substr(type,4,7),'_',sprintf("%02d",state.code),"_",sprintf("%04d",yr),"-", sprintf("%04d",yr)),
           urlpath=paste0(substr(type,4,7),mon,sprintf("%04d",yr),".dat"),
           datpath = paste0(fold,"/",urlpath),
           url = paste0(base_url,"by_month",sprintf("%04d",yr), "/", urlpath))%>%ungroup()
  if(type=="dsi3240") {
    y<-y %>% filter(!row_number()%in% c(1:11))
  } 
  
  xx = file.path(dirc,(list.files(dirc)))
  tryCatch({y %>%filter(!fold %in% xx) %>%group_by(yr) %>% do(dir.create(unique(.$fold)))}, error=function(e){})
  
  tryCatch({y%>% group_by(yr,mon)%>% do(suppressMessages(suppressWarnings(download.file(.$url,destfile=.$datpath))))}, error=function(e){})
  
  if(df){
   dt<-y %>% group_by(yr,mon) %>% do(read_f(.$datpath)) %>% ungroup() %>% select(-yr,-mon) %>%
     filter(as.numeric(substr(X2,1,2))==state.code)%>% group_by(X2) %>% do(tidy_df(.)) %>%
     ungroup() %>% select(-X2)%>% as.data.frame(.)
   return(dt)
}
}

# Reading the raw data file
# @param x (character) path to the file.
# @return data.frame

read_f<-function(x){
  x<-as.character(x)
  column.widths <- c(3, 6, 2, 4, 2, 4, 2, 4, 3, 4, 6,2, rep(c( 4,6,2), 24))
  df<-suppressMessages(suppressWarnings(readr::read_fwf(x,readr::fwf_widths(column.widths))))
  df<-as.data.frame(apply(df,2,as.character))
  df
}

# A helper function to make the data tidy
# @param df (data.frame) untidy data.frame from read_f
# @return tidy data.frame

tidy_df<-function(df){
  ctime <- seq(from=10, to=84,by=3)
  cprcp <- seq(from=11, to=84,by=3)
  cflag <- seq(from=12, to=84,by=3)
  names(df)[c(1:9)]  = c("Record_Type","Id","coop","HPCP","Element_Units","Year","Month","Day","No._occured")
  names(df)[ctime] = paste0("time_",1:length(ctime))
  names(df)[cprcp] = paste0("prcp_",1:length(cprcp))
  names(df)[cflag] = paste0("flag_",1:length(cflag))

  dtime <- df %>%
    dplyr::select(-coop,-HPCP,-dplyr::contains("prcp"),-dplyr::contains("flag")) %>%
    tidyr::gather(var, Hour, -Record_Type,-Element_Units,-Id, -Year, -Month, -Day,-No._occured) %>%
    dplyr::arrange(Year,Month,Day)%>%
    dplyr::select(-var)

  dprcp <- df %>%
    dplyr::select(-coop,-HPCP,-dplyr::contains("time"),-dplyr::contains("flag")) %>%
    tidyr::gather(var, Prcp, -Record_Type,-Id,-Element_Units, -Year, -Month, -Day,-No._occured) %>%
    dplyr::arrange(Year,Month,Day)

  dflag <- df %>%
    dplyr::select(-coop,-HPCP,-dplyr::contains("prcp"),-dplyr::contains("time")) %>%
    tidyr::gather(var, Flag, -Record_Type,-Id,-Element_Units, -Year, -Month, -Day,-No._occured) %>%
    dplyr::arrange(Year,Month,Day)

  df<-cbind(dtime,Prcp=dprcp$Prcp,Flag=dflag$Flag)
  df<-df[!is.na(df$Hour) & df$Record_Type=="HPD",]
  ix <- as.numeric(which(apply(df, 1, function(x) any(grepl("_", x)))))
  if (length(ix) >0) {
    df <-df[-ix,]
  }
  df[,4:9] = apply(df[,4:9],2,function(x) as.numeric(as.character(x)))

  df<-df %>%
    dplyr::mutate(Hour =floor(Hour/100),Prcp = Prcp/100,
                  Timestamp=as.POSIXct(paste0(Year,"-",Month,"-",Day," ",Hour,":00"),format='%Y-%m-%d %H:%M')) %>%
    dplyr::filter(Hour<25 & !is.na(Timestamp)) %>%
    dplyr::mutate(Prcp=replace(Prcp, Prcp>99, 9999))%>%
    dplyr::select(Id,Timestamp,Prcp,Flag,-Record_Type,-Element_Units,-No._occured,-Year,-Month,-Day,-Hour)%>%
    as.data.frame(.)
  df
}

# Converts the state abbreviation to state code used in dsi3240 dataset
# @param state (character) state name abbreviation.
# @return state code

state_code<-function(state){
  st<-data.frame(name = state.name,abb = state.abb)
  st<-as.data.frame(rbind(st[-c(2,11),],st[c(2,11),]))
  st$code<-c(1:51)[-49]
  state.code<-st[which(st$abb==toupper(state)),3]
  state.code
}


ExecutableFileName7Zip <- function()
{
  executableName <- "C:\\Program Files\\7-Zip\\7z.exe"

  if(file.exists(executableName))
  {
    return (executableName)
  }
  #other executable file names and ideas go here ...
  stop("failed to find 7zip....Install 7zip from http://www.7-zip.org/download.html")
}

# simple function to extract 7zip file
# need to have 7zip installed
RunProcess = function(executable, arguments)
{
  command = paste(sep="", "\"", executable,  "\" ", arguments);

  print (command)

  exitCode = system(command, intern = FALSE, ignore.stdout = FALSE, ignore.stderr = FALSE, wait = TRUE, input = NULL
                    , show.output.on.console = TRUE
                    #, minimized = FALSE
                    , invisible = FALSE
  );
  if(exitCode != 0)
  {
    stop("Process returned error");
  }
  return (exitCode)
}


Decompress7Zip <- function(zipFileName, outputDirectory, delete=FALSE)
{
  executableName <- ExecutableFileName7Zip()

  arguments <- paste(sep="",
                     "e ",
                     "\"", zipFileName, "\" ",
                     "\"-o", outputDirectory, "\" ",
                     "")

  print( arguments)

  RunProcess(executableName, arguments)

  if(delete)
  {
    unlink(zipFileName);
  }
}
