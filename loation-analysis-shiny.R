################################################################################
library(RCurl)
library(httr)
library(jsonlite)
library(glue)

library(tidyverse)
library(data.table)
library(magrittr)
library(stringr)

library(geosphere)
library(leaflet)
library(htmltools)

library(shiny)
library(shinydashboard)
library(DT)

################################################################################
# <1. 학교>
url_file <- 'https://raw.githubusercontent.com/smldlyst/location-analysis/main/data/%EA%B2%BD%EA%B8%B0%EB%8F%84%20%EA%B5%AC%EB%A6%AC%EC%8B%9C_%ED%95%99%EA%B5%90%ED%98%84%ED%99%A9_20220301.csv'
school <- read.csv(url_file, encoding='UTF-8')
school$학교구분 <- NA
school[str_detect(school$학교명, "초등학교"), ]$학교구분 <- "초등학교"
school[str_detect(school$학교명, "중학교"), ]$학교구분 <- "중학교"
school[str_detect(school$학교명, "고등학교"), ]$학교구분 <- "고등학교"
school$학생수_순위 <- order(school$학생수, decreasing=TRUE)
################################################################################
# <2. 아파트>
url_file <- 'https://raw.githubusercontent.com/smldlyst/location-analysis/main/data/apt_total_info.csv'
apt_total_info <- fread(url_file, encoding='UTF-8')
api_key <- "Your_KaKao_API"
responses <- list()
kaptAddr <- apt_total_info$kaptAddr

for (i in 1:length(kaptAddr)) {
  GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
      query = list(query = kaptAddr[i]),
      add_headers(Authorization = paste0("KakaoAK ", api_key))) %>%
    content(as ='text') %>%
    fromJSON() -> responses[i] 
}

# Checking Null List
null.list <- list()
null.idx <- NULL
for (i in 1:length(responses)) {
  if (length(responses[[i]])==0) {
    null.idx <- rbind(null.idx, i)
  }
}
null.idx <- as.vector(null.idx)
null.idx

length(kaptAddr)
length(responses)- length(null.idx)

lon_lat_dt <- list()
for (i in 1:length(kaptAddr)) {
  lon_lat_dt[[i]] <- data.table(addr = responses[[i]]$address$address_name, 
                                long = as.character(responses[[i]]$address$x), 
                                lat = as.character(responses[[i]]$address$y))
}


for (i in 1:length(lon_lat_dt)) {
  if(nrow(lon_lat_dt[[i]]) >= 2) {
    lon_lat_dt[[i]] <- lon_lat_dt[[i]][2,]
  }
}
lon_lat_dt <- lon_lat_dt[-null.idx] 
lon_lat_dt <- rbindlist(lon_lat_dt) # 병합
apt_cmplt <- cbind(apt_total_info[-null.idx, ], lon_lat_dt)

apt_cmplt %<>% as.data.frame
apt_cmplt[,c("long", "lat")] <- sapply(apt_cmplt[,c("long", "lat")], as.numeric)

city.idx <- str_detect(apt_cmplt$kaptAddr, "구리시")
apt_cmplt_slctd <- apt_cmplt[city.idx, ]
################################################################################
# <3. 학원>
url_file <- "https://raw.githubusercontent.com/smldlyst/location-analysis/main/data/2022.%207.%201.%EC%9E%90%20%ED%95%99%EC%9B%90%20%ED%98%84%ED%99%A9(%EA%B5%AC%EB%A6%AC%EC%8B%9C).csv"
academy <- read.csv(url_file, encoding='UTF-8')
str(academy)

api_key <- "Your_KaKao_API"
responses <- list()
acaAddr <- academy$학원주소

for (i in 1:length(acaAddr)) {
  GET(url = 'https://dapi.kakao.com/v2/local/search/address.json',
      query = list(query = acaAddr[i]),
      add_headers(Authorization = paste0("KakaoAK ", api_key))) %>%
    content(as ='text') %>%
    fromJSON() -> responses[i] 
}

# Checking Null List
null.list <- list()
null.idx <- NULL
for (i in 1:length(responses)) {
  if (length(responses[[i]])==0) {
    null.idx <- rbind(null.idx, i)
  }
}
null.idx <- as.vector(null.idx)
null.idx

lon_lat_dt <- list()
for (i in 1:length(acaAddr)) {
  lon_lat_dt[[i]] <- data.table(addr = responses[[i]]$address$address_name, 
                                long = as.character(responses[[i]]$address$x), 
                                lat = as.character(responses[[i]]$address$y))
}
lon_lat_dt <- lon_lat_dt[-null.idx] 
lon_lat_dt <- rbindlist(lon_lat_dt)

academy_cmplt <- cbind(academy[-null.idx, ], lon_lat_dt)
academy_cmplt[,c("long", "lat")] <- sapply(academy_cmplt[,c("long", "lat")], as.numeric)
################################################################################

# Shiny App으로 서비스화 (반경 선택에 따른 테이블 출력 및 기대이익 제공)
school %<>% rename(주소=주.소)
dim(school) # 구리시 학교 32개
school$학교구분 <- NA
school[str_detect(school$학교명, "초등학교"), ]$학교구분 <- "초등학교"
school[str_detect(school$학교명, "중학교"), ]$학교구분 <- "중학교"
school[str_detect(school$학교명, "고등학교"), ]$학교구분 <- "고등학교"

academy_longlat <- academy_cmplt %>% filter(학원명=="영어수학사관학원") %>% select(학원명, long, lat) %>% rename(name=학원명)
school_longlat <- school %>% select(학교명, 경도, 위도) %>% rename(name=학교명, long=경도, lat=위도)
df_longlat <- rbind(academy_longlat, school_longlat)
dist_haversine <- distm(df_longlat %>% select(long, lat), fun = distHaversine)[1,]

df_longlat$dist_haversine <- dist_haversine
df_longlat$dist_hvrsn.tf <- paste0((round(df_longlat$dist_haversine/1000, 2)), "km")
df_longlat$dist_range <- ifelse(df_longlat$dist_haversine < 1000, "1km 이내", 
                                ifelse(df_longlat$dist_haversine <2000, "1km 이상 ~ 2km 이내",
                                       ifelse(df_longlat$dist_haversine < 3000, "2km 이상 ~ 3km 이내", "3km 이상")))


df_longlat %<>% filter(name!="영어수학사관학원") 

school$반경 <- df_longlat$dist_haversine
school$거리 <- df_longlat$dist_hvrsn.tf
school$거리범위 <- df_longlat$dist_range

################################################################################

# Marker별 색깔 입히기
icon.standard <- makeAwesomeIcon(library = "fa",
                                 icon = "flag",
                                 markerColor = "red", iconColor = "black")

icon.apt <- makeAwesomeIcon(library="fa",
                            icon="home",
                            markerColor="lightgreen", iconColor="black")

icon.sch <- makeAwesomeIcon(library="fa",
                            icon="institution",
                            markerColor="white", iconColor="royalblue")


icon.acd <- makeAwesomeIcon(library="fa",
                            icon="map-marker",
                            markerColor="lightred", iconColor="black")



# Marker별 컨텐츠 출력할 정보 할당
labs_apt <- lapply(seq(nrow(apt_cmplt_slctd)), function(i) {
  paste0( '<p><b>', '아파트명 : ', apt_cmplt_slctd[i, "kaptName"], '</b></p>',
          '<p><b>', '세대수 : ', apt_cmplt_slctd[i, "kaptdaCnt"], '</b></p>',
          '<p><b>', '인접교육시설 :', apt_cmplt_slctd[i, "educationFacility"], '</b></p>' ) 
})

# 동구중 Error 추가
labs_dg_ms <- paste0( '<p><b>', '학교명 : ', school[21, "학교명"], '</b></p>', 
                      '<p><b>', '학생수 :', school[21, "학생수"], '</b></p>' )

labs_sch <- lapply(seq(nrow(school)), function(i) {
  paste0( '<p><b>', '학교명 : ', school[i, "학교명"], '</b></p>', 
          '<p><b>', '학생수 :', school[i, "학생수"], '</b></p>' )
})

labs_acd <- lapply(seq(nrow(academy_cmplt)), function(i) {
  paste0( '<p><b>', '학원명 : ', academy_cmplt[i, "학원명"], '</b></p>')
})

# 원화 숫자를 한글로 표기 
price.hangeul <- function(price) {
  price <- as.character(price)
  str.length <- nchar(price)
  
  if (str.length <= 4) {
    print(
      paste0(price, "원")
    ) }
  
  else if (str.length <= 8) {
    if(str.length == 5){
      paste0(str_sub(price, 1, 1), "만원")
    }
    else if(str.length == 6){
      paste0(str_sub(price, 1, 2), "만원")
    }
    else if(str.length == 7){
      paste0(str_sub(price, 1, 3), "만원")
    }
    else {
      paste0(str_sub(price, 1, 4), "만원")
    }
  }
  
  else if (str.length <= 12) {
    if(str.length == 9){
      paste0(str_sub(price, 1, 1), "억 ", str_sub(price, 2, 5), "만원")
    }
    else if(str.length == 10){
      paste0(str_sub(price, 1, 2), "억 ", str_sub(price, 3, 6), "만원")
    }
    else if(str.length == 11){
      paste0(str_sub(price, 1, 3), "억 ", str_sub(price, 4, 7), "만원")
    }
    else {
      paste0(str_sub(price, 1, 4), "억 ", str_sub(price, 5, 8), "만원")
    }
  }
  
  else if (str.length <= 16) {
    if(str.length == 13){
      paste0(str_sub(price, 1, 1), "조 ", str_sub(price, 2, 5), "억원")
    }
    else if(str.length == 14){
      paste0(str_sub(price, 1, 2), "조 ", str_sub(price, 3, 6), "억원")
    }
    else if(str.length == 15){
      paste0(str_sub(price, 1, 3), "조 ", str_sub(price, 4, 7), "억원")
    }
    else {
      paste0(str_sub(price, 1, 4), "조 ", str_sub(price, 5, 8), "억원")
    }
  }
  
  else
    print(
      paste0(price, "원")
    )
}
################################################################################
library(shiny)
library(shinydashboard)
library(htmltools)


# UI
ui <- dashboardPage(
  
  skin = "black",
  
  dashboardHeader(title = HTML(paste('<b>', "빅위즈덤's 입지분석 서비스", '</b>'))),
  
  dashboardSidebar(),
  
  dashboardBody(
    # Boxes need to be put in a row (or column)
    fluidRow(
      
      box(
        title ="1. 대상 반경을 설정합니다.",
        width=2,
        selectInput(inputId = "dist_range",
                    label = "학원으로부터 거리를 선택하세요.",
                    choices = sort(unique(df_longlat$dist_range)))
      ),
      
      
      box(
        title = "2. 수익을 설정합니다.",
        width=2,
        textInput(inputId = "revenue",
                  label = HTML(paste("학생 1명당 수업료를 설정하세요.", "<br>", "*(단위 : 원)")),
                  value=350000)
      ),
      
      
      box(
        title = "3. 목표하는 고객 수를 설정합니다.",
        width=2,
        textInput(inputId = "target_raito",
                  label = HTML(paste("목표고객 비율을 설정하세요.", "<br>", "*(단위 : %)")),
                  value=10)
      ),
      
      box(leafletOutput("map"
                        # width = 20
                        # height = 500,
      )
      ),
      
      
      box(dataTableOutput("table1"),
          width=8
      ),
      
      box(dataTableOutput("table2"),
          width=3
      ),
      
      box(dataTableOutput("table3"),
          width=3
      )
    )
  )
)

# Server
server <- function(input, output) {
  
  output$table1 <- renderDataTable({
    
    target_prop <- as.numeric(input$target_raito)/100 
    target_sales <- as.numeric(input$revenue)
    slctd_schname <- df_longlat[which(df_longlat$dist_range == input$dist_range), ] %>% select(name) %>%
      unlist() %>% as.vector()
    
    output_dt <- school %>%
      filter(학교명 %in% slctd_schname) %>%
      mutate(목표_학생수 = round(학생수 * target_prop, 0)) %>% 
      mutate(기대_매출액 = target_sales * 목표_학생수) %>%
      # mutate(기대_매출액 = price.hangeul(기대_매출액)) %>%
      select(학교명, 학교구분,
             주소, 거리,
             학생수, 목표_학생수, 기대_매출액) %>%
      arrange(desc(기대_매출액))
    
  })
  
  output$table2 <- renderDataTable({
    
    target_prop <- as.numeric(input$target_raito)/100  
    target_sales <- as.numeric(input$revenue)
    slctd_schname <- df_longlat[which(df_longlat$dist_range == input$dist_range), ] %>% select(name) %>%
      unlist() %>% as.vector()
    
    output_dt2 <- school %>%
      filter(학교명 %in% slctd_schname) %>%
      mutate(목표_학생수 = round(학생수 * target_prop, 0)) %>% 
      mutate(기대_매출액 = target_sales * 목표_학생수) %>%
      select(학교명, 학교구분,
             주소, 거리,
             학생수, 목표_학생수, 기대_매출액) %>%
      group_by(학교구분) %>%
      summarise(
        개수 = n(),
        구분별_기대_매출액 = price.hangeul(sum(기대_매출액))) %>% 
      arrange(desc(학교구분))
  })
  
  
  output$table3 <- renderDataTable({
    
    target_prop <- as.numeric(input$target_raito)/100  
    target_sales <- as.numeric(input$revenue)
    slctd_schname <- df_longlat[which(df_longlat$dist_range == input$dist_range), ] %>% select(name) %>%
      unlist() %>% as.vector()
    
    output_dt3 <- school %>%
      filter(학교명 %in% slctd_schname) %>%
      mutate(목표_학생수 = round(학생수 * target_prop, 0)) %>% 
      mutate(기대_매출액 = target_sales * 목표_학생수) %>%
      select(기대_매출액) %>%
      summarise(기대_매출액_합계 = price.hangeul(sum(기대_매출액)))
  })
  
  
  output$map <- renderLeaflet({
    
    # radius Option
    if (input$dist_range == "1km 이내") {
      radius = 1000
    } else if (input$dist_range == "1km 이상 ~ 2km 이내"){
      radius = 2000
    } else if (input$dist_range == "2km 이상 ~ 3km 이내"){
      radius = 3000
    } else
      radius = 5000
    
    # zoom Option
    if (input$dist_range == "1km 이내") {
      zoom = 14
    } else if (input$dist_range == "1km 이상 ~ 2km 이내"){
      zoom = 14
    } else if (input$dist_range == "2km 이상 ~ 3km 이내"){
      zoom = 13
    } else
      zoom = 13
    
    leaflet() %>% addTiles() %>% 
      
      # 시야
      setView(lng = 127.138489,
              lat = 37.607843,
              zoom = zoom) %>%
      
      
      # 학원 Marker
      addAwesomeMarkers(lng = 127.138489,
                        lat = 37.607843,
                        icon = icon.standard,
                        label = "영수사관학원") %>%
      
      
      
      # 학원 Rectangle
      addRectangles(
        lng1=127.138406, lat1=37.607860,
        lng2=127.138594, lat2=37.607834,
        fillColor = "transparent") %>% 
      
      # 학원 근처 반경 표시
      addCircles(lng = 127.138489, 
                 lat = 37.607843,
                 radius = radius) %>% 
      
      
      # 학원위치 Popup 표시
      addPopups(lng = 127.138489, lat = 37.607843,
                popup="<b>영어수학사관학원</b>",
                options = popupOptions(closeButton = TRUE)) %>% 

      # 경쟁사(학원) Marker 표시
      addAwesomeMarkers(lng = academy_cmplt$long,
                        lat = academy_cmplt$lat,
                        label = lapply(labs_acd, htmltools::HTML),
                        icon = icon.acd)

      
      # 아파트 Marker
      addAwesomeMarkers(lng = apt_cmplt$long,
                        lat = apt_cmplt$lat,
                        label = lapply(labs_apt, htmltools::HTML),
                        icon = icon.apt) %>% 
      
      # 학교 Marker
      addAwesomeMarkers(lng = school$경도, 
                        lat = school$위도,
                        label = lapply(labs_sch, htmltools::HTML),
                        icon = icon.sch) %>% 
      
      # (+동구중)학교 Marker  (leaflet에서 위경도 Error로 인해 구글맵 위경도 찾아 수정하여 제공)
      addAwesomeMarkers(lng = 127.135112, # 구글맵
                        lat = 37.612591, # 구글맵
                        label = htmltools::HTML(labs_dg_ms),
                        icon = icon.sch)
    
    
  })
}

shinyApp(ui, server)
################################################################################

