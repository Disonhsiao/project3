#筆電
#setwd("D:/大學資料_note/四下/巨量資料分析/期末專題")
#桌電
setwd("D:/DeskTopThings/大學資料/四下/巨量資料分析/期末專題")
invisible(lapply(c("plyr", "magrittr", "rvest", "ggrepel", "ggplot2"), library, character.only = TRUE))


#名稱對照txt
#宜蘭
DataCheck_Yilan <-  read.table("DataCheck_Yilan.txt", header = TRUE, encoding = "UTF-8", sep = ",")
#宜蘭測站檔案名稱
yilan_FilePosName <- DataCheck_Yilan$FileName
#宜蘭測站英文名稱
yilan_posname <- DataCheck_Yilan$EnglishName
#宜蘭各測站海拔高度
yilan_posHeight <- DataCheck_Yilan$PosHeight_m

#臺南
DataCheck_Tainan <- read.table("DataCheck_Tainan.txt", header = TRUE, encoding = "UTF-8", sep = ",")
#測站檔案名稱
Tainan_FilePosName <- DataCheck_Tainan$FileName
#測站英文名稱
Tainan_posname <- DataCheck_Tainan$EnglishName
#各測站海拔高度
Tainan_posHeight <- DataCheck_Tainan$PosHeight_m

#澎湖
DataCheck_Penghu <- read.table("DataCheck_Penghu.txt", header = TRUE, encoding = "UTF-8", sep = ",")
#測站檔案名稱
Penghu_FilePosName <- DataCheck_Penghu$FileName
#測站英文名稱
Penghu_posname <- DataCheck_Penghu$EnglishName
#各測站海拔高度
Penghu_posHeight <- DataCheck_Penghu$PosHeight_m

#臺南
DataCheck_Changhua <- read.table("DataCheck_Changhua.txt", header = TRUE, encoding = "UTF-8", sep = ",")
#測站檔案名稱
Changhua_FilePosName <- DataCheck_Changhua$FileName
#測站英文名稱
Changhua_posname <- DataCheck_Changhua$EnglishName
#各測站海拔高度
Changhua_posHeight <- DataCheck_Changhua$PosHeight_m



#-------------------------------
#取得各數據十年平均(num是取第幾行資料<依照CSV檔內的行>, mod分1(正常)跟2(每月平均), region是宜蘭1or臺南2or澎湖3or彰化4, 測站)
GetData10YearAvg <- function(num, mod, region, pos)
{
  #region對照名稱
  regionDic <- c("1" = "宜蘭", "2" = "臺南", "3" = "澎湖", "4" = "彰化")
  #設定路徑到資料夾
  if(as.character(region) %in% names(regionDic) == FALSE)
  {
    cat("Region Error! 1> Yilan 2>Tainan 3>Penghu 4>Changhua\n")
    return (NULL)
  }
  #筆電
  #setwd(sprintf("D:/大學資料_note/四下/巨量資料分析/期末專題/%s", regionDic[as.character(region)]))
  #桌電
  setwd(sprintf("D:/DeskTopThings/大學資料/四下/巨量資料分析/期末專題/%s", regionDic[as.character(region)]))
  
  num <- as.integer(num)
  if(num < 2 | num > 35)
  {
    cat("Please type in number from 2 to 35!\n")
    return (NULL)
  }
  
  #取得資料
  
  DF <- aaply(pos, mod ,num, region, .margins = 1, .fun = Data_observePos)
  
  #資料轉為DF
  if(num == 15 & mod == 2) #四季風向
  {
    if(ncol(DF) != 4)
      DF <- data.frame(t(DF))
  }
  else
    DF <- data.frame(DF)
  
  #取得每個測站的10年完全平均，幾個測站最後就出幾個值
  if(mod == 1)
  {
    if(num == 2) #測站平地壓力
      colnames(DF) <- c("PosPress")
    else if(num ==8 ) # 氣溫
      colnames(DF) <- c("Temperature")
    else if(num == 15 ) #風向
      colnames(DF) <- c("WindDir")
    else
    {
      colnames(DF) <- c("Unknown")
      print(sprintf("%d: The data of the number has not been assigned!", num))
    }
  }
  #每月10年測站平均值
  else if(mod == 2)
  {
    if(num == 19 | num == 8 | num == 2) #模式2下雨量跟氣溫跟氣壓的每月要特別調整型態成三行
    {
      #return(DF)
      #DF <- data.frame(matrix(DF, ncol = 3))
      DF <- data.frame(matrix(unlist(DF), ncol = 3))
      DF[[2]] <- factor(DF[[2]], levels = c(1:12))
    }
    
    if(num == 19) #降雨量 要獨立資料，因為是取1~12月個別平均
      colnames(DF) <- c("PosName", "Month", "Rainfall_mm")
    else if( num == 8) #氣溫 要獨立資料，因為是取1~12月個別平均
      colnames(DF) <- c("PosName", "Month", "Temprature_C")
    else if(num == 2) #平地壓力 要獨立資料，因為是取1~12月個別平均
      colnames(DF) <- c("PosName", "Month", "PosPress")
    else if (num == 15) #風向
      colnames(DF) <- c("Wind_Winter", "Wind_Spring", "Wind_Summer", "Wind_Auturn")
    else
    {
      colnames(DF) <- c("Unknown")
      print(sprintf("%d: The data of the number has not been assigned!", num))
    }
  }
  return (DF)
}
#第幾個測站(測站編號, mode, 要取的資料)
Data_observePos <- function(pos, mod, num, region)
{
  #該測站每年的個別平均值
  posData <- aaply(c(11:20), pos, mod, num, region, .margins = 1, .fun = Data_year)
  #每次回傳該測站的十年平均值
  # if(num == 15) #風向有四行，故要用colmeans
  #   return (round(colMeans(posData, na.rm = TRUE) , 2))

  if( mod == 1)
  {
    return (round( mean(posData, na.rm = TRUE) ,2))
  }
  else if (mod == 2)
  {
    if ( num == 19 | num == 8 | num == 2) # 回傳該測站各1~12月的10年平均雨量/氣溫/氣壓
    {
      posData <- round(colMeans(posData, na.rm = TRUE), 2) #各月的10年平均
      mat <- matrix(posData)
      if(region == 1)
        mat <- cbind(yilan_posname[pos], c(1:12), mat)
      else if (region == 2)
        mat <- cbind(Tainan_posname[pos], c(1:12), mat)
      else if (region == 3)
        mat <- cbind(Penghu_posname[pos], c(1:12), mat)
      else if (region == 4)
        mat <- cbind(Changhua_posname[pos], c(1:12), mat)
      return(mat)
    }
    else if(num == 15) #風向有四行，故要用colmeans
      return (round(colMeans(posData, na.rm = TRUE) , 2))
  }
}
#該測站第幾年資料(年, 測站, 數據)
Data_year <- function(year, pos, mod, num, region)
{
  #csv檔案名稱
  if(region == 1)
    fileName <- sprintf("./%s-20%02s.csv", yilan_FilePosName[pos], year)
  else if (region == 2)
    fileName <- sprintf("./%s-20%02s.csv", Tainan_FilePosName[pos], year)
  else if (region == 3)
    fileName <- sprintf("./%s-20%02s.csv", Penghu_FilePosName[pos], year)
  else if (region == 4)
    fileName <- sprintf("./%s-20%02s.csv", Changhua_FilePosName[pos], year)
  #temp
  temp <- read.table(fileName, header = TRUE, encoding = "UTF-8", sep = ",")[[num]][2:13] #因為第一列是英文名，故取2~13列
  #若數值是... (沒有數值)，則用NA取代
  temp[temp == "..." | temp == "&" | temp == "/" | temp == "X" | temp == "T"] <- NA
  #字串轉換為數字
  temp <- as.numeric(temp)
  
  #風向做四季個別平均
  # if( num == 15) #風向 直接回傳1~12月風向
  #   return (temp)
  if( num == 15 ) #風向，有四行
    data <- c(mean(temp[1:3], na.rm = TRUE), mean(temp[4:6], na.rm = TRUE), mean(temp[7:9], na.rm = TRUE), mean(temp[10:12], na.rm = TRUE))
  else
  {
    if(mod == 1)#做該年資料平均
    {
      data <- mean(temp, na.rm = TRUE)
    }
    else if (mod == 2) #直接回傳該年1~12月數值
    {
      if( num == 19 | num == 8 | num == 2) #雨量/氣溫，直接回傳該年的1~12月數值
        data <- temp
    }
  }
  return (data)
}

#分群繪製雨量圖(DF)
PaintRainfall <- function(rainfallDF)
{
  rainfallDF$PosName <- factor(rainfallDF$PosName, levels = rainfallDF$PosName[1: (nrow(rainfallDF)/12) ])
  rainfallDF$Month <- factor(rainfallDF$Month, levels = c(1:12))
  rainfallDF$Rainfall_mm <- as.numeric(rainfallDF$Rainfall_mm)
  ggplot(rainfallDF, aes(x = Month, y = Rainfall_mm, group = PosName, color = PosName)) + geom_point() + geom_line() + labs(title = "Yilan_Pos vs RainFall in Months (10yearsAvg)")
}

#ddply 依照月份將該區全測站依月份做平均(df, 地區名稱, 降雨=19 氣溫=8 氣壓=2)
AllPosAvg <- function(df, regionName, num)
{
  df[[3]] <- as.numeric(df[[3]])
  value <- round(mean(df[[3]], na.rm = TRUE),2)
  num <- as.character(num)
  cName <- c("19"= "Rainfall_mm", "8"="Temperature_C", "2"="PosPress_hPa")
  newdf <- data.frame(RegionName = regionName, Month = df$Month[1], value)
  colnames(newdf)[3] <- cName[num]
  return (newdf)
}

#畫風向玫瑰圖
Rose <- function(posName)
{
  url_start <- c("Yilan" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=467080&stname=%25E5%25AE%259C%25E8%2598%25AD&datepicker=",
                 "Taiping" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0U710&stname=%25E5%25A4%25AA%25E5%25B9%25B3%25E5%25B1%25B1&datepicker=",
                 "Wujie" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0U780&stname=%25E4%25BA%2594%25E7%25B5%2590&datepicker=",
                 "Dong-ao" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0U760&stname=%25E6%259D%25B1%25E6%25BE%25B3&datepicker=",
                 "Nanao" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0U770&stname=%25E5%258D%2597%25E6%25BE%25B3&datepicker=",
                 "Yuanyang" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0UA10&stname=%25E9%25B4%259B%25E9%25B4%25A6%25E6%25B9%2596&datepicker=",
                 "Yulan" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0U650&stname=%25E7%258E%2589%25E8%2598%25AD&datepicker=",
                 "Bailing" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0UA30&stname=%25E7%2599%25BD%25E5%25B6%25BA&datepicker=",
                 "Xideshan" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0UA40&stname=%25E8%25A5%25BF%25E5%25BE%25B7%25E5%25B1%25B1&datepicker=",
                 "Ximaoshan" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0UA50&stname=%25E8%25A5%25BF%25E5%25B8%25BD%25E5%25B1%25B1&datepicker=",
                 "Zhangshushan" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0UA60&stname=%25E6%25A8%259F%25E6%25A8%25B9%25E5%25B1%25B1&datepicker=",
                 "Yujing" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0O930&stname=%25E7%258E%2589%25E4%25BA%2595&datepicker=",
                 "Guanshan" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C1O880&stname=%25E9%2597%259C%25E5%25B1%25B1&datepicker=",
                 "Tainan" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=467410&stname=%25E8%2587%25BA%25E5%258D%2597&datepicker=",
                 "Beimen" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0X290&stname=%25E5%258C%2597%25E9%2596%2580&datepicker=",
                 "Madou" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0X120&stname=%25E9%25BA%25BB%25E8%25B1%2586&datepicker=",
                 "Beiliao" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0O830&stname=%25E5%258C%2597%25E5%25AF%25AE&datepicker=",
                 "Houbi" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0X260&stname=%25E5%25BE%258C%25E5%25A3%2581&datepicker=",
                 "Baihe" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0X210&stname=%25E7%2599%25BD%25E6%25B2%25B3&datepicker=",
                 "Yuanlin" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0G650&stname=%25E5%2593%25A1%25E6%259E%2597&datepicker=",
                 "Xiushui" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0G780&stname=%25E7%25A7%2580%25E6%25B0%25B4&datepicker=",
                 "Erlin" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0G730&stname=%25E4%25BA%258C%25E6%259E%2597&datepicker=",
                 "Beidou" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0G840&stname=%25E5%258C%2597%25E6%2596%2597&datepicker=",
                 "Ershui" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0G880&stname=%25E4%25BA%258C%25E6%25B0%25B4&datepicker=",
                 "Lukang" = "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=C0G640&stname=%25E9%25B9%25BF%25E6%25B8%25AF&datepicker=",
                 "Penghu"= "https://e-service.cwb.gov.tw/HistoryDataQuery/MonthDataController.do?command=viewMain&station=467350&stname=%25E6%25BE%258E%25E6%25B9%2596&datepicker="
                 )
  
  #台南/彰化一次取全測站數值
  if(posName == "TainanALL")
    url <- url_start[12:19]
  else if(posName == "ChanghuaALL")
    url <- url_start[20:25]
  else 
    url <- url_start[posName]
  
  DF <- adply(url, .margins = 1, .fun = "Rose1", .id = NULL)
  
  DF$WindSpd_N <- cut(DF$WindSpd,
                      breaks = c(0, 2.1, 4.1,  6.1, Inf),
                      labels = c("0~2", "2.1~4", "4.1~6", ">6.1"),
                      right = F)
  DF$WindDir_N <- cut(DF$WindDir,
                      breaks = c(0, 11.26, 33.76, 56.26, 78.76, 101.26, 127.76, 146.26, 168.76, 191.26, 213.76, 236.26, 258.76, 281.26, 303.76, 326.26, 348.75, 360),
                      labels = c("N","NNE","NE","ENE","E","ESE","ES","SSE","S","SWS","SW","WSW","W","WNW","NW","NNW","N"),
                      include.lowest = TRUE)
  
  #季節分群轉為list並繪圖存圖
  lis <- dlply(DF, posName, .variables = "Season", .fun = "Rose5")
  return (lis)
}
Rose1 <- function(url_start)
{
  #年分2011-2020
  y <- rep(2011:2020, each = 12)
  #每年12個月的資料
  m <- rep(1:12, length = 12*10)
  ym <- sprintf("%d%s%02d", y, "-", m)
  #全部資料的url vector
  urls <- paste0(url_start, ym, "#")
  seasons <- c("Winter", "Spring", "Summer", "Auturm")
  
  urlDF <- data.frame("Urls" = urls, "Season" = rep(seasons, each = 3))
  urlDF$Season <- factor(urlDF$Season, seasons)
  
  DF <- ddply(urlDF, .variables = "Season", .fun = Rose2, .id = NULL)
  return (DF)
}
Rose2 <- function(url) #adply 一次接收所有年的以季分群(春or夏or秋or冬)
{
  DF <- adply(url$Urls, .margins = 1, .fun = "Rose3", .id = NULL)
  return (DF)
}
Rose3 <- function(url) #adply 接收每個月url
{
  print(url)
  windSpd<- Rose4(url, 17)
  windDir<- Rose4(url, 18)
  DF <- data.frame("WindSpd" = windSpd, "WindDir" = windDir)
  return (DF)
}
#取風速7or風向8
Rose4 <- function(urll, num)
{
  temp <- read_html(urll) %>%
    html_nodes(xpath = paste0("//td[(((count(preceding-sibling::*) + 1) = ", num, ") and parent::*)]")) %>%
    html_text(trim	= T) #trim是否去掉空格
  #若數值是... (沒有數值)，則用NA取代
  temp[temp == "..." | temp == "&" | temp == "/" | temp == "X" | temp == "T"] <- NA
  #刪除第一個元素並轉為數值
  temp <- as.numeric(temp)
  
  return(temp)
}
#分季節畫圖存圖
Rose5 <- function(df, name) #dlply獲得季節分群df, 並最後回傳含四季df的list
{
  df <- na.omit(df)
  #畫圖
  print(ggplot(data = df, aes(x = WindDir_N, fill = WindSpd_N))+
          geom_bar( position = position_stack(reverse = TRUE)) +
          theme_bw() +
          scale_fill_discrete(guide = guide_legend(reverse=TRUE), name = "Wind Speed (m/s)")+
          coord_polar(start = -(360/16 * pi/180)/2) +
          xlab("") + labs(title = sprintf("%s_Wind_%s", name, df$Season[1])))
  
  #存圖
  #季節file檔名
  setwd("D:/DeskTopThings/大學資料/四下/巨量資料分析/期末專題/匯出圖")
  SeansonName <- c("Winter"="1Winter", "Spring"="2Spring", "Summer"="3Summer", "Auturm"="4Auturm")
  ggsave(filename = sprintf("Rose_%s_Wind_%s.png", name, SeansonName[df$Season[1]]))
  return(df)
}

#-------------------------------
#建立宜蘭總資料(測站名，風向，氣壓，氣溫，高度，測站名+高度)
Yilan_Data <- data.frame("PosName" = yilan_posname ,GetData10YearAvg(15, 2, 1, c(1:11)), GetData10YearAvg(2, 1, 1, c(1:11)), GetData10YearAvg(8, 1, 1, c(1:11)), "PosHeight" = yilan_posHeight, "NamePosHeight" = paste(yilan_posname, yilan_posHeight))
#降雨資料
Yilan_Data_Rainfall <- GetData10YearAvg(19, 2, 1, c(1:11))
#氣壓高度折線圖
#高度factor從高度小到大
Yilan_Data$NamePosHeight <- factor(Yilan_Data$NamePosHeight, levels = Yilan_Data$NamePosHeight[order(Yilan_Data$PosHeight)])
ggplot(Yilan_Data, aes(x = NamePosHeight, y = PosPress, group = 1)) + geom_point() + geom_line() + scale_x_discrete(guide = guide_axis(n.dodge = 3))
#氣壓高度散佈圖
ggplot(Yilan_Data, aes(x = PosHeight, y = PosPress, label = NamePosHeight, group = 1)) + geom_text_repel()  + geom_point(color = "red") + geom_line() +　labs(x = "PosHeight(m)", y = "PosPressAvg(hPa)", title = "Yilan_Press vs Height") + geom_smooth(method = 'lm')
lm(Yilan_Data$PosPress ~ Yilan_Data$PosHeight)
#溫度高度散佈圖
ggplot(Yilan_Data, aes(x = PosHeight, y = Temperature, label = NamePosHeight, group = 1)) + geom_text_repel()  + geom_point(color = "red") + geom_line() +　labs(x = "PosHeight(m)", y = "Temperature((℃)", title = "Yilan_Temperature vs Height") + geom_smooth(method = 'lm')
lm(Yilan_Data$Temperature ~ Yilan_Data$PosHeight)
#月份降雨折線圖
Yilan_Data_Rainfall$PosName <- factor(Yilan_Data_Rainfall$PosName, levels = yilan_posname)
Yilan_Data_Rainfall$Month <- factor(Yilan_Data_Rainfall$Month, levels = c(1:12))
Yilan_Data_Rainfall$Rainfall_mm <- as.numeric(Yilan_Data_Rainfall$Rainfall_mm)
ggplot(Yilan_Data_Rainfall, aes(x = Month, y = Rainfall_mm, group = PosName, color = PosName)) + geom_point() + geom_line() + labs(title = "Yilan_Pos vs RainFall in Months (10yearsAvg)")  #+ scale_x_discrete(guide = guide_axis(n.dodge = 3))
#月份降雨折線圖(測站分群)
PaintRainfall(GetData10YearAvg(19, 2, 1, c(2, 8, 6, 1)))
PaintRainfall(GetData10YearAvg(19, 2, 1, c(10, 9, 1)))
PaintRainfall(GetData10YearAvg(19, 2, 1, c(5, 1, 3, 4)))


#------------------------------------------------------------
#建立臺南總資料
Tainan_Data <- data.frame("PosName" = Tainan_posname, GetData10YearAvg(2, 1, 2, c(1:8)) , "PosHeight" = Tainan_posHeight, "NamePosHeight" = paste(Tainan_posname, Tainan_posHeight))
#降雨資料
Tainan_Data_Rainfall <- GetData10YearAvg(19, 2, 2, c(1:8))

#月份降雨折線圖
Tainan_Data_Rainfall$PosName <- factor(Tainan_Data_Rainfall$PosName, levels = Tainan_posname)
Tainan_Data_Rainfall$Month <- factor(Tainan_Data_Rainfall$Month, levels = c(1:12))
Tainan_Data_Rainfall$Rainfall_mm <- as.numeric(Tainan_Data_Rainfall$Rainfall_mm)
ggplot(Tainan_Data_Rainfall, aes(x = Month, y = Rainfall_mm, group = PosName, color = PosName)) + geom_point() + geom_line() + labs(title = "Tainan_Pos vs RainFall in Months (10yearsAvg)")


#-------------------------------------------------------------
#澎湖
#降雨資料
Penghu_Data_Rainfall <- GetData10YearAvg(19, 2, 3, 1)

#月份降雨
Penghu_Data_Rainfall$PosName <- factor(Penghu_Data_Rainfall$PosName, levels = Penghu_posname)
Penghu_Data_Rainfall$Month <- factor(Penghu_Data_Rainfall$Month, levels = c(1:12))
Penghu_Data_Rainfall$Rainfall_mm <- as.numeric(Penghu_Data_Rainfall$Rainfall_mm)


#-------------------------------------------------------------
#風花圖

#宜蘭站
# Rose_Yilan <- Rose("Yilan")
# #台南全站
# Rose_Tainan <- Rose("TainanALL")
# #彰化全站
# Rose_Changhua <- Rose("ChanghuaALL")
# #澎湖站
# Rose_Penghu <- Rose("Penghu")

#宜蘭各站
# Rose_Yilan_Taiping <- Rose("Taiping")
# Rose_Yilan_Wujie <- Rose("Wujie")
# assign("Rose_Yilan_Dong-ao", Rose("Dong-ao"))
# Rose_Yilan_Nanao <- Rose("Nanao")
# Rose_Yilan_Yuanyang <- Rose("Yuanyang")
# Rose_Yilan_Yulan <- Rose("Yulan")
# Rose_Yilan_Bailing <- Rose("Bailing")
# Rose_Yilan_Xideshan <- Rose("Xideshan")
# Rose_Yilan_Ximaoshan <- Rose("Ximaoshan")
# Rose_Yilan_Zhangshushan <- Rose("Zhangshushan")


#台南各站
# Rose_Tainan_1 <- Rose("Yujing")
# Rose_Tainan_2 <- Rose("Guanshan")
# Rose_Tainan_3 <- Rose("Tainan")
# Rose_Tainan_4 <- Rose("Beimen")
# Rose_Tainan_5 <- Rose("Madou")
# Rose_Tainan_6 <- Rose("Beiliao")
# Rose_Tainan_7 <- Rose("Houbi")
# Rose_Tainan_8 <- Rose("Baihe")


#-------------------------------------------------------------
#建立共存資料
#宜蘭臺南澎湖彰化四地區全測站每月雨量平均
YNPC_Rainfall <- rbind( ddply(GetData10YearAvg(19, 2, 1, c(1:11)), "Yilan", 19, .variables = "Month", .fun = "AllPosAvg")
                     , ddply(GetData10YearAvg(19, 2, 2, c(1:8)), "Tainan", 19, .variables = "Month", .fun = "AllPosAvg")
                     , ddply(GetData10YearAvg(19, 2, 3, c(1)), "Penghu", 19, .variables = "Month", .fun = "AllPosAvg")
                     , ddply(GetData10YearAvg(19, 2, 4, c(1:6)), "Changhua", 19, .variables = "Month", .fun = "AllPosAvg"))

#宜蘭台南澎湖彰化四地區全測站每月氣溫平均
YNPC_Temperature <- rbind( ddply(GetData10YearAvg(8, 2, 1, c(1:11)), "Yilan", 8, .variables = "Month", .fun = "AllPosAvg")
                    , ddply(GetData10YearAvg(8, 2, 2, c(1:8)), "Tainan", 8, .variables = "Month", .fun = "AllPosAvg")
                    , ddply(GetData10YearAvg(8, 2, 3, c(1)), "Penghu", 8, .variables = "Month", .fun = "AllPosAvg")
                    , ddply(GetData10YearAvg(8, 2, 4, c(1:6)), "Changhua", 8, .variables = "Month", .fun = "AllPosAvg"))

#宜蘭台南澎湖彰化四地區全測站每月氣壓平均
YNPC_PosPress <- rbind( ddply(GetData10YearAvg(2, 2, 1, c(1:11)), "Yilan", 2, .variables = "Month", .fun = "AllPosAvg")
                        , ddply(GetData10YearAvg(2, 2, 2, c(1:8)), "Tainan", 2, .variables = "Month", .fun = "AllPosAvg")
                        , ddply(GetData10YearAvg(2, 2, 3, c(1)), "Penghu", 2, .variables = "Month", .fun = "AllPosAvg")
                        , ddply(GetData10YearAvg(2, 2, 4, c(1:6)), "Changhua", 2, .variables = "Month", .fun = "AllPosAvg"))

YNPC_data <- cbind(YNPC_Rainfall, "Temperature_C" =YNPC_Temperature$Temperature_C, "PosPress_hPa" = YNPC_PosPress$PosPress_hPa)
remove(YNPC_Rainfall)
remove(YNPC_Temperature)
remove(YNPC_PosPress)

#畫降雨
ggplot(YNPC_data, aes(x = Month, y = Rainfall_mm, group = RegionName, color = RegionName)) + geom_point() + geom_line() + labs(title = "4 Regions Rainfall in Months (10yearsAvg)")
#畫氣溫
ggplot(YNPC_data, aes(x = Month, y = Temperature_C, group = RegionName, color = RegionName)) + geom_point() + geom_line() + labs(title = "4 Regions Temprature in Months (10yearsAvg)")
#畫氣壓
ggplot(YNPC_data, aes(x = Month, y = PosPress_hPa, group = RegionName, color = RegionName)) + geom_point() + geom_line() + labs(title = "4 Regions Press in Months (10yearsAvg)")





