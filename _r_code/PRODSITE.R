###PRODUCTION WEBSITE
{
##WORLD CHARTS
{
    library (readr)
    library(taRifx)
    library(ggplot2)
    library(dplyr)
    library(plotly)
    library(htmlwidgets)
    library(rmarkdown)
    options(scipen=999)
    
    
    {urlfile="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_confirmed_global.csv"
      world<-read_csv(url(urlfile))
      world$Lat<-NULL
      world$Long<-NULL
      world$'Province/State'<-NULL
      world1<-t(world)
      colnames(world1) = world1[1, ]
      world1 <- as.data.frame(world1)
      world1<-remove.factors(world1)
      world1 = world1[-1,]
      Dates<- cbind(date = rownames(world1), world1)
      Dates<-subset(Dates,select = c("date"))
      Dates$ID <- seq.int(nrow(Dates))
      US<-subset(world1,select = c("US"))
      US <- as.data.frame(apply(US, 2, as.numeric))
      US$ID <- seq.int(nrow(US))
      US <- merge(US,Dates,by="ID")
      US$pct_changePOS <- (US$US-lag(US$US))/lag(US$US)
      US$DailyNewPOS <- (US$US-lag(US$US))
      US2=tail(US, n=35)
      US2$date <- as.Date(US2$date, "%m/%d/%y")
      US2$Slope5 <- (lag(US2$US,n=4)-US2$US)/(lag(US2$ID,n=4)-US2$ID)
      US3<-subset(US,`US`>500)
      US3$Days <- seq(1:nrow(US3))
      US3$date <- as.Date(US3$date, "%m/%d/%y")
      US3$Slope5 <- (lag(US3$US,n=4)-US3$US)/(lag(US3$ID,n=4)-US3$ID)
      US4<-subset(US3,`Days`>299)}
    
    {  
      DailyNewPOS1 <- plot_ly(US3, x = ~date, y = ~DailyNewPOS, type = 'bar')
      DailyNewPOS1 <- DailyNewPOS1 %>% layout(
        title = "US Daily Covid Cases",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(count = 3,label = "3 mo",step = "month",stepmode = "backward"),
              list(count = 6,label = "6 mo",step = "month",stepmode = "backward"),
              list(count = 1,label = "1 yr",step = "year",stepmode = "backward"),
              list(count = 1,label = "YTD",step = "year",stepmode = "todate"),
              list(step = "all"))),
          rangeslider = list(visible=FALSE)),
        yaxis = list(title = "Cases"))
      
      margin <- list(autoexpand = TRUE,l = 10,r = 10,t = 100)
      
      DailyNewPOS1<-DailyNewPOS1 %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE)%>%
        layout(xaxis = list(title=""))%>%
        layout(yaxis = list(title=""))%>%
        layout(xaxis=list(fixedrange=TRUE)) %>% 
        layout(yaxis=list(fixedrange=TRUE)) %>%
        layout(title = "US Daily Covid Cases", margin = margin)
      
      #USDailyNewPOS1
      withr::with_dir('~/Sites/SITE1', saveWidget(DailyNewPOS1, file="DailyNewPOS1.html",selfcontained = FALSE)) 
    }  
    
    # WORLD ACTUALS - DEATHS
    
    {urlfile2="https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/csse_covid_19_data/csse_covid_19_time_series/time_series_covid19_deaths_global.csv"
      world_deaths<-read_csv(url(urlfile2))
      world_deaths$Lat<-NULL
      world_deaths$Long<-NULL
      world_deaths$'Province/State'<-NULL
      world_deaths1<-t(world_deaths)
      colnames(world_deaths1) = world_deaths1[1, ]
      world_deaths1 <- as.data.frame(world_deaths1)
      world_deaths1<-remove.factors(world_deaths1)
      world_deaths1 = world_deaths1[-1,]
      world_deaths1Dates<- cbind(date = rownames(world_deaths1), world_deaths1)
      world_deaths1Dates<-subset(world_deaths1Dates,select = c("date"))
      world_deaths1Dates$ID <- seq.int(nrow(world_deaths1Dates))
      USdeaths<-subset(world_deaths1,select = c("US"))
      USdeaths <- as.data.frame(apply(USdeaths, 2, as.numeric))
      USdeaths$ID <- seq.int(nrow(USdeaths))
      USdeaths <- merge(USdeaths,world_deaths1Dates,by="ID")
      USdeaths$pct_changePOS <- (USdeaths$US-lag(USdeaths$US))/lag(USdeaths$US)
      USdeaths$DailyNewPOS <- (USdeaths$US-lag(USdeaths$US))
      US2deaths<-subset(USdeaths,`US`>10)
      US2deaths$date <- as.Date(US2deaths$date, "%m/%d/%y")
      US2deaths$Days <- seq(1:nrow(US2deaths))
      USdeaths$date <- as.Date(USdeaths$date, "%m/%d/%y")
      USdeaths$Slope5 <- (lag(USdeaths$US,n=4)-USdeaths$US)/(lag(USdeaths$ID,n=4)-USdeaths$ID)
      US2deaths$Days <- as.numeric(US2deaths$Days)
      US2deaths$Avg<-(US2deaths$US+(lag(US2deaths$US,n=1)*2)+(lag(US2deaths$US,n=2)*3)+(lag(US2deaths$US,n=3)*4)+(lag(US2deaths$US,n=4)*5)+(lag(US2deaths$US,n=5)*6)+(lag(US2deaths$US,n=6)*7))/28
      US3deaths<-subset(US2deaths,`US`>22)
      US3deaths$Days <- seq(1:nrow(US3deaths))
      US4deaths<-subset(US3deaths,`Days`>303)}
    
    {
      DailyNewDEATH1 <- plot_ly(US2deaths, x = ~date, y = ~DailyNewPOS, type = 'bar')
      DailyNewDEATH1 <- DailyNewDEATH1 %>% layout(
        title = "US New Covid Deaths",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(count = 3,label = "3 mo",step = "month",stepmode = "backward"),
              list(count = 6,label = "6 mo",step = "month",stepmode = "backward"),
              list(count = 1,label = "1 yr",step = "year",stepmode = "backward"),
              list(count = 1,label = "YTD",step = "year",stepmode = "todate"),
              list(step = "all"))),
          rangeslider = list(visible=FALSE)),
        yaxis = list(title = "Cases"))
      
      margin <- list(autoexpand = TRUE,l = 10,r = 10,t = 100)
      
      DailyNewDEATH1<-DailyNewDEATH1 %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE)%>%
        layout(xaxis = list(title=""))%>%
        layout(yaxis = list(title=""))%>%
        layout(xaxis=list(fixedrange=TRUE)) %>% 
        layout(yaxis=list(fixedrange=TRUE)) %>%
        layout(title = "US Daily Covid Deaths", margin = margin)
      
      #USDailyCovidDeaths
      withr::with_dir('~/Sites/SITE1', saveWidget(DailyNewDEATH1, file="DailyNewDEATH1.html",selfcontained = FALSE)) 
    }
  }

##MASS CHARTS
{
    library (readr)
    library(taRifx)
    library(ggplot2)
    library(dplyr)
    library(plotly)
    library(htmlwidgets)
    options(scipen=999)
    
    Population <- read_csv("Sites/SITE1/_files/Population.csv")
    dave1="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
    dave1<-read_csv(url(dave1))
    dave1<-inner_join(Population, dave1, by = c("State1" = "state"))
    MA<-subset(dave1, State=="MA")
    MA$DailyNewPOS <- (MA$cases-lag(MA$cases))
    MA$DailyNewDeath <- (MA$deaths-lag(MA$deaths))
    MA5<-subset(MA,DailyNewPOS>0)
    MA10<-subset(MA5,date>'2020-12-31')
    MA7<-subset(MA,DailyNewDeath>-1)
    MA11<-subset(MA7,date>'2020-12-31')
    MA5$MA7CASES<-(MA5$DailyNewPOS+(lag(MA5$DailyNewPOS,n=1))+(lag(MA5$DailyNewPOS,n=2))+(lag(MA5$DailyNewPOS,n=3))+(lag(MA5$DailyNewPOS,n=4))+(lag(MA5$DailyNewPOS,n=5))+(lag(MA5$DailyNewPOS,n=6)))/7
    MA7$MA7DEATHS<-(MA7$DailyNewDeath+(lag(MA7$DailyNewDeath,n=1))+(lag(MA7$DailyNewDeath,n=2))+(lag(MA7$DailyNewDeath,n=3))+(lag(MA7$DailyNewDeath,n=4))+(lag(MA7$DailyNewDeath,n=5))+(lag(MA7$DailyNewDeath,n=6)))/7
    
    {
      MADailyNewPOS1 <- plot_ly(MA5, x = ~date, y = ~DailyNewPOS, type = 'bar',name = 'Total Cases')
      MADailyNewPOS1 <- MADailyNewPOS1 %>% layout(
        title = "Mass Daily New Covid Cases",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(count = 3,label = "3 mo",step = "month",stepmode = "backward"),
              list(count = 6,label = "6 mo",step = "month",stepmode = "backward"),
              list(count = 1,label = "1 yr",step = "year",stepmode = "backward"),
              list(count = 1,label = "YTD",step = "year",stepmode = "todate"),
              list(step = "all"))),
          rangeslider = list(visible=FALSE)),
        yaxis = list(title = "Cases"))
      
      margin <- list(autoexpand = TRUE,l = 10,r = 10,t = 100)
      
      MADailyNewPOS1<-MADailyNewPOS1 %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE)%>%
        layout(xaxis = list(title=""))%>%
        layout(yaxis = list(title=""))%>%
        layout(xaxis=list(fixedrange=TRUE)) %>% 
        layout(yaxis=list(fixedrange=TRUE)) %>%
        layout(title = "MA Daily Covid Cases", margin = margin)
     
      MADailyNewPOS1 <- MADailyNewPOS1 %>% add_trace(MA5, x = ~date, y = ~MA7CASES, type = 'scatter', mode = 'lines',name = '7 Day Avg')
      MADailyNewPOS1 <- MADailyNewPOS1 %>% layout(showlegend = FALSE)
      
      #MADailyNewPOS1
      withr::with_dir('~/Sites/SITE1', saveWidget(MADailyNewPOS1, file="MADailyNewPOS1.html",selfcontained = FALSE)) 
      
      
      
      MADailyNewDEATH1 <- plot_ly(MA7, x = ~date, y = ~DailyNewDeath, type = 'bar')
      MADailyNewDEATH1 <- MADailyNewDEATH1 %>% layout(
        title = "Mass Daily New Covid Deaths",
        xaxis = list(
          rangeselector = list(
            buttons = list(
              list(count = 3,label = "3 mo",step = "month",stepmode = "backward"),
              list(count = 6,label = "6 mo",step = "month",stepmode = "backward"),
              list(count = 1,label = "1 yr",step = "year",stepmode = "backward"),
              list(count = 1,label = "YTD",step = "year",stepmode = "todate"),
              list(step = "all"))),
          rangeslider = list(visible=FALSE)),
        yaxis = list(title = "Cases"))
      
      margin <- list(autoexpand = TRUE,l = 10,r = 10,t = 100)
      
      MADailyNewDEATH1<-MADailyNewDEATH1 %>%
        config(displayModeBar = FALSE) %>%
        config(showLink = FALSE)%>%
        layout(xaxis = list(title=""))%>%
        layout(yaxis = list(title=""))%>%
        layout(xaxis=list(fixedrange=TRUE)) %>% 
        layout(yaxis=list(fixedrange=TRUE)) %>%
        layout(title = "MA Daily Covid Deaths", margin = margin)
      
      MADailyNewDEATH1 <- MADailyNewDEATH1 %>% add_trace(MA5, x = ~date, y = ~MA7DEATHS, type = 'scatter', mode = 'lines',name = '7 Day Avg')
      MADailyNewDEATH1 <- MADailyNewDEATH1 %>% layout(showlegend = FALSE)
      
      #MADailyNewDEATH1
      withr::with_dir('~/Sites/SITE1', saveWidget(MADailyNewDEATH1, file="MADailyNewDEATH1.html",selfcontained = FALSE)) 
    }    
  }
  

##STATES MAP
{  
library(ggthemes)
library(ggpubr)
library(tmap)
library(sf)
library(tmaptools)
library(plotly)
library(ggplot2)
library(readr)
library(dplyr)
library(rgdal)
library(leaflet)
library(tigris)
library(leaflet.providers)
library(htmlwidgets)
library(tigris)
library(leaflet.extras)
options(scipen=999)
  
#State Growth Rate of Daily Cases
PollsCheck <- read_csv("Sites/SITE1/_files/PollsCheck.csv")
#Population <- read_csv("Documents/Population.csv")
#dave1="https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv"
#dave1<-read_csv(url(dave1))
#dave1<-inner_join(Population, dave1, by = c("State1" = "state"))
date1=Sys.Date()-1
date2=Sys.Date()-32
  
 
#Subset State Files
{AL<-subset(dave1,State=="AL")
    AR<-subset(dave1,State=="AR")
    AZ<-subset(dave1,State=="AZ")
    CA<-subset(dave1,State=="CA")
    CO<-subset(dave1,State=="CO")
    CT<-subset(dave1,State=="CT")
    DC<-subset(dave1,State=="DC")
    DE<-subset(dave1,State=="DE")
    FL<-subset(dave1,State=="FL")
    GA<-subset(dave1,State=="GA")
    HI<-subset(dave1,State=="HI")
    IA<-subset(dave1,State=="IA")
    ID<-subset(dave1,State=="ID")
    IL<-subset(dave1,State=="IL")
    IN<-subset(dave1,State=="IN")
    KS<-subset(dave1,State=="KS")
    KY<-subset(dave1,State=="KY")
    LA<-subset(dave1,State=="LA")
    MA<-subset(dave1,State=="MA")
    MD<-subset(dave1,State=="MD")
    ME<-subset(dave1,State=="ME")
    MI<-subset(dave1,State=="MI")
    MN<-subset(dave1,State=="MN")
    MO<-subset(dave1,State=="MO")
    MP<-subset(dave1,State=="MP")
    MS<-subset(dave1,State=="MS")
    MT<-subset(dave1,State=="MT")
    NC<-subset(dave1,State=="NC")
    ND<-subset(dave1,State=="ND")
    NE<-subset(dave1,State=="NE")
    NH<-subset(dave1,State=="NH")
    NJ<-subset(dave1,State=="NJ")
    NM<-subset(dave1,State=="NM")
    NV<-subset(dave1,State=="NV")
    NY<-subset(dave1,State=="NY")
    OH<-subset(dave1,State=="OH")
    OK<-subset(dave1,State=="OK")
    OR<-subset(dave1,State=="OR")
    PA<-subset(dave1,State=="PA")
    PR<-subset(dave1,State=="PR")
    RI<-subset(dave1,State=="RI")
    SC<-subset(dave1,State=="SC")
    SD<-subset(dave1,State=="SD")
    TN<-subset(dave1,State=="TN")
    TX<-subset(dave1,State=="TX")
    UT<-subset(dave1,State=="UT")
    VA<-subset(dave1,State=="VA")
    VI<-subset(dave1,State=="VI")
    VT<-subset(dave1,State=="VT")
    WA<-subset(dave1,State=="WA")
    WI<-subset(dave1,State=="WI")
    WV<-subset(dave1,State=="WV")
    WY<-subset(dave1,State=="WY")
    AK<-subset(dave1,State=="AK")}
    
#Subset Population
{ALp<-subset(Population,State=="AL")
      ARp<-subset(Population,State=="AR")
      AZp<-subset(Population,State=="AZ")
      CAp<-subset(Population,State=="CA")
      COp<-subset(Population,State=="CO")
      CTp<-subset(Population,State=="CT")
      DCp<-subset(Population,State=="DC")
      DEp<-subset(Population,State=="DE")
      FLp<-subset(Population,State=="FL")
      GAp<-subset(Population,State=="GA")
      HIp<-subset(Population,State=="HI")
      IAp<-subset(Population,State=="IA")
      IDp<-subset(Population,State=="ID")
      ILp<-subset(Population,State=="IL")
      INp<-subset(Population,State=="IN")
      KSp<-subset(Population,State=="KS")
      KYp<-subset(Population,State=="KY")
      LAp<-subset(Population,State=="LA")
      MAp<-subset(Population,State=="MA")
      MDp<-subset(Population,State=="MD")
      MEp<-subset(Population,State=="ME")
      MIp<-subset(Population,State=="MI")
      MNp<-subset(Population,State=="MN")
      MOp<-subset(Population,State=="MO")
      MPp<-subset(Population,State=="MP")
      MSp<-subset(Population,State=="MS")
      MTp<-subset(Population,State=="MT")
      NCp<-subset(Population,State=="NC")
      NDp<-subset(Population,State=="ND")
      NEp<-subset(Population,State=="NE")
      NHp<-subset(Population,State=="NH")
      NJp<-subset(Population,State=="NJ")
      NMp<-subset(Population,State=="NM")
      NVp<-subset(Population,State=="NV")
      NYp<-subset(Population,State=="NY")
      OHp<-subset(Population,State=="OH")
      OKp<-subset(Population,State=="OK")
      ORp<-subset(Population,State=="OR")
      PAp<-subset(Population,State=="PA")
      PRp<-subset(Population,State=="PR")
      RIp<-subset(Population,State=="RI")
      SCp<-subset(Population,State=="SC")
      SDp<-subset(Population,State=="SD")
      TNp<-subset(Population,State=="TN")
      TXp<-subset(Population,State=="TX")
      UTp<-subset(Population,State=="UT")
      VAp<-subset(Population,State=="VA")
      VIp<-subset(Population,State=="VI")
      VTp<-subset(Population,State=="VT")
      WAp<-subset(Population,State=="WA")
      WIp<-subset(Population,State=="WI")
      WVp<-subset(Population,State=="WV")
      WYp<-subset(Population,State=="WY")
      AKp<-subset(Population,State=="AK")}
    
#Calc Daily Cases
{AL$DailyNewPOS <- (AL$cases-lag(AL$cases))
      AR$DailyNewPOS <- (AR$cases-lag(AR$cases))
      AZ$DailyNewPOS <- (AZ$cases-lag(AZ$cases))
      CA$DailyNewPOS <- (CA$cases-lag(CA$cases))
      CO$DailyNewPOS <- (CO$cases-lag(CO$cases))
      CT$DailyNewPOS <- (CT$cases-lag(CT$cases))
      DC$DailyNewPOS <- (DC$cases-lag(DC$cases))
      DE$DailyNewPOS <- (DE$cases-lag(DE$cases))
      FL$DailyNewPOS <- (FL$cases-lag(FL$cases))
      GA$DailyNewPOS <- (GA$cases-lag(GA$cases))
      HI$DailyNewPOS <- (HI$cases-lag(HI$cases))
      IA$DailyNewPOS <- (IA$cases-lag(IA$cases))
      ID$DailyNewPOS <- (ID$cases-lag(ID$cases))
      IL$DailyNewPOS <- (IL$cases-lag(IL$cases))
      IN$DailyNewPOS <- (IN$cases-lag(IN$cases))
      KS$DailyNewPOS <- (KS$cases-lag(KS$cases))
      KY$DailyNewPOS <- (KY$cases-lag(KY$cases))
      LA$DailyNewPOS <- (LA$cases-lag(LA$cases))
      MA$DailyNewPOS <- (MA$cases-lag(MA$cases))
      MD$DailyNewPOS <- (MD$cases-lag(MD$cases))
      ME$DailyNewPOS <- (ME$cases-lag(ME$cases))
      MI$DailyNewPOS <- (MI$cases-lag(MI$cases))
      MN$DailyNewPOS <- (MN$cases-lag(MN$cases))
      MO$DailyNewPOS <- ifelse(MO$cases-lag(MO$cases)>4*(lag(MO$cases,n=1)-lag(MO$cases,n=2)),(lag(MO$cases,n=1)-lag(MO$cases,n=2)),MO$cases-lag(MO$cases))
      MP$DailyNewPOS <- (MP$cases-lag(MP$cases))
      MS$DailyNewPOS <- (MS$cases-lag(MS$cases))
      MT$DailyNewPOS <- (MT$cases-lag(MT$cases))
      NC$DailyNewPOS <- (NC$cases-lag(NC$cases))
      ND$DailyNewPOS <- (ND$cases-lag(ND$cases))
      NE$DailyNewPOS <- (NE$cases-lag(NE$cases))
      NH$DailyNewPOS <- (NH$cases-lag(NH$cases))
      NJ$DailyNewPOS <- (NJ$cases-lag(NJ$cases))
      NM$DailyNewPOS <- (NM$cases-lag(NM$cases))
      NV$DailyNewPOS <- (NV$cases-lag(NV$cases))
      NY$DailyNewPOS <- (NY$cases-lag(NY$cases))
      OH$DailyNewPOS <- (OH$cases-lag(OH$cases))
      OK$DailyNewPOS <- (OK$cases-lag(OK$cases))
      OR$DailyNewPOS <- (OR$cases-lag(OR$cases))
      PA$DailyNewPOS <- (PA$cases-lag(PA$cases))
      PR$DailyNewPOS <- (PR$cases-lag(PR$cases))
      RI$DailyNewPOS <- (RI$cases-lag(RI$cases))
      SC$DailyNewPOS <- (SC$cases-lag(SC$cases))
      SD$DailyNewPOS <- (SD$cases-lag(SD$cases))
      TN$DailyNewPOS <- (TN$cases-lag(TN$cases))
      TX$DailyNewPOS <- (TX$cases-lag(TX$cases))
      UT$DailyNewPOS <- (UT$cases-lag(UT$cases))
      VA$DailyNewPOS <- (VA$cases-lag(VA$cases))
      VI$DailyNewPOS <- (VI$cases-lag(VI$cases))
      VT$DailyNewPOS <- (VT$cases-lag(VT$cases))
      WA$DailyNewPOS <- (WA$cases-lag(WA$cases))
      WI$DailyNewPOS <- (WI$cases-lag(WI$cases))
      WV$DailyNewPOS <- (WV$cases-lag(WV$cases))
      WY$DailyNewPOS <- (WY$cases-lag(WY$cases))
      AK$DailyNewPOS <- (AK$cases-lag(AK$cases))}
    
#Calc 7 Day Average Daily Cases
{WY$DailyAvg<-(lag(WY$DailyNewPOS,n=0)+lag(WY$DailyNewPOS,n=1)+lag(WY$DailyNewPOS,n=2)+lag(WY$DailyNewPOS,n=3)+lag(WY$DailyNewPOS,n=4)+lag(WY$DailyNewPOS,n=5)+lag(WY$DailyNewPOS,n=6))/7
      AK$DailyAvg<-(lag(AK$DailyNewPOS,n=0)+lag(AK$DailyNewPOS,n=1)+lag(AK$DailyNewPOS,n=2)+lag(AK$DailyNewPOS,n=3)+lag(AK$DailyNewPOS,n=4)+lag(AK$DailyNewPOS,n=5)+lag(AK$DailyNewPOS,n=6))/7
      AL$DailyAvg<-(lag(AL$DailyNewPOS,n=0)+lag(AL$DailyNewPOS,n=1)+lag(AL$DailyNewPOS,n=2)+lag(AL$DailyNewPOS,n=3)+lag(AL$DailyNewPOS,n=4)+lag(AL$DailyNewPOS,n=5)+lag(AL$DailyNewPOS,n=6))/7
      AR$DailyAvg<-(lag(AR$DailyNewPOS,n=0)+lag(AR$DailyNewPOS,n=1)+lag(AR$DailyNewPOS,n=2)+lag(AR$DailyNewPOS,n=3)+lag(AR$DailyNewPOS,n=4)+lag(AR$DailyNewPOS,n=5)+lag(AR$DailyNewPOS,n=6))/7
      AZ$DailyAvg<-(lag(AZ$DailyNewPOS,n=0)+lag(AZ$DailyNewPOS,n=1)+lag(AZ$DailyNewPOS,n=2)+lag(AZ$DailyNewPOS,n=3)+lag(AZ$DailyNewPOS,n=4)+lag(AZ$DailyNewPOS,n=5)+lag(AZ$DailyNewPOS,n=6))/7
      CA$DailyAvg<-(lag(CA$DailyNewPOS,n=0)+lag(CA$DailyNewPOS,n=1)+lag(CA$DailyNewPOS,n=2)+lag(CA$DailyNewPOS,n=3)+lag(CA$DailyNewPOS,n=4)+lag(CA$DailyNewPOS,n=5)+lag(CA$DailyNewPOS,n=6))/7
      CO$DailyAvg<-(lag(CO$DailyNewPOS,n=0)+lag(CO$DailyNewPOS,n=1)+lag(CO$DailyNewPOS,n=2)+lag(CO$DailyNewPOS,n=3)+lag(CO$DailyNewPOS,n=4)+lag(CO$DailyNewPOS,n=5)+lag(CO$DailyNewPOS,n=6))/7
      CT$DailyAvg<-(lag(CT$DailyNewPOS,n=0)+lag(CT$DailyNewPOS,n=1)+lag(CT$DailyNewPOS,n=2)+lag(CT$DailyNewPOS,n=3)+lag(CT$DailyNewPOS,n=4)+lag(CT$DailyNewPOS,n=5)+lag(CT$DailyNewPOS,n=6))/7
      DC$DailyAvg<-(lag(DC$DailyNewPOS,n=0)+lag(DC$DailyNewPOS,n=1)+lag(DC$DailyNewPOS,n=2)+lag(DC$DailyNewPOS,n=3)+lag(DC$DailyNewPOS,n=4)+lag(DC$DailyNewPOS,n=5)+lag(DC$DailyNewPOS,n=6))/7
      DE$DailyAvg<-(lag(DE$DailyNewPOS,n=0)+lag(DE$DailyNewPOS,n=1)+lag(DE$DailyNewPOS,n=2)+lag(DE$DailyNewPOS,n=3)+lag(DE$DailyNewPOS,n=4)+lag(DE$DailyNewPOS,n=5)+lag(DE$DailyNewPOS,n=6))/7
      FL$DailyAvg<-(lag(FL$DailyNewPOS,n=0)+lag(FL$DailyNewPOS,n=1)+lag(FL$DailyNewPOS,n=2)+lag(FL$DailyNewPOS,n=3)+lag(FL$DailyNewPOS,n=4)+lag(FL$DailyNewPOS,n=5)+lag(FL$DailyNewPOS,n=6))/7
      GA$DailyAvg<-(lag(GA$DailyNewPOS,n=0)+lag(GA$DailyNewPOS,n=1)+lag(GA$DailyNewPOS,n=2)+lag(GA$DailyNewPOS,n=3)+lag(GA$DailyNewPOS,n=4)+lag(GA$DailyNewPOS,n=5)+lag(GA$DailyNewPOS,n=6))/7
      HI$DailyAvg<-(lag(HI$DailyNewPOS,n=0)+lag(HI$DailyNewPOS,n=1)+lag(HI$DailyNewPOS,n=2)+lag(HI$DailyNewPOS,n=3)+lag(HI$DailyNewPOS,n=4)+lag(HI$DailyNewPOS,n=5)+lag(HI$DailyNewPOS,n=6))/7
      IA$DailyAvg<-(lag(IA$DailyNewPOS,n=0)+lag(IA$DailyNewPOS,n=1)+lag(IA$DailyNewPOS,n=2)+lag(IA$DailyNewPOS,n=3)+lag(IA$DailyNewPOS,n=4)+lag(IA$DailyNewPOS,n=5)+lag(IA$DailyNewPOS,n=6))/7
      ID$DailyAvg<-(lag(ID$DailyNewPOS,n=0)+lag(ID$DailyNewPOS,n=1)+lag(ID$DailyNewPOS,n=2)+lag(ID$DailyNewPOS,n=3)+lag(ID$DailyNewPOS,n=4)+lag(ID$DailyNewPOS,n=5)+lag(ID$DailyNewPOS,n=6))/7
      IL$DailyAvg<-(lag(IL$DailyNewPOS,n=0)+lag(IL$DailyNewPOS,n=1)+lag(IL$DailyNewPOS,n=2)+lag(IL$DailyNewPOS,n=3)+lag(IL$DailyNewPOS,n=4)+lag(IL$DailyNewPOS,n=5)+lag(IL$DailyNewPOS,n=6))/7
      IN$DailyAvg<-(lag(IN$DailyNewPOS,n=0)+lag(IN$DailyNewPOS,n=1)+lag(IN$DailyNewPOS,n=2)+lag(IN$DailyNewPOS,n=3)+lag(IN$DailyNewPOS,n=4)+lag(IN$DailyNewPOS,n=5)+lag(IN$DailyNewPOS,n=6))/7
      KS$DailyAvg<-(lag(KS$DailyNewPOS,n=0)+lag(KS$DailyNewPOS,n=1)+lag(KS$DailyNewPOS,n=2)+lag(KS$DailyNewPOS,n=3)+lag(KS$DailyNewPOS,n=4)+lag(KS$DailyNewPOS,n=5)+lag(KS$DailyNewPOS,n=6))/7
      KY$DailyAvg<-(lag(KY$DailyNewPOS,n=0)+lag(KY$DailyNewPOS,n=1)+lag(KY$DailyNewPOS,n=2)+lag(KY$DailyNewPOS,n=3)+lag(KY$DailyNewPOS,n=4)+lag(KY$DailyNewPOS,n=5)+lag(KY$DailyNewPOS,n=6))/7
      LA$DailyAvg<-(lag(LA$DailyNewPOS,n=0)+lag(LA$DailyNewPOS,n=1)+lag(LA$DailyNewPOS,n=2)+lag(LA$DailyNewPOS,n=3)+lag(LA$DailyNewPOS,n=4)+lag(LA$DailyNewPOS,n=5)+lag(LA$DailyNewPOS,n=6))/7
      MA$DailyAvg<-(lag(MA$DailyNewPOS,n=0)+lag(MA$DailyNewPOS,n=1)+lag(MA$DailyNewPOS,n=2)+lag(MA$DailyNewPOS,n=3)+lag(MA$DailyNewPOS,n=4)+lag(MA$DailyNewPOS,n=5)+lag(MA$DailyNewPOS,n=6))/7
      MD$DailyAvg<-(lag(MD$DailyNewPOS,n=0)+lag(MD$DailyNewPOS,n=1)+lag(MD$DailyNewPOS,n=2)+lag(MD$DailyNewPOS,n=3)+lag(MD$DailyNewPOS,n=4)+lag(MD$DailyNewPOS,n=5)+lag(MD$DailyNewPOS,n=6))/7
      ME$DailyAvg<-(lag(ME$DailyNewPOS,n=0)+lag(ME$DailyNewPOS,n=1)+lag(ME$DailyNewPOS,n=2)+lag(ME$DailyNewPOS,n=3)+lag(ME$DailyNewPOS,n=4)+lag(ME$DailyNewPOS,n=5)+lag(ME$DailyNewPOS,n=6))/7
      MI$DailyAvg<-(lag(MI$DailyNewPOS,n=0)+lag(MI$DailyNewPOS,n=1)+lag(MI$DailyNewPOS,n=2)+lag(MI$DailyNewPOS,n=3)+lag(MI$DailyNewPOS,n=4)+lag(MI$DailyNewPOS,n=5)+lag(MI$DailyNewPOS,n=6))/7
      MN$DailyAvg<-(lag(MN$DailyNewPOS,n=0)+lag(MN$DailyNewPOS,n=1)+lag(MN$DailyNewPOS,n=2)+lag(MN$DailyNewPOS,n=3)+lag(MN$DailyNewPOS,n=4)+lag(MN$DailyNewPOS,n=5)+lag(MN$DailyNewPOS,n=6))/7
      MO$DailyAvg<-(lag(MO$DailyNewPOS,n=0)+lag(MO$DailyNewPOS,n=1)+lag(MO$DailyNewPOS,n=2)+lag(MO$DailyNewPOS,n=3)+lag(MO$DailyNewPOS,n=4)+lag(MO$DailyNewPOS,n=5)+lag(MO$DailyNewPOS,n=6))/7
      MP$DailyAvg<-(lag(MP$DailyNewPOS,n=0)+lag(MP$DailyNewPOS,n=1)+lag(MP$DailyNewPOS,n=2)+lag(MP$DailyNewPOS,n=3)+lag(MP$DailyNewPOS,n=4)+lag(MP$DailyNewPOS,n=5)+lag(MP$DailyNewPOS,n=6))/7
      MS$DailyAvg<-(lag(MS$DailyNewPOS,n=0)+lag(MS$DailyNewPOS,n=1)+lag(MS$DailyNewPOS,n=2)+lag(MS$DailyNewPOS,n=3)+lag(MS$DailyNewPOS,n=4)+lag(MS$DailyNewPOS,n=5)+lag(MS$DailyNewPOS,n=6))/7
      MT$DailyAvg<-(lag(MT$DailyNewPOS,n=0)+lag(MT$DailyNewPOS,n=1)+lag(MT$DailyNewPOS,n=2)+lag(MT$DailyNewPOS,n=3)+lag(MT$DailyNewPOS,n=4)+lag(MT$DailyNewPOS,n=5)+lag(MT$DailyNewPOS,n=6))/7
      NC$DailyAvg<-(lag(NC$DailyNewPOS,n=0)+lag(NC$DailyNewPOS,n=1)+lag(NC$DailyNewPOS,n=2)+lag(NC$DailyNewPOS,n=3)+lag(NC$DailyNewPOS,n=4)+lag(NC$DailyNewPOS,n=5)+lag(NC$DailyNewPOS,n=6))/7
      ND$DailyAvg<-(lag(ND$DailyNewPOS,n=0)+lag(ND$DailyNewPOS,n=1)+lag(ND$DailyNewPOS,n=2)+lag(ND$DailyNewPOS,n=3)+lag(ND$DailyNewPOS,n=4)+lag(ND$DailyNewPOS,n=5)+lag(ND$DailyNewPOS,n=6))/7
      NE$DailyAvg<-(lag(NE$DailyNewPOS,n=0)+lag(NE$DailyNewPOS,n=1)+lag(NE$DailyNewPOS,n=2)+lag(NE$DailyNewPOS,n=3)+lag(NE$DailyNewPOS,n=4)+lag(NE$DailyNewPOS,n=5)+lag(NE$DailyNewPOS,n=6))/7
      NH$DailyAvg<-(lag(NH$DailyNewPOS,n=0)+lag(NH$DailyNewPOS,n=1)+lag(NH$DailyNewPOS,n=2)+lag(NH$DailyNewPOS,n=3)+lag(NH$DailyNewPOS,n=4)+lag(NH$DailyNewPOS,n=5)+lag(NH$DailyNewPOS,n=6))/7
      NJ$DailyAvg<-(lag(NJ$DailyNewPOS,n=0)+lag(NJ$DailyNewPOS,n=1)+lag(NJ$DailyNewPOS,n=2)+lag(NJ$DailyNewPOS,n=3)+lag(NJ$DailyNewPOS,n=4)+lag(NJ$DailyNewPOS,n=5)+lag(NJ$DailyNewPOS,n=6))/7
      NM$DailyAvg<-(lag(NM$DailyNewPOS,n=0)+lag(NM$DailyNewPOS,n=1)+lag(NM$DailyNewPOS,n=2)+lag(NM$DailyNewPOS,n=3)+lag(NM$DailyNewPOS,n=4)+lag(NM$DailyNewPOS,n=5)+lag(NM$DailyNewPOS,n=6))/7
      NV$DailyAvg<-(lag(NV$DailyNewPOS,n=0)+lag(NV$DailyNewPOS,n=1)+lag(NV$DailyNewPOS,n=2)+lag(NV$DailyNewPOS,n=3)+lag(NV$DailyNewPOS,n=4)+lag(NV$DailyNewPOS,n=5)+lag(NV$DailyNewPOS,n=6))/7
      NY$DailyAvg<-(lag(NY$DailyNewPOS,n=0)+lag(NY$DailyNewPOS,n=1)+lag(NY$DailyNewPOS,n=2)+lag(NY$DailyNewPOS,n=3)+lag(NY$DailyNewPOS,n=4)+lag(NY$DailyNewPOS,n=5)+lag(NY$DailyNewPOS,n=6))/7
      OH$DailyAvg<-(lag(OH$DailyNewPOS,n=0)+lag(OH$DailyNewPOS,n=1)+lag(OH$DailyNewPOS,n=2)+lag(OH$DailyNewPOS,n=3)+lag(OH$DailyNewPOS,n=4)+lag(OH$DailyNewPOS,n=5)+lag(OH$DailyNewPOS,n=6))/7
      OK$DailyAvg<-(lag(OK$DailyNewPOS,n=0)+lag(OK$DailyNewPOS,n=1)+lag(OK$DailyNewPOS,n=2)+lag(OK$DailyNewPOS,n=3)+lag(OK$DailyNewPOS,n=4)+lag(OK$DailyNewPOS,n=5)+lag(OK$DailyNewPOS,n=6))/7
      OR$DailyAvg<-(lag(OR$DailyNewPOS,n=0)+lag(OR$DailyNewPOS,n=1)+lag(OR$DailyNewPOS,n=2)+lag(OR$DailyNewPOS,n=3)+lag(OR$DailyNewPOS,n=4)+lag(OR$DailyNewPOS,n=5)+lag(OR$DailyNewPOS,n=6))/7
      PA$DailyAvg<-(lag(PA$DailyNewPOS,n=0)+lag(PA$DailyNewPOS,n=1)+lag(PA$DailyNewPOS,n=2)+lag(PA$DailyNewPOS,n=3)+lag(PA$DailyNewPOS,n=4)+lag(PA$DailyNewPOS,n=5)+lag(PA$DailyNewPOS,n=6))/7
      PR$DailyAvg<-(lag(PR$DailyNewPOS,n=0)+lag(PR$DailyNewPOS,n=1)+lag(PR$DailyNewPOS,n=2)+lag(PR$DailyNewPOS,n=3)+lag(PR$DailyNewPOS,n=4)+lag(PR$DailyNewPOS,n=5)+lag(PR$DailyNewPOS,n=6))/7
      RI$DailyAvg<-(lag(RI$DailyNewPOS,n=0)+lag(RI$DailyNewPOS,n=1)+lag(RI$DailyNewPOS,n=2)+lag(RI$DailyNewPOS,n=3)+lag(RI$DailyNewPOS,n=4)+lag(RI$DailyNewPOS,n=5)+lag(RI$DailyNewPOS,n=6))/7
      SC$DailyAvg<-(lag(SC$DailyNewPOS,n=0)+lag(SC$DailyNewPOS,n=1)+lag(SC$DailyNewPOS,n=2)+lag(SC$DailyNewPOS,n=3)+lag(SC$DailyNewPOS,n=4)+lag(SC$DailyNewPOS,n=5)+lag(SC$DailyNewPOS,n=6))/7
      SD$DailyAvg<-(lag(SD$DailyNewPOS,n=0)+lag(SD$DailyNewPOS,n=1)+lag(SD$DailyNewPOS,n=2)+lag(SD$DailyNewPOS,n=3)+lag(SD$DailyNewPOS,n=4)+lag(SD$DailyNewPOS,n=5)+lag(SD$DailyNewPOS,n=6))/7
      TN$DailyAvg<-(lag(TN$DailyNewPOS,n=0)+lag(TN$DailyNewPOS,n=1)+lag(TN$DailyNewPOS,n=2)+lag(TN$DailyNewPOS,n=3)+lag(TN$DailyNewPOS,n=4)+lag(TN$DailyNewPOS,n=5)+lag(TN$DailyNewPOS,n=6))/7
      TX$DailyAvg<-(lag(TX$DailyNewPOS,n=0)+lag(TX$DailyNewPOS,n=1)+lag(TX$DailyNewPOS,n=2)+lag(TX$DailyNewPOS,n=3)+lag(TX$DailyNewPOS,n=4)+lag(TX$DailyNewPOS,n=5)+lag(TX$DailyNewPOS,n=6))/7
      UT$DailyAvg<-(lag(UT$DailyNewPOS,n=0)+lag(UT$DailyNewPOS,n=1)+lag(UT$DailyNewPOS,n=2)+lag(UT$DailyNewPOS,n=3)+lag(UT$DailyNewPOS,n=4)+lag(UT$DailyNewPOS,n=5)+lag(UT$DailyNewPOS,n=6))/7
      VA$DailyAvg<-(lag(VA$DailyNewPOS,n=0)+lag(VA$DailyNewPOS,n=1)+lag(VA$DailyNewPOS,n=2)+lag(VA$DailyNewPOS,n=3)+lag(VA$DailyNewPOS,n=4)+lag(VA$DailyNewPOS,n=5)+lag(VA$DailyNewPOS,n=6))/7
      VI$DailyAvg<-(lag(VI$DailyNewPOS,n=0)+lag(VI$DailyNewPOS,n=1)+lag(VI$DailyNewPOS,n=2)+lag(VI$DailyNewPOS,n=3)+lag(VI$DailyNewPOS,n=4)+lag(VI$DailyNewPOS,n=5)+lag(VI$DailyNewPOS,n=6))/7
      VT$DailyAvg<-(lag(VT$DailyNewPOS,n=0)+lag(VT$DailyNewPOS,n=1)+lag(VT$DailyNewPOS,n=2)+lag(VT$DailyNewPOS,n=3)+lag(VT$DailyNewPOS,n=4)+lag(VT$DailyNewPOS,n=5)+lag(VT$DailyNewPOS,n=6))/7
      WA$DailyAvg<-(lag(WA$DailyNewPOS,n=0)+lag(WA$DailyNewPOS,n=1)+lag(WA$DailyNewPOS,n=2)+lag(WA$DailyNewPOS,n=3)+lag(WA$DailyNewPOS,n=4)+lag(WA$DailyNewPOS,n=5)+lag(WA$DailyNewPOS,n=6))/7
      WI$DailyAvg<-(lag(WI$DailyNewPOS,n=0)+lag(WI$DailyNewPOS,n=1)+lag(WI$DailyNewPOS,n=2)+lag(WI$DailyNewPOS,n=3)+lag(WI$DailyNewPOS,n=4)+lag(WI$DailyNewPOS,n=5)+lag(WI$DailyNewPOS,n=6))/7
      WV$DailyAvg<-(lag(WV$DailyNewPOS,n=0)+lag(WV$DailyNewPOS,n=1)+lag(WV$DailyNewPOS,n=2)+lag(WV$DailyNewPOS,n=3)+lag(WV$DailyNewPOS,n=4)+lag(WV$DailyNewPOS,n=5)+lag(WV$DailyNewPOS,n=6))/7
      WY$DailyAvg<-(lag(WY$DailyNewPOS,n=0)+lag(WY$DailyNewPOS,n=1)+lag(WY$DailyNewPOS,n=2)+lag(WY$DailyNewPOS,n=3)+lag(WY$DailyNewPOS,n=4)+lag(WY$DailyNewPOS,n=5)+lag(WY$DailyNewPOS,n=6))/7}
    
#Calc Avg Cases Per100K
{AL$CasesPer100K <- (AL$DailyAvg)/(ALp$Population2/100000)
      AR$CasesPer100K <- (AR$DailyAvg)/(ARp$Population2/100000)
      AZ$CasesPer100K <- (AZ$DailyAvg)/(AZp$Population2/100000)
      CA$CasesPer100K <- (CA$DailyAvg)/(CAp$Population2/100000)
      CO$CasesPer100K <- (CO$DailyAvg)/(COp$Population2/100000)
      CT$CasesPer100K <- (CT$DailyAvg)/(CTp$Population2/100000)
      DC$CasesPer100K <- (DC$DailyAvg)/(DCp$Population2/100000)
      DE$CasesPer100K <- (DE$DailyAvg)/(DEp$Population2/100000)
      FL$CasesPer100K <- (FL$DailyAvg)/(FLp$Population2/100000)
      GA$CasesPer100K <- (GA$DailyAvg)/(GAp$Population2/100000)
      HI$CasesPer100K <- (HI$DailyAvg)/(HIp$Population2/100000)
      IA$CasesPer100K <- (IA$DailyAvg)/(IAp$Population2/100000)
      ID$CasesPer100K <- (ID$DailyAvg)/(IDp$Population2/100000)
      IL$CasesPer100K <- (IL$DailyAvg)/(ILp$Population2/100000)
      IN$CasesPer100K <- (IN$DailyAvg)/(INp$Population2/100000)
      KS$CasesPer100K <- (KS$DailyAvg)/(KSp$Population2/100000)
      KY$CasesPer100K <- (KY$DailyAvg)/(KYp$Population2/100000)
      LA$CasesPer100K <- (LA$DailyAvg)/(LAp$Population2/100000)
      MA$CasesPer100K <- (MA$DailyAvg)/(MAp$Population2/100000)
      MD$CasesPer100K <- (MD$DailyAvg)/(MDp$Population2/100000)
      ME$CasesPer100K <- (ME$DailyAvg)/(MEp$Population2/100000)
      MI$CasesPer100K <- (MI$DailyAvg)/(MIp$Population2/100000)
      MN$CasesPer100K <- (MN$DailyAvg)/(MNp$Population2/100000)
      MO$CasesPer100K <- (MO$DailyAvg)/(MOp$Population2/100000)
      #MP$CasesPer100K <- (MP$DailyAvg)/(MPp$Population2/100000)
      MS$CasesPer100K <- (MS$DailyAvg)/(MSp$Population2/100000)
      MT$CasesPer100K <- (MT$DailyAvg)/(MTp$Population2/100000)
      NC$CasesPer100K <- (NC$DailyAvg)/(NCp$Population2/100000)
      ND$CasesPer100K <- (ND$DailyAvg)/(NDp$Population2/100000)
      NE$CasesPer100K <- (NE$DailyAvg)/(NEp$Population2/100000)
      NH$CasesPer100K <- (NH$DailyAvg)/(NHp$Population2/100000)
      NJ$CasesPer100K <- (NJ$DailyAvg)/(NJp$Population2/100000)
      NM$CasesPer100K <- (NM$DailyAvg)/(NMp$Population2/100000)
      NV$CasesPer100K <- (NV$DailyAvg)/(NVp$Population2/100000)
      NY$CasesPer100K <- (NY$DailyAvg)/(NYp$Population2/100000)
      OH$CasesPer100K <- (OH$DailyAvg)/(OHp$Population2/100000)
      OK$CasesPer100K <- (OK$DailyAvg)/(OKp$Population2/100000)
      OR$CasesPer100K <- (OR$DailyAvg)/(ORp$Population2/100000)
      PA$CasesPer100K <- (PA$DailyAvg)/(PAp$Population2/100000)
      #PR$CasesPer100K <- (PR$DailyAvg)/(PRp$Population2/100000)
      RI$CasesPer100K <- (RI$DailyAvg)/(RIp$Population2/100000)
      SC$CasesPer100K <- (SC$DailyAvg)/(SCp$Population2/100000)
      SD$CasesPer100K <- (SD$DailyAvg)/(SDp$Population2/100000)
      TN$CasesPer100K <- (TN$DailyAvg)/(TNp$Population2/100000)
      TX$CasesPer100K <- (TX$DailyAvg)/(TXp$Population2/100000)
      UT$CasesPer100K <- (UT$DailyAvg)/(UTp$Population2/100000)
      VA$CasesPer100K <- (VA$DailyAvg)/(VAp$Population2/100000)
      #VI$CasesPer100K <- (VI$DailyAvg)/(VIp$Population2/100000)
      VT$CasesPer100K <- (VT$DailyAvg)/(VTp$Population2/100000)
      WA$CasesPer100K <- (WA$DailyAvg)/(WAp$Population2/100000)
      WI$CasesPer100K <- (WI$DailyAvg)/(WIp$Population2/100000)
      WV$CasesPer100K <- (WV$DailyAvg)/(WVp$Population2/100000)
      WY$CasesPer100K <- (WY$DailyAvg)/(WYp$Population2/100000)
      AK$CasesPer100K <- (AK$DailyAvg)/(AKp$Population2/100000)}
    
#Select Dates
{AK1<-subset(AK,date==date1)
      AL1<-subset(AL,date==date1)
      AR1<-subset(AR,date==date1)
      AZ1<-subset(AZ,date==date1)
      CA1<-subset(CA,date==date1)
      CO1<-subset(CO,date==date1)
      CT1<-subset(CT,date==date1)
      DC1<-subset(DC,date==date1)
      DE1<-subset(DE,date==date1)
      FL1<-subset(FL,date==date1)
      GA1<-subset(GA,date==date1)
      HI1<-subset(HI,date==date1)
      IA1<-subset(IA,date==date1)
      ID1<-subset(ID,date==date1)
      IL1<-subset(IL,date==date1)
      IN1<-subset(IN,date==date1)
      KS1<-subset(KS,date==date1)
      KY1<-subset(KY,date==date1)
      LA1<-subset(LA,date==date1)
      MA1<-subset(MA,date==date1)
      MD1<-subset(MD,date==date1)
      ME1<-subset(ME,date==date1)
      MI1<-subset(MI,date==date1)
      MN1<-subset(MN,date==date1)
      MO1<-subset(MO,date==date1)
      MP1<-subset(MP,date==date1)
      MS1<-subset(MS,date==date1)
      MT1<-subset(MT,date==date1)
      NC1<-subset(NC,date==date1)
      ND1<-subset(ND,date==date1)
      NE1<-subset(NE,date==date1)
      NH1<-subset(NH,date==date1)
      NJ1<-subset(NJ,date==date1)
      NM1<-subset(NM,date==date1)
      NV1<-subset(NV,date==date1)
      NY1<-subset(NY,date==date1)
      OH1<-subset(OH,date==date1)
      OK1<-subset(OK,date==date1)
      OR1<-subset(OR,date==date1)
      PA1<-subset(PA,date==date1)
      PR1<-subset(PR,date==date1)
      RI1<-subset(RI,date==date1)
      SC1<-subset(SC,date==date1)
      SD1<-subset(SD,date==date1)
      TN1<-subset(TN,date==date1)
      TX1<-subset(TX,date==date1)
      UT1<-subset(UT,date==date1)
      VA1<-subset(VA,date==date1)
      VI1<-subset(VI,date==date1)
      VT1<-subset(VT,date==date1)
      WA1<-subset(WA,date==date1)
      WI1<-subset(WI,date==date1)
      WV1<-subset(WV,date==date1)
      WY1<-subset(WY,date==date1)
      AK0<-subset(AK,date==date2)
      AL0<-subset(AL,date==date2)
      AR0<-subset(AR,date==date2)
      AZ0<-subset(AZ,date==date2)
      CA0<-subset(CA,date==date2)
      CO0<-subset(CO,date==date2)
      CT0<-subset(CT,date==date2)
      DC0<-subset(DC,date==date2)
      DE0<-subset(DE,date==date2)
      FL0<-subset(FL,date==date2)
      GA0<-subset(GA,date==date2)
      HI0<-subset(HI,date==date2)
      IA0<-subset(IA,date==date2)
      ID0<-subset(ID,date==date2)
      IL0<-subset(IL,date==date2)
      IN0<-subset(IN,date==date2)
      KS0<-subset(KS,date==date2)
      KY0<-subset(KY,date==date2)
      LA0<-subset(LA,date==date2)
      MA0<-subset(MA,date==date2)
      MD0<-subset(MD,date==date2)
      ME0<-subset(ME,date==date2)
      MI0<-subset(MI,date==date2)
      MN0<-subset(MN,date==date2)
      MO0<-subset(MO,date==date2)
      MP0<-subset(MP,date==date2)
      MS0<-subset(MS,date==date2)
      MT0<-subset(MT,date==date2)
      NC0<-subset(NC,date==date2)
      ND0<-subset(ND,date==date2)
      NE0<-subset(NE,date==date2)
      NH0<-subset(NH,date==date2)
      NJ0<-subset(NJ,date==date2)
      NM0<-subset(NM,date==date2)
      NV0<-subset(NV,date==date2)
      NY0<-subset(NY,date==date2)
      OH0<-subset(OH,date==date2)
      OK0<-subset(OK,date==date2)
      OR0<-subset(OR,date==date2)
      PA0<-subset(PA,date==date2)
      PR0<-subset(PR,date==date2)
      RI0<-subset(RI,date==date2)
      SC0<-subset(SC,date==date2)
      SD0<-subset(SD,date==date2)
      TN0<-subset(TN,date==date2)
      TX0<-subset(TX,date==date2)
      UT0<-subset(UT,date==date2)
      VA0<-subset(VA,date==date2)
      VI0<-subset(VI,date==date2)
      VT0<-subset(VT,date==date2)
      WA0<-subset(WA,date==date2)
      WI0<-subset(WI,date==date2)
      WV0<-subset(WV,date==date2)
      WY0<-subset(WY,date==date2)}
    
#Merge Files
{AK2<- merge(AK1,AK0, by="State")
      AL2<- merge(AL1,AL0, by="State")
      AR2<- merge(AR1,AR0, by="State")
      AZ2<- merge(AZ1,AZ0, by="State")
      CA2<- merge(CA1,CA0, by="State")
      CO2<- merge(CO1,CO0, by="State")
      CT2<- merge(CT1,CT0, by="State")
      DC2<- merge(DC1,DC0, by="State")
      DE2<- merge(DE1,DE0, by="State")
      FL2<- merge(FL1,FL0, by="State")
      GA2<- merge(GA1,GA0, by="State")
      HI2<- merge(HI1,HI0, by="State")
      IA2<- merge(IA1,IA0, by="State")
      ID2<- merge(ID1,ID0, by="State")
      IL2<- merge(IL1,IL0, by="State")
      IN2<- merge(IN1,IN0, by="State")
      KS2<- merge(KS1,KS0, by="State")
      KY2<- merge(KY1,KY0, by="State")
      LA2<- merge(LA1,LA0, by="State")
      MA2<- merge(MA1,MA0, by="State")
      MD2<- merge(MD1,MD0, by="State")
      ME2<- merge(ME1,ME0, by="State")
      MI2<- merge(MI1,MI0, by="State")
      MN2<- merge(MN1,MN0, by="State")
      MO2<- merge(MO1,MO0, by="State")
      MP2<- merge(MP1,MP0, by="State")
      MS2<- merge(MS1,MS0, by="State")
      MT2<- merge(MT1,MT0, by="State")
      NC2<- merge(NC1,NC0, by="State")
      ND2<- merge(ND1,ND0, by="State")
      NE2<- merge(NE1,NE0, by="State")
      NH2<- merge(NH1,NH0, by="State")
      NJ2<- merge(NJ1,NJ0, by="State")
      NM2<- merge(NM1,NM0, by="State")
      NV2<- merge(NV1,NV0, by="State")
      NY2<- merge(NY1,NY0, by="State")
      OH2<- merge(OH1,OH0, by="State")
      OK2<- merge(OK1,OK0, by="State")
      OR2<- merge(OR1,OR0, by="State")
      PA2<- merge(PA1,PA0, by="State")
      PR2<- merge(PR1,PR0, by="State")
      RI2<- merge(RI1,RI0, by="State")
      SC2<- merge(SC1,SC0, by="State")
      SD2<- merge(SD1,SD0, by="State")
      TN2<- merge(TN1,TN0, by="State")
      TX2<- merge(TX1,TX0, by="State")
      UT2<- merge(UT1,UT0, by="State")
      VA2<- merge(VA1,VA0, by="State")
      VI2<- merge(VI1,VI0, by="State")
      VT2<- merge(VT1,VT0, by="State")
      WA2<- merge(WA1,WA0, by="State")
      WI2<- merge(WI1,WI0, by="State")
      WV2<- merge(WV1,WV0, by="State")
      WY2<- merge(WY1,WY0, by="State")}
    
    
Testme<-rbind(AK2,	AL2,	AR2,	AZ2,	CA2,	CO2,	CT2,	DC2,	DE2,	FL2,	GA2,	HI2,	IA2,	ID2,	IL2,	IN2,	KS2,	KY2,	LA2,	MA2,	MD2,	ME2,	MI2,	MN2,	MO2,	MS2,	MT2,	NC2,	ND2,	NE2,	NH2,	NJ2,	NM2,	NV2,	NY2,	OH2,	OK2,	OR2,	PA2,	RI2,	SC2,	SD2,	TN2,	TX2,	UT2,	VA2,	VT2,	WA2,	WI2,	WV2,	WY2)
Testme2<-rbind(AK,	AL,	AR,	AZ,	CA,	CO,	CT,	DC,	DE,	FL,	GA,	HI,	IA,	ID,	IL,	IN,	KS,	KY,	LA,	MA,	MD,	ME,	MI,	MN,	MO,	MS,	MT,	NC,	ND,	NE,	NH,	NJ,	NM,	NV,	NY,	OH,	OK,	OR,	PA,	RI,	SC,	SD,	TN,	TX,	UT,	VA,	VT,	WA,	WI,	WV,	WY)
Testme2<-subset(Testme2,date>"2021-01-01")
Statetrend <- merge(Testme,PollsCheck, by="State")
Statetrend$CasesPer100K4Week<-(Statetrend$CasesPer100K.x-Statetrend$CasesPer100K.y)
Statetrend$CasesPer100K4WeekGroup <- ifelse(Statetrend$CasesPer100K4Week<mean(Statetrend$CasesPer100K4Week),"AboveAverage","BelowAverage")
Statetrend$CasesPer100KGroup <- ifelse(Statetrend$CasesPer100K.x>mean(Statetrend$CasesPer100K.x),"AboveAverage","BelowAverage")
Statetrend2 <- Statetrend
USASHAPE<-st_read("cb_2019_us_state_500k/cb_2019_us_state_500k.shp")
USASHAPE <- USASHAPE %>% rename(State=STUSPS)
USASHAPE<-inner_join(USASHAPE,Statetrend)
USASHAPE2<-readOGR("cb_2019_us_state_500k/cb_2019_us_state_500k.shp")
Statetrend2 <- Statetrend2 %>% rename(STUSPS=State)
USASHAPE3<-geo_join(USASHAPE2,Statetrend2,"STUSPS","STUSPS")
  
  
##Leaflet USA Map
pal<-colorNumeric("Reds",domain=USASHAPE3$CasesPer100K.x)
#popup_text<- paste0("Total: ",as.character(USASHAPE3$CasesPer100K.x))
#popup_text2<- paste0("Total: ",as.character(USASHAPE3$State1))
    
labels <- sprintf(
"<strong>%s</strong><br/>%g cases per 100K people</sup>",
USASHAPE3$State1, USASHAPE3$CasesPer100K.x
) %>% 
lapply(htmltools::HTML)
    
USACasesPer100k<-leaflet(options = leafletOptions(attributionControl=FALSE,minZoom = 4, maxZoom = 4,zoomControl = FALSE,dragging=FALSE))%>%
  setView(-98.483330,38.712046,zoom=4)%>%
  setMapWidgetStyle(list(background= "white"))%>%
  #addProviderTiles("CartoDB.Positron") %>%
  addPolygons(data=USASHAPE3,
              weight=1,
              smoothFactor = .5,
              color = "grey",
              fillOpacity = .8,
              fillColor = ~pal(USASHAPE3$CasesPer100K.x),
              #popup = ~popup_text,
              label = labels,
              labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                       padding = "3px 8px"),
                                              textsize = "15px",
                                              direction = "auto"))
withr::with_dir('~/Sites/SITE1', saveWidget(USACasesPer100k, file="USACasesPer100k.html",selfcontained = FALSE))

States<-ggplot(data = Testme2, aes(x = date, y = CasesPer100K)) +
  labs(title = "",y = "", x = "")+ 
  geom_line(color = "steelblue", size = .5) +
  #geom_point(color="steelblue", size=.5) + 
  facet_wrap(facets = vars(State1))+
  theme(plot.title = element_text(hjust = 0.5),
        axis.text = element_text( size = 7))
ggsave("States.jpeg", path="~/Sites/SITE1/",plot = States, dpi=700)
}
  

##TOWN MAP
{#PreProcess
library(readxl)
library(tmap)
library(sf)
library(tmaptools)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(ggpubr)
library(leaflet.extras)
library(leaflet)
library(rgdal)
library(readr)
library(tigris)
library(leaflet.providers)
options(scipen=999)


#Leaftlet Town Map

Town<-readOGR("towns/TOWNS_POLYM.shp")
Town1<-spTransform(Town, CRS("+init=epsg:4326"))
#TownTest<-spTransform(Town, CRS("+init=epsg:4326"))
CovidMaster <- read_excel("Sites/SITE1/_files/CovidMaster.xlsx",sheet = "CovidMaster")
Town2<-geo_join(Town1,CovidMaster,"TOWN_ID","Number1")
pal<-colorNumeric("Reds",domain=Town2$TwoWeekPer1000avg)

#TownTest <- as.data.frame(TownTest)
#write.csv(TownTest,'TownTest.csv')

  labels <- sprintf(
    "<strong>%s</strong><br/>%g cases per 1K people</sup>",
    Town2$TOWN, Town2$LastWeek
  ) %>% 
    lapply(htmltools::HTML)
  
  MATownsPer1K<-leaflet(options = leafletOptions(attributionControl=FALSE,minZoom = 8.25, maxZoom = 8.25,zoomControl = FALSE,dragging=FALSE,scrollWheelZoom = FALSE))%>%
    setMapWidgetStyle(list(background= "white"))%>%
    setView(-71.659690,42.260569,zoom=8.25)%>%
    addLegend(pal=pal,values = Town2$LastWeek,position = "topright", title = "Cases Per 1k",) %>%
    #addProviderTiles("CartoDB.Positron") %>%
    #suspendScroll(sleep = TRUE)%>%
    addPolygons(data=Town2,
                weight=1,
                smoothFactor = .5,
                color = "grey",
                fillOpacity = .8,
                fillColor = ~pal(Town2$LastWeek),
                #popup = ~popup_text,
                label = labels,
                labelOptions = labelOptions(style = list("font-weight" = "normal", 
                                                         padding = "3px 8px"),
                                            textsize = "15px",
                                            direction = "auto"))
  withr::with_dir('~/Sites/SITE1', saveWidget(MATownsPer1K, file="MATownsPer1K.html",selfcontained = FALSE))
}
  
  
##TOWN CHARTS
{
library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(ggpubr)
library(rgdal)
library(readr)
library(reshape2)
  
CovidMaster <- read_excel("Sites/SITE1/_files/CovidMaster.xlsx",sheet = "CovidMaster")
CovidMasterStats <- read_excel("Sites/SITE1/_files/CovidMaster.xlsx",sheet = "Stats")
  
CovidPly<- subset(CovidMaster,County=="Plymouth")
CovidPly<-CovidPly %>% select(Town, c(Week2:ncol(.)))
CovidPly<-CovidPly[1:(length(CovidPly)-3)]
CovidPly<-melt(CovidPly)
CovidPly<-inner_join(CovidPly,CovidMasterStats)
  
CovidBar<- subset(CovidMaster,County=="Barnstable")
CovidBar<-CovidBar %>% select(Town, c(Week2:ncol(.)))
CovidBar<-CovidBar[1:(length(CovidBar)-3)]
CovidBar<-melt(CovidBar)
CovidBar<-inner_join(CovidBar,CovidMasterStats)

CovidMid<- subset(CovidMaster,County=="Middlesex")
CovidMid<-CovidMid %>% select(Town, c(Week2:ncol(.)))
CovidMid<-CovidMid[1:(length(CovidMid)-3)]
CovidMid<-melt(CovidMid)
CovidMid<-inner_join(CovidMid,CovidMasterStats)
  
{  
CovidPly<-ggplot(data = CovidPly, aes(x = Date, y = value)) +
    labs(title = "",y = "", x = "")+ 
    geom_line(color = "steelblue", size = .5) +
    geom_point(color="steelblue", size=.5) + 
    facet_wrap(facets = vars(Town),nrow = 4)+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text( size = 7))
ggsave("CovidPly.jpeg", path="~/Sites/SITE1/",plot = CovidPly, dpi=300)
}

CovidBar<-ggplot(data = CovidBar, aes(x = Date, y = value)) +
    labs(title = "",y = "", x = "")+ 
    geom_line(color = "steelblue", size = .5) +
    geom_point(color="steelblue", size=.5) + 
    facet_wrap(facets = vars(Town),nrow = 3)+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text( size = 7))
ggsave("CovidBar.jpeg", path="~/Sites/SITE1/",plot = CovidBar,dpi=300)

{  
  CovidMid<-ggplot(data = CovidMid, aes(x = Date, y = value)) +
    labs(title = "",y = "", x = "")+ 
    geom_line(color = "steelblue", size = .5) +
    geom_point(color="steelblue", size=.5) + 
    facet_wrap(facets = vars(Town),nrow = 4)+
    theme(plot.title = element_text(hjust = 0.5),
          axis.text = element_text( size = 7))
  ggsave("CovidMid.jpeg", path="~/Sites/SITE1/",plot = CovidMid, dpi=300)
}

}

   
##TOWN TOTAL INFECTED
{library(readxl)
library(dplyr)
library(ggplot2)
library(plotly)
library(ggthemes)
library(ggpubr)
library(rgdal)
library(readr)
library(reshape2)
  
options(scipen=999)
CovidMaster <- read_excel("Sites/SITE1/_files/CovidMaster.xlsx",sheet = "CovidMaster")
Covid99<-CovidMaster %>% select(Town, County, TwoWeekPer1000avg, TotalInfectedPer100, PopDen,KidPop,Trump, Biden,MHI,Percent20s)
Covid99<-Covid99 %>%
  group_by(County) %>%
  mutate(Group=ifelse(TotalInfectedPer100>mean(TotalInfectedPer100),"AboveAverage","BelowAverage"))%>%
  as.data.frame()
  
ply<-ggdotchart(subset(Covid99,County=="Plymouth"), x = "Town", y = "TotalInfectedPer100",
                color = "Group",
                palette = c("#00AFBB", "#E7B800"),
                sorting = "descending",
                add = "segments",
                rotate = FALSE,
                group = "Group",
                dot.size = 9,
                #title= "Plymouth County",
                label = round(subset(Covid99,County=="Plymouth")$TotalInfectedPer100,digits=1),
                font.label = list(color = "black", size = 9, 
                                  vjust = 0.5),
                ggtheme = theme_pubr(),
)+
  ggpar(theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()))
  
ggsave("ply.jpeg", path="~/Sites/SITE1/",dpi=900, device="jpeg")
  
  
bar<-ggdotchart(subset(Covid99,County=="Barnstable"), x = "Town", y = "TotalInfectedPer100",
                color = "Group",
                palette = c("#00AFBB", "#E7B800"),
                sorting = "descending",
                add = "segments",
                rotate = FALSE,
                group = "Group",
                dot.size = 9,
                #title= "Barnstable County",
                label = round(subset(Covid99,County=="Barnstable")$TotalInfectedPer100,digits=1),
                font.label = list(color = "black", size = 9, 
                                  vjust = 0.5),
                ggtheme = theme_pubr(),
)+
ggpar(theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()))
  
ggsave("bar.jpeg", path="~/Sites/SITE1/",dpi=900, device="jpeg") 


mid<-ggdotchart(subset(Covid99,County=="Middlesex"), x = "Town", y = "TotalInfectedPer100",
                color = "Group",
                palette = c("#00AFBB", "#E7B800"),
                sorting = "descending",
                add = "segments",
                rotate = FALSE,
                group = "Group",
                dot.size = 9,
                #title= "Middlesex County",
                label = round(subset(Covid99,County=="Middlesex")$TotalInfectedPer100,digits=1),
                font.label = list(color = "black", size = 9, 
                                  vjust = 0.5),
                ggtheme = theme_pubr(),
)+
  ggpar(theme(plot.title = element_text(hjust = 0.5),legend.position = "none",axis.title.x = element_blank(),axis.title.y = element_blank()))

ggsave("mid.jpeg", path="~/Sites/SITE1/",dpi=900, device="jpeg") 


####ValueBoxs

#rmarkdown::render("valueboxes.Rmd")
#this runs from the main directory

}
}
