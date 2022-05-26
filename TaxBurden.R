###Tax Burden on Labor in the OECD###

#Clear working environment####
rm(list=ls())
gc()

#general set-up
using<-function(...,prompt=TRUE){
  libs<-sapply(substitute(list(...))[-1],deparse)
  req<-unlist(lapply(libs,require,character.only=TRUE))
  need<-libs[req==FALSE]
  n<-length(need)
  installAndRequire<-function(){
    install.packages(need)
    lapply(need,require,character.only=TRUE)
  }
  if(n>0){
    libsmsg<-if(n>2) paste(paste(need[1:(n-1)],collapse=", "),",",sep="") else need[1]
    if(n>1){
      libsmsg<-paste(libsmsg," and ", need[n],sep="")
    }
    libsmsg<-paste("The following packages count not be found: ",libsmsg,"n\r\n\rInstall missing packages?",collapse="")
    if(prompt==FALSE){
      installAndRequire()
    }else if(winDialog(type=c("yesno"),libsmsg)=="YES"){
      installAndRequire()
    }
  }
}

setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

using(gtools)
using(plyr)
using(reshape2)
using(OECD)
using(readxl)
using(rvest)
using(htmltab)
using(tidyverse)
using(stringr)
using(dplyr)
using(naniar)
using(rsdmx)

oecd_countries<-c("AUS",
                  "AUT",
                  "BEL",
                  "COL",
                  "CAN",
                  "CHL",
                  "CRI",
                  "CZE",
                  "DNK",
                  "EST",
                  "FIN",
                  "FRA",
                  "DEU",
                  "GRC",
                  "HUN",
                  "ISL",
                  "IRL",
                  "ISR",
                  "ITA",
                  "JPN",
                  "KOR",
                  "LVA",
                  "LUX",
                  "LTU",
                  "MEX",
                  "NLD",
                  "NZL",
                  "NOR",
                  "POL",
                  "PRT",
                  "SVK",
                  "SVN",
                  "ESP",
                  "SWE",
                  "CHE",
                  "TUR",
                  "GBR",
                  "USA")

#Reading in and cleaning OECD's Taxing Wages dataset###

#dataset_list<-get_datasets()
#data<-search_dataset("Tax wages", data= dataset_list)
#dataset<-("AWCOMP")
#AWCOMP is Taxing wages _ comparative tables#
#dstruc<-get_data_structure(dataset)
#str(dstruc, max.level = 1)
#dstruc$INDICATOR
#dstruc$FAM_TYPE
#dstruc$COU
#dstruc$VAR_DESC
#dstruc$UNIT
#dstruc$POWERCODE
#dstruc$REFERENCEPERIOD
#dataset <- ("TXWDECOMP")
#dstruc$ER

#dataset_list <- get_datasets()

#search_dataset("Table I.4.", data= dataset_list)
#datasetI4 <- ("TABLE_I4")
#dstruc <- get_data_structure(datasetI4)
#str(dstruc, max.level = 1)
#dstruc$VAR_DESC
#dstruc$INCOMEAW
#dstruc$MARGRATES


#Read CSV OECD Country codes
iso_codes <- read.csv("source-data/iso_country_codes.csv")
colnames(iso_codes)<-c("Label","ISO2","Country")

#Table 2: Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage, 2020####
#144 Average tax wedge (% labour costs); 1441 Income tax as % of labour costs; 1442   Employee SSC as % of labour costs;1443 Average rate of employer's social security contributions (% gross wage earnings)###

data <-get_dataset("AWCOU",filter= list(c("144","1441","1442","1443"),c("SINGLE2"),c(oecd_countries,"OAVG")),start_time = 2021)
data_labourcost<-get_dataset("AWCOMP",filter= list(c("5_1","5_2","5_3"),c("SINGLE2"),c(oecd_countries,"OAVG")),start_time = 2021)

#Save Gross Earnings in USDPP for MTW
Gross_Earnings_USDPP <-  subset(data_labourcost, INDICATOR == "5_1")
Gross_Earnings_USDPP <-  subset(Gross_Earnings_USDPP, select=-c(TIME_FORMAT, UNIT, POWERCODE, Time, FAM_TYPE, INDICATOR))
colnames(Gross_Earnings_USDPP)[colnames(Gross_Earnings_USDPP)=="ObsValue"] <- "Gross_USDPP"
data <- rbind (data, data_labourcost)

#Drop redundant columns
data <- subset(data, select=-c(TIME_FORMAT, UNIT, POWERCODE, Time, FAM_TYPE))

#Put data into columns, add a rank column, order the data and rearrange the columns
data$ObsValue<-as.numeric(data$ObsValue)

table1 <- spread(data, INDICATOR, ObsValue)
table1$Rank <- rank(100-table1$`144`)
#Double check that OAVG is ranked 24th
table1$Rank<-if_else(table1$Rank>24,table1$Rank-1,table1$Rank)

#Double check that OAVG is on row 32
table1["32", "Rank"] <- 39
table1$Rank<-as.numeric(table1$Rank)

table1 <- table1[c ("COU","Rank", 144, 1441, 1442, 1443, "5_3")]

table1 [order(-table1$Rank),]

#Rename columns
colnames(table1)[colnames(table1)=="COU"] <- "Country"
colnames(table1)[colnames(table1)=="144"] <- "Tax Wedge in % (As a Share of Labor Cost)"
colnames(table1)[colnames(table1)=="1441"] <- "Income Tax in %"
colnames(table1)[colnames(table1)=="1442"] <- "Employee Payroll Taxes in %"
colnames(table1)[colnames(table1)=="1443"] <- "Employer Payroll Taxes in %"
colnames(table1)[colnames(table1)=="5_3"] <- "Total Average Annual Labor Cost per Employee in $"

table1<-merge(iso_codes,table1,by=c("Country"))
#table1<-table1[,-c(1,3)]
colnames(table1)[colnames(table1)=="Country"] <- "ISO3"
colnames(table1)[colnames(table1)=="Label"] <- "Country"
write.csv(table1,"final-outputs/table1.csv",row.names = F)


#Table 3: Tax Wedge Including VAT of a Single Worker with no Children Earning a Nation's Average Wage, 2020####
table2 <- spread (data, key = INDICATOR, value = ObsValue)

#Read in VAT file
VAT_data <- read.csv("source-data/VAT_data.csv")

#MERGE VAT file and table 3
table2<- merge(table2, VAT_data, by='COU')

table2$"1441_USD"<-table2$"1441"/100*table2$"5_3"
table2$"1442_USD"<-table2$"1442"/100*table2$"5_3"
table2$"1443_USD"<-table2$"1443"/100*table2$"5_3"
table2$"VAT_effective_%"<-table2$"VAT_rate"/(100+table2$"VAT_rate")*table2$"VRR"
table2$"VAT_amount"<-table2$"VAT_effective"/100*table2$"5_3"
table2$"Tax Wedge Including VAT in % (As Share of Labor Cost and VAT)"<- (table2$"1441_USD"+table2$"1442_USD"+ table2$"1443_USD"+table2$"VAT_amount")/(table2$"5_3"+table2$"VAT_amount")
table2$"Total Average Annual Labor Cost per Employee Including VAT in $" <-(table2$"VAT_amount"+table2$"5_3")
table2$"Income Tax in % (As Share of Labor Cost Including VAT)" <- table2$"1441_USD"/table2$"Total Average Annual Labor Cost per Employee Including VAT in $"
table2$"Employee and Employer Payroll Taxes in % (As Share of Labor Costs Including VAT)" <- (table2$"1442_USD"+ table2$"1443_USD")/table2$"Total Average Annual Labor Cost per Employee Including VAT in $"
table2$"VAT in % (As Share of Labor Costs Including VAT)" <- table2$VAT_amount/table2$"Total Average Annual Labor Cost per Employee Including VAT in $"
table2$Rank <- rank (100-table2$"Tax Wedge Including VAT in % (As Share of Labor Cost and VAT)")
#Double check that OAVG is ranked 24rd
table2$Rank<-if_else(table2$Rank>24,table2$Rank-1,table2$Rank)

#Double check that OAVG is on row 32
table2["32", "Rank"] <- "NA"

#Saving data from table 3 to use it in Figure 6
figure6 <- table2

table2 <- table2[c ("COU","Rank", "Tax Wedge Including VAT in % (As Share of Labor Cost and VAT)", "Income Tax in % (As Share of Labor Cost Including VAT)", "Employee and Employer Payroll Taxes in % (As Share of Labor Costs Including VAT)","VAT in % (As Share of Labor Costs Including VAT)", "Total Average Annual Labor Cost per Employee Including VAT in $")]
table2$Rank<-as.numeric(table2$Rank)
#Rename columns
colnames(table2)[colnames(table2)=="COU"] <- "Country"
table2<-merge(iso_codes,table2,by=c("Country"))
table2<-table2[,-c(1,3)]
colnames(table2)[colnames(table2)=="Label"] <- "Country"


write.csv(table2,"final-outputs/table2.csv",row.names = F)

#Figure 1 Average OECD Tax Burden. OECD  Average of the Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage, 2020####

Figure1_data <- table1
Figure1_data <- subset.data.frame (Figure1_data, Figure1_data$"Country" =="OECD Average")

print(colnames(Figure1_data))

variable<-c("After-Tax Income","Income Tax", "Employee Payroll Taxes", "Employer Payroll Taxes","Pre-tax")
percent<-c(100-Figure1_data["32","Tax Wedge in % (As a Share of Labor Cost)"],
           Figure1_data["32","Income Tax in %"],
           Figure1_data["32","Employee Payroll Taxes in %"],
           Figure1_data["32","Employer Payroll Taxes in %"],
           100)
Figure1<-data.frame(variable,percent)
Figure1$dollar<-Figure1$percent/100*Figure1_data["32","Total Average Annual Labor Cost per Employee in $"]

write.csv(Figure1,"final-outputs/Figure1.csv",row.names = F)


#Figure 2 Tax Burden in OECD Countries.Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage####
Figure2 <- subset (data, subset = INDICATOR == 144)

#Drop redundant columns
Figure2 <- subset (Figure2, select=-c(INDICATOR))

#Rename columns
colnames(Figure2)[colnames(Figure2)=="COU"] <- "Country"
colnames(Figure2)[colnames(Figure2)=="ObsValue"] <- "Tax Wedge in %"
Figure2 [order(-Figure2$"Tax Wedge in %"),]

Figure2<-merge(iso_codes,Figure2,by=c("Country"))
Figure2<-Figure2[,-c(1,3)]
colnames(Figure2)[colnames(Figure2)=="Label"] <- "Country"

write.csv(Figure2, "final-outputs/Figure2.csv",row.names = F)

#Figure 3  Tax Burden of Singles vs. Families. Tax Wedges at a Nation's Average Wage, 2021####

Figure3<-get_dataset("AWCOU",filter= list(c("144"),c("SINGLE2","MARRIED1")),start_time = 2021)

#Drop redundant columns
Figure3 <- subset(Figure3, select=-c(INDICATOR, TIME_FORMAT, UNIT, POWERCODE, Time))

#Put data into columns

Figure3 <- spread (Figure3, key = FAM_TYPE, value = ObsValue)
Figure3 <-Figure3 [c ("COU","SINGLE2","MARRIED1")]

#Rename columns
colnames(Figure3)[colnames(Figure3)=="COU"] <- "Country"
colnames(Figure3)[colnames(Figure3)=="MARRIED1"] <- "Tax Wedge in %. One-Earner Married with Two Children"
colnames(Figure3)[colnames(Figure3)=="SINGLE2"] <- "Tax Wedge in %. Single Worker with no Children"

Figure3<-merge(iso_codes,Figure3,by=c("Country"))
Figure3<-Figure3[,-c(1,3)]
colnames(Figure3)[colnames(Figure3)=="Label"] <- "Country"

write.csv(Figure3, "final-outputs/Figure3.csv",row.names = F)

#Figure 4 OECD Average Tax Burden, 2000-2020. Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage####
Figure4<-get_dataset ("AWCOU",filter= list(c("144"),c("SINGLE2"), c("OAVG")), start_time = 2000)

#Drop redundant columns
Figure4 <- subset(Figure4, select=-c(INDICATOR, FAM_TYPE, COU, TIME_FORMAT, POWERCODE))

#Rename columns
colnames(Figure4)[colnames(Figure4)=="Time"] <- "Year"
colnames(Figure4)[colnames(Figure4)=="ObsValue"] <- "OECD Average"

write.csv(Figure4, "final-outputs/Figure4.csv",row.names = F)


#Figure 5 Most Notable Changes in the Tax Wedge between 2000 and 2020####

Figure5 <-get_dataset("AWCOU",filter= list(c("144"),c("SINGLE2")),start_time = 2021)

Figure5_2000 <-get_dataset("AWCOU",filter= list(c("144"),c("SINGLE2")),start_time = 2000, end_time = 2000)

Figure5 <- rbind (Figure5,Figure5_2000)

#Drop the average of OECD and redundant columns
Figure5 <- subset(Figure5, Figure5$COU !="OAVG")
Figure5 <- subset(Figure5, select=-c(INDICATOR, FAM_TYPE, TIME_FORMAT, UNIT, POWERCODE))


#Put data into columns
Figure5 <- spread (Figure5, key = Time, value = ObsValue)

#Determine the 6 countries with the Most Notable Changes in the Tax Wedge between 2000 and 2020
Figure5$`2000`<-as.numeric(Figure5$`2000`)
Figure5$`2021`<-as.numeric(Figure5$`2021`)

Figure5$dif <- Figure5$'2021'- Figure5$'2000'
Figure5$abs <- abs(Figure5$dif)
Figure5$Rank <- rank (Figure5$abs)
Figure5 <- subset(Figure5, Figure5$Rank > 32)

#Drop redundant columns
Figure5 <- subset (Figure5, select=-c(2,3,abs, Rank))

#Rename columns
colnames(Figure5)[colnames(Figure5)=="COU"] <- "Country"
colnames(Figure5)[colnames(Figure5)=="dif"] <- "Change in Tax Wedge between 2000 and 2021"

Figure5 [order(Figure5$"Change in Tax Wedge between 2000 and 2021"),]

Figure5<-merge(iso_codes,Figure5,by=c("Country"))
Figure5<-Figure5[,-c(1,3)]
colnames(Figure5)[colnames(Figure5)=="Label"] <- "Country"

write.csv(Figure5, "final-outputs/Figure5.csv",row.names = F)

#Figure 6 Tax Burden Accounting for VAT in OECD Countries. Tax Wedge of a Single Worker with no Children Earning a Nation's Average Wage, 2020####
figure6 <- figure6 [c ("COU", 144, "Tax Wedge Including VAT in % (As Share of Labor Cost and VAT)")]

#Rename columns
colnames(figure6)[colnames(figure6)=="COU"] <- "Country"
colnames(figure6)[colnames(figure6)=="144"] <- "Tax Wedge Excluding VAT"
colnames(figure6)[colnames(figure6)=="Tax Wedge Including VAT in % (As Share of Labor Cost and VAT)"] <- "Tax Wedge Accounting for VAT"

figure6$`Tax Wedge Accounting for VAT`<-figure6$`Tax Wedge Accounting for VAT`*100
figure6<-merge(iso_codes,figure6,by=c("Country"))
figure6<-figure6[,-c(1,3)]
colnames(figure6)[colnames(figure6)=="Label"] <- "Country"

write.csv(figure6, "final-outputs/figure6.csv",row.names = F)

###Reading in and cleaning tax wedge decomposition###

#dataset_list<-get_datasets()
#data_MTW<-search_dataset("Tax wedge decomposition", data= dataset_list)
#dataset <- ("TXWDECOMP")
#dstruc <- get_data_structure(dataset)
#str(dstruc, max.level = 1)
#dstruc$INDICATOR
#dstruc$FAM_TYPE
#dstruc$COU
#dstruc$VAR_DESC
#dstruc$UNIT
#dstruc$ER
#dstruc$YEA
#Figure 8 (not in 2021 report) Economic Cost for the Marginal Dollar of Revenue Collected from Labor. Ratio of Marginal Tax Wedge to Average Tax Wedge, 2020####
#martax_wedge
#Table_I4#
#dataset<-("Table_I4")
#dstruc<-get_data_structure(dataset)
#str(dstruc, max.level = 1)
#dstruc$VAR_DESC
#dstruc$INCOMEAW
#dstruc$CL_TABLE_I4_MARGRATES

#martax_wedge<-get_dataset("Table_I4",filter= list(c(oecd_countries),c("67","100","133","167"),c("TOT_TAX_WEDGE")), start_time = 2020)


#martax_wedge<-martax_wedge[c(1,2,5,6)]

#colnames(martax_wedge)<-c("Country","Income","Year","martax_wedge")
#martax_wedge<-spread(martax_wedge,Year,martax_wedge)

#OAVG_67<-mean(subset(martax_wedge$`2020`,martax_wedge$Income==67))
#OAVG_100<-mean(subset(martax_wedge$`2020`,martax_wedge$Income==100))
#OAVG_133<-mean(subset(martax_wedge$`2020`,martax_wedge$Income==133))
#OAVG_167<-mean(subset(martax_wedge$`2020`,martax_wedge$Income==167))

#Country<-c("OAVG","OAVG","OAVG","OAVG")
#Income<-c("67","100","133","167")
#Value<-c(OAVG_67,OAVG_100,OAVG_133,OAVG_167)

#OAVG<-data.frame(Country,Income,Value)
#colnames(OAVG)<-c("Country","Income","2020")

#martax_wedge<-rbind(martax_wedge,OAVG)

#martax_wedge2020<-aggregate(martax_wedge$`2020`,by=list(martax_wedge$Country),FUN=mean)


#avgtax_wedge
#Table_I5#
#dataset<-("Table_I5")
#dstruc<-get_data_structure(dataset)
#str(dstruc, max.level = 1)
#dstruc$VAR_DESC
#dstruc$INCOMEAW
#dstruc$CL_TABLE_I4_MARGRATES

#avgtax_wedge<-get_dataset("Table_I5",filter= list(c(oecd_countries),c("67","100","133","167"),c("TOT_TAX_WEDGE")), start_time = 2020)

#avgtax_wedge<-avgtax_wedge[c(1,2,5,6)]

#colnames(avgtax_wedge)<-c("Country","Income","Year","avgtax_wedge")
#avgtax_wedge<-spread(avgtax_wedge,Year,avgtax_wedge)

#OAVG_67<-mean(subset(avgtax_wedge$`2020`,avgtax_wedge$Income==67))
#OAVG_100<-mean(subset(avgtax_wedge$`2020`,avgtax_wedge$Income==100))
#OAVG_133<-mean(subset(avgtax_wedge$`2020`,avgtax_wedge$Income==133))
#OAVG_167<-mean(subset(avgtax_wedge$`2020`,avgtax_wedge$Income==167))

#Country<-c("OAVG","OAVG","OAVG","OAVG")
#Income<-c("67","100","133","167")
#Value<-c(OAVG_67,OAVG_100,OAVG_133,OAVG_167)

#OAVG<-data.frame(Country,Income,Value)
#colnames(OAVG)<-c("Country","Income","2020")

#avgtax_wedge<-rbind(avgtax_wedge,OAVG)


#avgtax_wedge2020<-aggregate(avgtax_wedge$`2020`,by=list(avgtax_wedge$Country),FUN=mean)

#countries<-avgtax_wedge2020$Group.1

#tax_wedge2020<-martax_wedge2020$x/avgtax_wedge2020$x

#figure8<-data.frame(countries,tax_wedge2020)

#colnames(figure8)<-c("Country","Marginal to Average Tax Wedge Ratio")
#figure8<-merge(iso_codes,figure8,by=c("Country"))
#figure8<-figure8[,-c(1,3)]
#colnames(figure8)[colnames(figure8)=="Label"] <- "Country"

#write.csv(figure8,"final-outputs/figure8.csv",row.names = F)



#Marginal Tax Wedges for country profile page charts####
data_MTW <-get_dataset("TXWDECOMP",filter= list(c(oecd_countries, "OAVG"),c("MRG_TX_WEDGE"),c("SGL"), c("50","51","52","53","54","55","56","57","58","59","60","61","62","63","64","65","66","67","68","69","70","71","72","73","74","75","76","77","78","79","80","81","82","83","84","85","86","87","88","89","90","91","92","93","94","95","96","97","98","99","100","101","102","103","104","105","106","107","108","109","110","111","112","113","114","115","116","117","118","119","120","121","122","123","124","125","126","127","128","129","130","131","132","133","134","135","136","137","138","139","140","141","142","143","144","145","146","147","148","149","150")),start_time = 2021)
table_MTW <- data_MTW[c(1,2,5)]

#Merge MTW with Average Gross Earnings in USDPP
table_MTW <- merge(table_MTW,Gross_Earnings_USDPP,by=c("COU"))
table_MTW$ER<-as.numeric(table_MTW$ER)
table_MTW$"Gross_USDPP"<-as.numeric(table_MTW$"Gross_USDPP")

#Calculate Gross Earnings for each percentile
table_MTW$"Gross_USDPP"<-table_MTW$"ER"*table_MTW$"Gross_USDPP"/100

#Round Gross Earnings to Hundreds Place
table_MTW$Gross_USDPP<-round(table_MTW$Gross_USDPP,-2)

#rename columns
colnames(table_MTW)[colnames(table_MTW)=="obsValue"] <- "Marginal Tax Wedge"
colnames(table_MTW)[colnames(table_MTW)=="ER"] <- "% of Total Earnings"
colnames(table_MTW)[colnames(table_MTW)=="Gross_USDPP"] <- "Annual Gross Wage in US Dollars"

write.csv(table_MTW, "final-outputs/table_MTW.csv",row.names = F)
