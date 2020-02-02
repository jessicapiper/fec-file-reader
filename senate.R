library(googlesheets4)
library(jsonlite)

#reads in files
fec_files <- c("collins-year-end.csv","gideon-year-end.csv","lajeunesse-year-end.csv","sweet-year-end.csv","savage-year-end.csv","kidman-year-end.csv")

# pac lists
leadership_pacs <- read.csv("leadership_pacs.csv",stringsAsFactors = FALSE,header=FALSE)
business_pacs <- read.csv("business_pacs.csv",stringsAsFactors = FALSE,header=FALSE)
ideological_pacs <- read.csv("ideological_pacs.csv",stringsAsFactors = FALSE,header=FALSE)
labor_pacs <- read.csv("labor_pacs.csv",stringsAsFactors = FALSE,header = FALSE)

topline_data <- function(){
  
  header <- c("candidate","reporting_period","total_net_contributions","total_spending","cash_on_hand","percent_from_small_donors","percent_from_large_donors","percent_from_PACs")
  
  for(each in fec_files){
    fec_data <- as.matrix(read.csv(each, header = FALSE, stringsAsFactors = FALSE))
    
    topline <- fec_data[2,]
    
    fec_data = fec_data[-c(1,2),]
    fec_data = fec_data[,-c(45:93)]
    
    #basic information
    candidate <- as.character(topline[3])
    period <- as.character(topline[12])
    
    #this quarter
    net_contributions <- as.numeric(topline[26])
    cash_on_hand <- as.numeric(topline[30])
    itemized <- as.numeric(topline[33]) - as.numeric(topline[52])
    unitemized <- as.numeric(topline[34])
    pac_money <- as.numeric(topline[37]) - as.numeric(topline[39])
    spent <- as.numeric(topline[27])
    
    #totals
    t_net_contributions <- as.numeric(topline[65])
    t_itemized <- as.numeric(topline[69]) - as.numeric(topline[88])
    t_unitemized <- as.numeric(topline[70])
    t_pac_money <- as.numeric(topline[73]) - as.numeric(topline[80])
    t_spent <- as.numeric(topline[66])
    t_jfcs <- as.numeric(topline[76])

    #exports key figures:
    result <- c(candidate, period, t_net_contributions+t_jfcs,t_spent,cash_on_hand,t_unitemized/t_net_contributions,t_itemized/t_net_contributions,t_pac_money/t_net_contributions)
    header <- cbind(header,result)
  }
  
  #sends to google docs
  to_export <- data.frame(header)
  sheets_append(to_export,ss = "1AHyU29SfGeXtHhiLsoWrHrXELqj4uT6DqfCzi7x81M8", sheet=1)
  
}

get_pac_data <- function(file,p){
  raw_data <- read.csv(file,header=FALSE,stringsAsFactors = FALSE)
  
  cycle <- 2020
  recipient <- as.character(raw_data[2,3])
  recipid <- as.character(raw_data[2,2])
  party <- p
  parent <- NA
  pacs<-c("Cycle","Cmteid","PAC","Parent","Amount","Date","Recipient","Recipid","Party","BLIO")
  
  for(i in 1:nrow(raw_data)){
    if(raw_data[i,1]=="SA11C"){
      #print(raw_data[i,7])
      cmteid <- as.character(raw_data[i,26])
      pac <- as.character(raw_data[i,7])
      amount <- as.numeric(as.character(raw_data[i,21]))
      #print(amount)
      date <- as.character(raw_data[i,20])
      
      blio <- ""
      if(pac %in% leadership_pacs[,1]){
        blio = "leadership"
      }
      else if(pac %in% business_pacs[,1]){
        blio = "business"
      }
      else if(pac %in% labor_pacs[,1]){
        blio = "labor"
      }
      else if(pac %in% ideological_pacs[,1]){
        blio = "ideological"
      }
      
      full_row <- c(cycle,cmteid,pac,parent,amount,date,recipient,recipid,party,blio)
      pacs <- rbind(pacs,full_row)
    }
  }
  
  sheets_append(data.frame(pacs),ss = "1AHyU29SfGeXtHhiLsoWrHrXELqj4uT6DqfCzi7x81M8", sheet=2)
  
}
