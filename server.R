# server.R
library(shiny)
library(readxl)
library(timeDate)
library(tm)
library(lubridate)


month.dates <- seq(as.Date("2015-03-01"), length=650, by="1 month") - 1
month.dates[1] <- as.Date("2015-02-27")  # force first date to be the 27th
dates<-seq(as.Date("2013-03-01"), length=666, by="1 month") - 1

transform_MarketRates <- function(market.rates){
  market.rates<-market.rates[!is.na(market.rates$Date),]  # remove unneeded rows
  market.rates<-market.rates[,c(-7)]  # remove unneeded columns
  names(market.rates)<-unlist(lapply(names(market.rates),function(x) gsub(" ","",x)))  # rename columns without spaces
  names(market.rates)[2]<-"NA_"
  market.rates[,"ImpliedBankRate"]<-unlist(lapply(market.rates[,"ImpliedBankRate"],function(x) {max(x,0.5,na.rm = T)}))
  market.rates
}
MarketRates<-read_excel("data/apfcashtransfersupdatefeb15.xlsx", sheet = "Market rates")
MarketRates<-transform_MarketRates(MarketRates)

transform_ModelledRates <- function(modelled.rates,first.rate.rise,target.rate,pace.of.rate.rises){
  modelled.rates<-modelled.rates[!is.na(modelled.rates$Date),]
  names(modelled.rates)<-unlist(lapply(names(modelled.rates),function(x) gsub(" ","",x)))  # rename columns without spaces
  names(modelled.rates)[2]<-"NA_"
  
  modelled.rates[1,"ImpliedBankRate"]<-0.5
  i<-2  # ignore first value
  while(i<nrow(modelled.rates)){
    if(as.Date(modelled.rates[i,"Date"])>=as.Date(first.rate.rise) & modelled.rates[i-1,"ImpliedBankRate"]<target.rate){
      modelled.rates[i,"ImpliedBankRate"]<-min(modelled.rates[i-1,"ImpliedBankRate"]+(pace.of.rate.rises/300),target.rate)
    }else{
      modelled.rates[i,"ImpliedBankRate"]<-modelled.rates[i-1,"ImpliedBankRate"]
    }
    i<-i+1
  }
  modelled.rates
}
ModelledRates.original<-read_excel("data/apfcashtransfersupdatefeb15.xlsx", sheet = "Modelled rates")

transform_InterestRates <- function(interest.rates,market.rates,modelled.rates,use.market.path){
  names(interest.rates)<-unlist(lapply(names(interest.rates),function(x) gsub(" ","",x)))  # rename columns without spaces
  names(interest.rates)[2]<-"NA_"
  
  if(use.market.path){
    interest.rates[,"ForwardBankRate"]<-market.rates[,"ImpliedBankRate"]
  }else{
    interest.rates[,"ForwardBankRate"]<-modelled.rates[,"ImpliedBankRate"]
  }
  
  interest.rates[1,"BankRatediscountfactor"]<-1
  i<-2  # ignore first value
  while(i<nrow(interest.rates)){
    interest.rates[i,"BankRatediscountfactor"]<-((1+(interest.rates[i,"ForwardBankRate"]/100))^((as.numeric(interest.rates[i-1,"Date"]-interest.rates[i,"Date"])/365))*interest.rates[i-1,"BankRatediscountfactor"])
    i<-i+1
  }
  
  interest.rates[1,"SpotBankRate"]<-0.5
  i<-2  # ignore first value
  while(i<nrow(interest.rates)){
    interest.rates[i,"SpotBankRate"]<-100*(-1+interest.rates[i,"BankRatediscountfactor"]^(-365/as.numeric(as.Date(interest.rates[i,"Date"])-as.Date(interest.rates[1,"Date"]))))
    i<-i+1
  }
  
  interest.rates[,"Spotgiltyield"]<-market.rates[,"Spotgiltyield(annualised)"]
  interest.rates[,"Startingtermpremium"]<-interest.rates[,"Spotgiltyield"]-interest.rates[,"SpotBankRate"]
  interest.rates
}
InterestRates.original<-read_excel("data/apfcashtransfersupdatefeb15.xlsx", sheet = "Interest rates")

transform_PurchaseHistory <- function(purchase.history){
  purchase.history<-purchase.history[!is.na(purchase.history$ISIN),]
  names(purchase.history)<-unlist(lapply(names(purchase.history),function(x) removePunctuation(gsub(" ","",x))))  # rename columns without spaces
  purchase.history
}
PurchaseHistory <- read_excel("data/apfcashtransfersupdatefeb15.xlsx", sheet = "Purchase history",skip = 1)
PurchaseHistory <- transform_PurchaseHistory(PurchaseHistory)

transform_GiltHoldingsAndCashflows <- function(gilt.holdings.and.cashflows,purchase.history){
  gilt.holdings.and.cashflows<-gilt.holdings.and.cashflows[!is.na(gilt.holdings.and.cashflows$ISIN),]
  names(gilt.holdings.and.cashflows)<-unlist(lapply(names(gilt.holdings.and.cashflows),function(x) removePunctuation(gsub(" ","",x))))  # rename columns without spaces
  
  gilt.holdings.and.cashflows$Maturitydate<-as.Date(substr(gilt.holdings.and.cashflows$Bond,start = nchar(gilt.holdings.and.cashflows$Bond)-5,stop = nchar(gilt.holdings.and.cashflows$Bond)),format = "%d%m%y")
  gilt.holdings.and.cashflows$Maturitymonth<-as.Date(alignMonthly(gilt.holdings.and.cashflows[,"Maturitydate"],include.weekends = T))
  
  gilt.holdings.and.cashflows$Couponrate<-as.numeric(substr(regmatches(gilt.holdings.and.cashflows$Bond, regexpr("_(?:\\d*\\.)?\\d+_",gilt.holdings.and.cashflows$Bond)),start = 2,stop = nchar(regmatches(gilt.holdings.and.cashflows$Bond, regexpr("_(?:\\d*\\.)?\\d+_",gilt.holdings.and.cashflows$Bond)))-1))
  gilt.holdings.and.cashflows$Redemptionpayment<-gilt.holdings.and.cashflows$Totalpurchasesnominalmn
  gilt.holdings.and.cashflows$Totalpurchasesproceedsmn<-unlist(lapply(gilt.holdings.and.cashflows$Bond,function(x) sum(purchase.history[purchase.history$Bond %in% x,"Totalallocationproceedsmn"])))
  
  gilt.holdings.and.cashflows$Januarycoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((1-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Februarycoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((2-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Marchcoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((3-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Aprilcoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((4-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Maycoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((5-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Junecoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((6-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Julycoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((7-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Augustcoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((8-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Septembercoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((9-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Octobercoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((10-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Novembercoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((11-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  gilt.holdings.and.cashflows$Decembercoupon <- unlist(apply(gilt.holdings.and.cashflows,1,function(x) if(((12-month(x["Maturitydate"])) %% 6)==0){as.numeric(x["Couponrate"])*as.numeric(x["Redemptionpayment"])/200}else{0}))
  
  gilt.holdings.and.cashflows
}
GiltHoldingsAndCashflows <- read_excel("data/apfcashtransfersupdatefeb15.xlsx", sheet = "Gilt holdings and cashflows",skip = 1)
GiltHoldingsAndCashflows <- transform_GiltHoldingsAndCashflows(GiltHoldingsAndCashflows,PurchaseHistory)

calculate_MonthlyCashflows <- function(dates,gilt.holdings.and.cashflows){
  monthly.cashflows<-data.frame(dates)
  names(monthly.cashflows)<-"Date"
  
  i<-1
  while(i<=nrow(monthly.cashflows)){
    monthly.cashflows[i,"Coupons"]<-sum(gilt.holdings.and.cashflows[gilt.holdings.and.cashflows$Maturitymonth>=monthly.cashflows[i,"Date"],8+month(monthly.cashflows[i,"Date"])])
    temp<-gilt.holdings.and.cashflows[gilt.holdings.and.cashflows$Maturitymonth==monthly.cashflows[i,"Date"],"Redemptionpayment"]
    monthly.cashflows[i,"Redemptions"]<-if(length(temp)<1){0}else{temp}
    monthly.cashflows[i,"Total"]<-monthly.cashflows[i,"Coupons"]+monthly.cashflows[i,"Redemptions"]
    i<-i+1
  }
  rm(temp,i)
  return(monthly.cashflows)
}
MonthlyCashflows<-calculate_MonthlyCashflows(dates,GiltHoldingsAndCashflows)

cashflow.dates <- MonthlyCashflows[MonthlyCashflows$Total>0,"Date"]

calculate_YearsAhead <- function(month,cashflow.date){
  as.numeric(as.Date(cashflow.date)-as.Date(month))/365
}
YearsAhead <- lapply(month.dates,function(x) calculate_YearsAhead(x,cashflow.dates))
YearsAhead <- matrix(unlist(YearsAhead), ncol = length(YearsAhead[[1]]), byrow = TRUE, dimnames = list(as.character(month.dates),as.character(cashflow.dates)))
YearsAhead[YearsAhead<0]<-0

calculate_Yieldcurve <- function(expected.rate,premium){
  expected.rate+premium
}

calculate_ExpectedRate <- function(m.date,cf.date,interest.rates,years.ahead){
  month.BankRatediscountfactor <- as.numeric(lapply(m.date,function(x) interest.rates[as.Date(interest.rates$Date) == x,"BankRatediscountfactor"]))
  cashflow.BankRatediscountfactor <- as.numeric(lapply(cf.date,function(x) interest.rates[as.Date(interest.rates$Date) == x,"BankRatediscountfactor"]))
  
  cashflow.BankRatediscountfactor[17]<-1.  # manually set 27-2-2015 to prevent subsequent date matching errors
  
  temp <- lapply(month.BankRatediscountfactor,function(x) x/cashflow.BankRatediscountfactor)
  temp <- matrix(unlist(temp), ncol = length(temp[[1]]), byrow = TRUE, dimnames = list(as.character(m.date),as.character(cf.date)))
  
  temp.2 <- (1/years.ahead)
  temp.2[is.infinite(temp.2)]<-NA
  
  ((temp)^(temp.2)-1)*100
}

calculate_BalanceSheet <- function(balance.sheet,gilt.holdings.and.cashflows,interest.rates,pdv.of.cashflows){
  balance.sheet <- balance.sheet[-which(!as.Date(balance.sheet$Month)<as.Date("2069-04-01")),seq(1,21,by=1)]  # remove unneeded columns & rows (first two rows, last 12 rows (including empty ones, and extras not needed))
  names(balance.sheet) <- unlist(lapply(names(balance.sheet),function(x) removePunctuation(gsub(" ","",x))))
  
  temp <- ceiling(month(balance.sheet$Month)/3-1)  # Calculate which quarter they belong to 
  temp[temp==0] <- 4  # set zero indexed months to fourth quarter
  balance.sheet$Quarter <- temp
  
  balance.sheet$BankRate <- interest.rates[1:nrow(balance.sheet),"ForwardBankRate"]
  balance.sheet$Monthlyinterestfactor <- (1+(balance.sheet$BankRate/100))^(day(balance.sheet$Month)/as.numeric(as.Date(paste(year(balance.sheet$Month)+1,"-01-01",sep=""))-as.Date(paste(year(balance.sheet$Month),"-01-01",sep=""))))-1
  balance.sheet[3:nrow(balance.sheet),"Valueofinitialgiltholdings"] <- rowSums(pdv.of.cashflows)
  
  balance.sheet[is.na(balance.sheet)] <- 0.
  
  i<-3
  while(i<=nrow(balance.sheet)){
    if(i==3){  # Initialise required variables
      balance.sheet[i,"Currentgiltholdingsinitialgiltholdings"]<-1
      balance.sheet[i,"Loan"] <- -375200
      balance.sheet[i,"Cash"] <- 1185
      balance.sheet[i,"Currentgiltholdings"] <- balance.sheet[i,"Currentgiltholdingsinitialgiltholdings"] * balance.sheet[i,"Valueofinitialgiltholdings"]
      balance.sheet[i,"Dueunderindemnity"] <- -1 * (balance.sheet[i,"Currentgiltholdings"] + balance.sheet[i,"Loan"] + balance.sheet[i,"Cash"])
      
      i<-i+1
    }
    
    if(balance.sheet[i-1,"Valueofinitialgiltholdings"]==0){
      balance.sheet[i:nrow(balance.sheet),5:21]<-0
      break
    }
    
    balance.sheet[i,"Interestoncash"] <- if(balance.sheet[i-1,"Currentgiltholdingsinitialgiltholdings"]>0){balance.sheet[i-1,"Monthlyinterestfactor"] * balance.sheet[i-1,"Cash"]}else{0}
    
    #     # does not use initial transfers to HMT addition, see Balance sheet original s/sheet for details
    balance.sheet[i,"OngoingtransferstoHMT"] <- if((month(balance.sheet[i,"Month"]) %% 3) == 1){-1 * (balance.sheet[i-1,"Cash"] + balance.sheet[i-1,"Deductionsfromnexttransfer"] + balance.sheet[i-1,"Anticipatedexpensesforthisquarter"])}else{0}
    
    balance.sheet[i,"Salesincome"] <- if(as.Date(balance.sheet[i,"Month"]) >= input$FirstSale){min(input$PaceOfSales*1000/3,balance.sheet[i-1,"Currentgiltholdings"])}else{0}
    
    temp.5 <- gilt.holdings.and.cashflows[which(as.Date(balance.sheet[i,"Month"]) == gilt.holdings.and.cashflows$Maturitymonth),"Totalpurchasesproceedsmn"]
    temp.5 <- if(identical(temp.5,numeric(0))){0}else{temp.5}
    
    balance.sheet[i,"Reinvestmentexpenditure"] <- if(as.Date(balance.sheet[i,"Month"]) < input$FirstSale){-1*(balance.sheet[i-1,"Currentgiltholdingsinitialgiltholdings"] - balance.sheet[i,"Salesincome"]/balance.sheet[i-1,"Valueofinitialgiltholdings"]) * temp.5}else{0}
    balance.sheet[i,"Currentgiltholdingsinitialgiltholdings"] <- max(balance.sheet[i-1,"Currentgiltholdingsinitialgiltholdings"]-((balance.sheet[i,"Reinvestmentexpenditure"]+balance.sheet[i,"Salesincome"])/balance.sheet[i-1,"Valueofinitialgiltholdings"]),0)
    
    balance.sheet[i,"Currentgiltholdings"] <- balance.sheet[i,"Currentgiltholdingsinitialgiltholdings"] * balance.sheet[i,"Valueofinitialgiltholdings"]
    
    balance.sheet[i,"Interestonloan"] <- if(!(balance.sheet[i,"Quarter"] == balance.sheet[i-1,"Quarter"]) & (balance.sheet[i,"Currentgiltholdingsinitialgiltholdings"] == 0)){0}else{balance.sheet[i-1,"Monthlyinterestfactor"] * balance.sheet[i-1,"Loan"]}
    balance.sheet[i,"Couponincome"] <- balance.sheet[i-1,"Currentgiltholdingsinitialgiltholdings"] * sum(gilt.holdings.and.cashflows[gilt.holdings.and.cashflows$Maturitymonth>as.Date(balance.sheet[i-1,"Month"]),8+month(balance.sheet[i,"Month"])])
    
    temp.0 <- gilt.holdings.and.cashflows[which(as.Date(balance.sheet[i,"Month"]) == gilt.holdings.and.cashflows$Maturitymonth),"Redemptionpayment"]
    temp.0 <- if(identical(temp.0,numeric(0))){0}else{temp.0}
    
    balance.sheet[i,"Redemptionincome"] <- (balance.sheet[i-1,"Currentgiltholdingsinitialgiltholdings"] - balance.sheet[i,"Salesincome"]/balance.sheet[i-1,"Valueofinitialgiltholdings"]) * temp.0
    
    temp.6 <- gilt.holdings.and.cashflows[which(as.Date(balance.sheet[i,"Month"]) == gilt.holdings.and.cashflows$Maturitymonth),"Totalpurchasesproceedsmn"]
    temp.6 <- if(identical(temp.6,numeric(0))){0}else{temp.6}
    
    temp.1 <- if(as.Date(balance.sheet[i,"Month"]) >= input$FirstSale){(balance.sheet[i-1,"Currentgiltholdingsinitialgiltholdings"] - (balance.sheet[i,"Salesincome"]/balance.sheet[i-1,"Valueofinitialgiltholdings"])) * temp.6}else{0}
    temp.2 <- if(balance.sheet[i-1,"Currentgiltholdings"] == 0){0}else{-1*(balance.sheet[i-1,"Loan"]-balance.sheet[i-1,"Deductionsfromnexttransfer"])*(balance.sheet[i,"Salesincome"]/(balance.sheet[i,"Salesincome"]+balance.sheet[i,"Currentgiltholdings"]+balance.sheet[i,"Redemptionincome"]))}
    
    temp.3 <- if(balance.sheet[i,"Quarter"] == 1){balance.sheet[i-1,"Deductionsfromnexttransfer"]}else{0}
    temp.4 <- balance.sheet[i-1,"Cash"] + sum(balance.sheet[i,11:18]) + balance.sheet[i-1,"Anticipatedexpensesforthisquarter"]
    
    balance.sheet[i,"Loanrepayment"] <- min(-min(temp.1 + temp.2 - temp.3, temp.4),0)
    balance.sheet[i,"Loan"] <- balance.sheet[i-1,"Loan"] - balance.sheet[i,"Loanrepayment"]
    balance.sheet[i,"Cash"] <- balance.sheet[i-1,"Cash"] + sum(balance.sheet[i,11:19])
    
    balance.sheet[i,"Deductionsfromnexttransfer"] <- balance.sheet[i-1,"Deductionsfromnexttransfer"] + balance.sheet[i,"Loan"] - balance.sheet[i-1,"Loan"] - (temp.1 + temp.2)
    
    balance.sheet[i,"Dueunderindemnity"] <- -1 * (balance.sheet[i,"Currentgiltholdings"] + balance.sheet[i,"Loan"] + balance.sheet[i,"Cash"])
    
    i<-i+1
  }
  
  
  i<-4
  while(i<nrow(balance.sheet)){
    # needs to be calculated in separate loop, as relies on future values of previous variables
    balance.sheet[i,"Anticipatedexpensesforthisquarter"] <- if(!(balance.sheet[i,"Quarter"] == balance.sheet[i+1,"Quarter"]) & (balance.sheet[i,"Currentgiltholdingsinitialgiltholdings"] == 0)){0}else{
      if(!(balance.sheet[i,"Quarter"] == balance.sheet[i+1,"Quarter"])){
        balance.sheet[i,"Monthlyinterestfactor"]*3*balance.sheet[i,"Loan"]-min(sum(balance.sheet[i+1,c("Redemptionincome","Reinvestmentexpenditure")]),0)+min((sum(balance.sheet[i+2,c("Redemptionincome","Reinvestmentexpenditure")]+max(sum(balance.sheet[i+1,c("Redemptionincome","Reinvestmentexpenditure")]),0))),0)+min((sum(balance.sheet[i+3,c("Redemptionincome","Reinvestmentexpenditure")])+max((sum(balance.sheet[i+2,c("Redemptionincome","Reinvestmentexpenditure")])+max(sum(balance.sheet[i+1,c("Redemptionincome","Reinvestmentexpenditure")]),0)))),0)
      }else{
        if(!(balance.sheet[i-1,"Quarter"] == balance.sheet[i+1,"Quarter"])){
          balance.sheet[i,"Monthlyinterestfactor"] * 2 * balance.sheet[i,"Loan"]-min(sum(balance.sheet[i+1,c("Redemptionincome","Reinvestmentexpenditure")]),0)+min((sum(balance.sheet[i+2,c("Redemptionincome","Reinvestmentexpenditure")]+max(sum(balance.sheet[i+1,c("Redemptionincome","Reinvestmentexpenditure")]),0))),0)
        }else{
          balance.sheet[i,"Monthlyinterestfactor"] * balance.sheet[i,"Loan"] - min(sum(balance.sheet[i+1,c("Redemptionincome","Reinvestmentexpenditure")]),0)
        }
      }
    }
    i<-i+1
  }
  
  return(balance.sheet)
}
BalanceSheet.original <- read_excel("data/apfcashtransfersupdatefeb15.xlsx", sheet = "Balance sheet",skip = 1)


shinyServer(function(input, output) {

  InterestRates <- reactive({
    ModelledRates <- transform_ModelledRates(ModelledRates.original,input$FirstRateRise,input$TargetRate,input$PaceOfRateRises)
    InterestRates <- transform_InterestRates(InterestRates.original,MarketRates,ModelledRates,input$UseMarketPath)
    InterestRates
  })

  ExpectedRate <- reactive({
    ExpectedRate <- calculate_ExpectedRate(month.dates,cashflow.dates,InterestRates(),YearsAhead)
    ExpectedRate[!(ExpectedRate>0)]<-NA  # set all negative values to NA
    ExpectedRate[is.infinite(ExpectedRate)]<-0  # manually set inf on the 27-2-2015 to zero
    ExpectedRate
  })
  
  Premium <- reactive({
    Premium <- pmin(YearsAhead/10,1)*(input$YieldImpact/100)
    Premium[which(!(rownames(Premium)>=input$FirstSale)),]<-0
    # TODO: v.slow, need to find a method that avoids apply
    temp <- apply(YearsAhead,c(1,2),function(x) InterestRates()[max(which(InterestRates()$NA_<=x)),"Startingtermpremium"])
    temp[is.na(temp)]<-0  # strictly required?
    Premium <- temp + Premium
    Premium
  })

  YieldCurve <- reactive({
    calculate_Yieldcurve(ExpectedRate(),Premium())
  })
  
  PDVofcashflows <- reactive({
    temp <- as.numeric(lapply(cashflow.dates,function(x) MonthlyCashflows[MonthlyCashflows$Date==as.Date(x),"Total"]))
    temp <- matrix(temp,nrow=650,ncol=261,byrow = T) # force in to a matrix for multiplication
    PDVofcashflows <- (1+YieldCurve()/100)^(-YearsAhead) * temp
    PDVofcashflows[is.na(YieldCurve())]<-0
    PDVofcashflows
  })
  
  BalanceSheet <- reactive({
    BalanceSheet <- calculate_BalanceSheet(BalanceSheet.original,GiltHoldingsAndCashflows,InterestRates(),PDVofcashflows())
    BalanceSheet
  })
  
  output$plot1 <- renderPlot({
    p <- ggplot()
    p <- p + geom_line(data = MarketRates[1:220,], aes(Date,ImpliedBankRate), colour = "red")
    p <- p + geom_line(data = InterestRates()[1:220,], aes(Date,ForwardBankRate), colour='blue')
    p <- p + geom_vline(xintercept = as.numeric(as.POSIXct(input$FirstSale)), linetype=4)
    p <- p + geom_vline(xintercept = as.numeric(as.POSIXct(input$FirstRateRise)), linetype=3)
    p
  })
  
  negative_balance <- reactive({
    negative_bal <- BalanceSheet()[1:150,c("Month","Loan","Dueunderindemnity")]
    reshape2::melt(negative_bal, id.vars = c("Month"), measure.vars = c("Loan","Dueunderindemnity"))
  })
  
  positive_balance <- reactive({
    positive_bal <- BalanceSheet()[1:150,c("Month","Cash","Currentgiltholdings")]
    long_positive_bal <- reshape2::melt(positive_bal, id.vars = c("Month"), measure.vars = c("Cash","Currentgiltholdings"))
    
    long_positive_bal$variable <- reorder(long_positive_bal$variable,-long_positive_bal$value)
    factor(long_positive_bal$variable, levels=rev(levels(long_positive_bal$variable)))
  })
  
  output$plot2 <- renderPlot({
    p <- ggplot()
    p <- p + geom_bar(data = positive_balance(), aes(x = "Month", y = "value", fill = "variable"), stat="identity")
    #p <- p + geom_bar(data = negative_balance(), aes(x = "Month", y = "value", fill = "variable"), stat="identity")
    #p <- p + geom_vline(xintercept = as.numeric(as.POSIXct(input$FirstSale)), linetype = 4)
    #p <- p + geom_vline(xintercept = as.numeric(as.POSIXct(input$FirstRateRise)), linetype = 3)
    p
  })
  
  output$plot3 <- renderPlot({
    OngoingtransferstoHMT = BalanceSheet()[BalanceSheet()[,"OngoingtransferstoHMT"]>0,c("Month","OngoingtransferstoHMT")]
    TransferstoHMT = BalanceSheet()[BalanceSheet()[,"OngoingtransferstoHMT"]<0,c("Month","OngoingtransferstoHMT")]
    
    p <- ggplot()
    p <- p + geom_bar(data = OngoingtransferstoHMT, aes(x = "Month", y = -"OngoingtransferstoHMT"), stat="identity", fill="#009E73")
    p <- p + geom_bar(data = TransferstoHMT, aes(x = "Month", y = -"OngoingtransferstoHMT"), stat="identity", fill="#E69F00")
    p
  })
  
  output$plot4 <- renderPlot({
    OngoingtransferstoHMT = BalanceSheet()[BalanceSheet()[,"OngoingtransferstoHMT"]>0,c("Month","OngoingtransferstoHMT")]
    TransferstoHMT = BalanceSheet()[BalanceSheet()[,"OngoingtransferstoHMT"]<0,c("Month","OngoingtransferstoHMT")]
    
    cumsum_OngoingtransferstoHMT = data.frame(OngoingtransferstoHMT$Month,cumsum(OngoingtransferstoHMT$OngoingtransferstoHMT))
    cumsum_TransferstoHMT = data.frame(TransferstoHMT$Month,cumsum(TransferstoHMT$OngoingtransferstoHMT))
    
    names(cumsum_OngoingtransferstoHMT) <- c("Month","OngoingtransferstoHMT")
    names(cumsum_TransferstoHMT) <- c("Month","TransferstoHMT")
    
    p <- ggplot()
    p <- p + geom_bar(data = cumsum_OngoingtransferstoHMT, aes_string(Month,-OngoingtransferstoHMT), stat="identity", fill="#009E73")
    p <- p + geom_bar(data = cumsum_TransferstoHMT, aes_string(Month,-TransferstoHMT), stat="identity", fill="#E69F00")
    p
  })
  
})

