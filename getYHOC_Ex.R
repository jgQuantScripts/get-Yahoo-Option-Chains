require("jsonlite");require("plyr");require("data.table")

getYHOOOC = function(symbol,x)
{
  url <- paste0('https://query2.finance.yahoo.com/v7/finance/options/',symbol,
                '?&date=',x)
  chain <- fromJSON(url)
  chain <- as.vector(chain)
  # CALLS
  CALLS <- as.data.frame(rbindlist(chain$optionChain$result[[1]]$options[[1]]$calls, use.names = TRUE,fill = TRUE))
  # FIX THE TIMESTAMPS
  CALLS$lastTradeDate <- as.Date(as.POSIXct(as.numeric(as.character(CALLS$lastTradeDate)),
                                            origin = "1970-01-01",tz="EST"))
  
  CALLS$expiration <- as.Date(as.POSIXct(as.numeric(as.character(CALLS$expiration)),
                                         origin = "1970-01-01",tz="EST"))
  # CALL/PUT
  NOMS <- c(names(CALLS),"type")
  CALLS <- cbind(CALLS,as.data.frame(rep("c",nrow(CALLS))))
  colnames(CALLS) <- NOMS
  # PUTS
  PUTS <- as.data.frame(rbindlist(chain$optionChain$result[[1]]$options[[1]]$puts,use.names = TRUE,fill=TRUE))
  # FIX THE TIMESTAMPS
  PUTS$lastTradeDate <- as.Date(as.POSIXct(as.numeric(as.character(PUTS$lastTradeDate)),
                                           origin = "1970-01-01",tz="EST"))
  
  PUTS$expiration <- as.Date(as.POSIXct(as.numeric(as.character(PUTS$expiration)),
                                        origin = "1970-01-01",tz="EST"))
  # CALL/PUT
  NOMS <- c(names(PUTS),"type")
  PUTS <- cbind(PUTS,as.data.frame(rep("p",nrow(PUTS))))
  colnames(PUTS) <- NOMS
  # CALLS/PUTS COMBINED
  OC <- rbind(CALLS,PUTS)
  OC
}
getOC = function(x)
{
  symbol <- as.character(x)
  # TEST 
  url <- paste0('https://query2.finance.yahoo.com/v7/finance/options/',symbol,
                '?&date=1471564800')
  chain <- try(fromJSON(url))
  if(!inherits(chain,'try-error'))
  {
    # ALL EXPIRATIONS DATES
    chain <- as.vector(chain)
    EXP <- chain$optionChain$result[[1]]$expirationDates
    # apply a function 
    all <- lapply(as.list(EXP), function(x){
      tmp <- try(getYHOOOC(symbol=symbol, x))
      if(!inherits(tmp,'try-error'))
        tmp
    })
    # COMBINE ALL EXPIRATIONS
    ALL <- rbindlist(all,use.names = TRUE, fill = TRUE)
    # GET ALL THE METADATA
    LIST1 <- chain$optionChain$result[[1]]$quote
    METADATA <- as.data.frame(do.call(cbind,LIST1))
    # WANT <- names(METADATA)
    WANT <- c("regularMarketChangePercent","regularMarketPreviousClose","bid","ask",
              "bidSize","askSize","averageDailyVolume3Month","averageDailyVolume10Day",
              "fiftyTwoWeekLowChange","fiftyTwoWeekLowChangePercent","fiftyTwoWeekHighChange",
              "fiftyTwoWeekHighChangePercent","fiftyTwoWeekLow","fiftyTwoWeekHigh",
              "dividendDate","earningsTimestamp","trailingAnnualDividendRate","trailingPE",
              "epsTrailingTwelveMonths","epsForward","sharesOutstanding",
              "twoHundredDayAverageChangePercent","bookValue","fiftyDayAverage",
              "fiftyDayAverageChange","fiftyDayAverageChangePercent","twoHundredDayAverage",
              "twoHundredDayAverageChange","marketCap","forwardPE","priceToBook",
              "preMarketChange","preMarketChangePercent","preMarketTime","preMarketPrice",
              "regularMarketPrice","regularMarketTime","regularMarketChange",
              "regularMarketOpen","regularMarketDayHigh","regularMarketDayLow",
              "regularMarketVolume","symbol"
    )
    METADATA <- METADATA[,names(METADATA) %in% WANT]
    ALL <- rbind.fill(cbind(ALL,METADATA))
    ALL
  }
}

OC <- getOC("FB")