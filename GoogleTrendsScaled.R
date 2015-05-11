######### Using GTrendsR only for more control
library(GTrendsR)
library(RCurl)
library(rjson)
library(reshape2) #For melt
library(plyr) #For ldply
library(dplyr)

initTrend <- function()
{
  ch <- GTrendsR::gconnect("Your Gogole email", "Your Google Password")
  authenticatePage2 <- getURL("http://www.google.com", curl = ch)

  return(ch)
}

getTrend <- function(term, baseline = '',
                     date = 'today 6-m',
                     geo = '', #Leave empty for world
                     params=list(),
                     scale = TRUE)
{
  query <- paste(baseline, term, sep=',')
  pp <- c(list(q = query,
               date = date,
               cat = "0-18", #0-18 = Shopping
               geo = geo,
               #content = 1,
               cid="TIMESERIES_GRAPH_0",
               # cmpt='q',
               #tz='',
               #graph = 'all_csv',
               export = 3 #Data only
  ),
  params)

  trendsURL <- "http://www.google.com/trends/fetchComponent"

  resultsText <- getForm(trendsURL, .params = pp, curl = ch)
  ## Sometimes we reach quota limit, in that case stop!
  if (any(grep("QUOTA", resultsText))) {
    print("Reached Google Trends quota limit! Please try again later.")
  }

  vec <- strsplit(resultsText, "\\\n{2,}")[[1]]
  headers <- unname(sapply(vec, function(v) strsplit(v, "\\\n")[[1]][1]))

  json <- sub('^.*google\\.visualization\\.Query\\.setResponse\\(', '', resultsText)
  json <- sub(');$', '', json)

  json <- gsub('new Date\\(([0-9,]+)\\)', '[\\1]', json)
  json <- gsub(',,', ',{"v":0,"f":"0"},', json)

  rawtable <- fromJSON(json)$table
  cols <- melt(rawtable$cols)
  datacols <- as.character(cols[cols[[2]] == 'label' & cols$value != 'Date',]$value)

  trend <- ldply(rawtable$rows,
                 function(r)
                 {
                   vals <- unlist(llply(r$c, function(col) col$v))

                   #Pad with trailing zeroes if data is missing
                   return(c(vals, rep(0, 3 + length(datacols) - length(vals)))) #3 - for y,m,d
                 }
  )

  colnames(trend) <- c('year', 'month', 'day', datacols)
  trend$month <- trend$month + 1 #JavaScript starts month numbers from 0

  #If baseline is provided return ratio from baseline
  if (baseline != '' & scale)
    for ( attr in datacols[-1])
      trend[[attr]] <- trend[[attr]] / trend[, 3+1]

  return(trend)
}

#Google-generated search categories
terms <- c(baseline="/m/0524b41" #Game of Thrones
           ,"/m/0gxrn7l" #The Newsroom
           ,"/m/0hr22w5" #Orange is the New Black
           ,"/m/0h3rv9x" #House of Cards
)

ch <- initTrend()
trend.baseline <- getTrend(terms['baseline'], date = 'today 24-m')
trends <- trend.baseline

scale <- FALSE
for (term in terms[-1])
{
  trend <- getTrend(term, terms['baseline'], date = trend.interval, scale=scale)
  term <- tail(colnames(trend), 1) #May be prettified by Google
  trends[[term]] <- trend[, term] * ifelse(scale, trend.baseline[, tail(colnames(trend.baseline), 1)], 1)
  Sys.sleep(sample(1:3,1))
}

trends %>% head

#Graph the trends
library(ggplot2)
trends %>%
  melt(id=c('year', 'month', 'day'),
       variable.name = "term", value.name = "trend") %>%
  mutate(week=as.Date(ISOdate(year, month, day))) %>%
  ggplot(aes(x=week, y=trend, color=term)) +
  geom_line() +
  theme(axis.text.x = element_text(angle = -45, hjust = 0))
