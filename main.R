# This program downloads the UI trust und files from the treasury website:
# https://www.treasurydirect.gov/govt/reports/tfmp/tfmp_utf.htm

library(stringr)
library(ggplot2)
library(dplyr)
library(zoo)

# --------------------------------------
# Download files
# --------------------------------------

baseurl1 ="https://www.treasurydirect.gov/govt/reports/tfmp/utf/"
baseurl2 = "/dfiw0"
mfiles = c("01","02","03","04","05","06","07","08","09","10","11","12")
yfiles = c("05","06","07","08","09","10","11","12","13","14","15","16","17")
sfiles = c("al","ny")

files=c()
for (y in yfiles){
  for (m in mfiles){files=c(files,paste0(m,y))}
}

# Remove future months
files = head(files,-7)

for (s in sfiles){
  for (x in files){
      name = paste0(baseurl1,s,baseurl2,x,"as",s,".txt")
      destfile=paste0("./download_files/",s,x,".txt")
      download.file(name, destfile, quiet = FALSE, mode = "w", cacheOK = TRUE)
  }
}

# --------------------------------------
# Combine files into master dataframe
# --------------------------------------

for (s in sfiles) {

  files <- list.files(path="./download_files", pattern=paste0(s,"*"), full.names=T, recursive=FALSE)
  
  opbal = c()
  dates = c()
  trans = c()
  type=c()
  dt=c()
  
  for (f in files){
    # read txt file
    d = readLines(f)
    
    # remove surplus lines and whitespace
    d = d[7:length(d)]
    d=gsub("\\s+", " ", str_trim(d))
    
    # extract opening balance
    opbal = c(opbal,word(d[1],2,2))
    
    d = d[-1]
    d = head(d,-2)
    
    # Extract dates and transactions
    dates=c(dates,word(d,1,1))
    type=c(type,word(d,4,-3))
    trans=c(trans,word(d,2,2))
    
    dt = c(dt,paste0(str_sub(f,-8,-7),"/01/20",str_sub(f,-6,-5)))
    
  }
  
  df = data.frame(date1=dates,date=as.Date(dates,"%m/%d/%Y"),type=type,trans=as.numeric(gsub(",","",trans)))
  dbal = data.frame(date1=dt,date=as.Date(dt,"%m/%d/%Y"),bal=as.numeric(gsub(",","",opbal)))
  
  # Save dataframes
  write.csv(df,paste0("./data/",s,"_transactions.csv"),row.names = F)
  write.csv(arrange(dbal,date),paste0("./data/",s,"_balances.csv"),row.names = F)
  
}

# --------------------------------------
# Create Monthly Dataset
# --------------------------------------

# Read in data

get_graph=function(s){

transname = paste0("./data/",s,"_transactions.csv")
balname = paste0("./data/",s,"_balances.csv")

dtrans <- read.csv(transname,stringsAsFactors = F) %>% mutate(date=as.Date(date1,"%m/%d/%Y"))
dbal <- read.csv(balname,stringsAsFactors = F) %>% mutate(date=as.Date(date1,"%m/%d/%Y"))

# Create deposits
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(trans>0) %>% group_by(year,month) %>%
  summarise(dep=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="positive") %>% ungroup() %>% select(dep,date)

# Create state deposits
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(type=="STATE DEPOSITS") %>% group_by(year,month) %>%
  summarise(state_dep=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="negative") %>% ungroup() %>% select(state_dep,date) %>%
  left_join(d,.,by="date")

# Create Withdrawals
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(trans<0) %>% group_by(year,month) %>%
  summarise(withd=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="negative") %>% ungroup() %>% select(withd,date,month,year) %>%
  left_join(d,.,by="date")

# Create state withdrawals
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(type=="STATE UI WITHDRAW") %>% group_by(year,month) %>%
  summarise(state_withd=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="negative") %>% ungroup() %>% select(state_withd,date) %>%
  left_join(d,.,by="date")

# Create other transactions
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(type!="STATE UI WITHDRAW"&type!="STATE DEPOSITS") %>% group_by(year,month) %>%
  summarise(other=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="other") %>% ungroup() %>% select(other,date) %>%
  left_join(d,.,by="date")

# Merge with balances (shifted back one month to make them closing balances)
d <- dbal %>% mutate(date=date-1/12,month = format(date, "%m"), year = format(date, "%Y")) %>% select(-date) %>%
  left_join(d,by=c("month","year")) %>% mutate(date=as.yearmon(date))

# --------------------------------------
# Plot
# --------------------------------------

themes <- theme_bw() +
  theme(legend.title=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

dfactor=1000000

g1 <- d %>% filter(date > "2007-01-01" & date <"2017-05-05") %>%
  ggplot() + 
  geom_bar(stat="identity",aes(x = date, y = bal/dfactor),alpha=0.75) + themes+
  xlab('') +
  ylab('Trust Fund Balance ($m)')

g <- d %>% filter(date > "2007-01-01" & date <"2017-05-05") %>%
  ggplot() + 
  geom_bar(stat="identity",aes(x = date, y = bal/dfactor),alpha=0.75) +
  geom_line(aes(x = date, y = dep/dfactor), color = "blue",size=1.2) +
  geom_line(aes(x = date, y = -withd/dfactor), color = "red",size=1.2) +
  xlab('') +
  ylab('Trust Fund Balance ($m)') + themes

return(list(g1,g))

}

ny_graphs=get_graph("ny")
al_graphs=get_graph("al")
