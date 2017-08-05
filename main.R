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
dtrans <- read.csv("./data/ny_transactions.csv",stringsAsFactors = F) %>% mutate(date=as.Date(date1,"%m/%d/%Y"))
dbal <- read.csv("./data/ny_balances.csv",stringsAsFactors = F) %>% mutate(date=as.Date(date1,"%m/%d/%Y"))

# Create deposits
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(trans>0) %>% group_by(year,month) %>%
  summarise(dep=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="positive") %>% ungroup() %>% select(dep,date)

# Create state deposits
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(type=="STATE DEPOSITS") %>% group_by(year,month) %>%
  summarise(state_dep=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="negative") %>% ungroup() %>% select(state_dep,date) %>%
  left_join(d,by="date")

# Create Withdrawals
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(trans<0) %>% group_by(year,month) %>%
  summarise(withd=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="negative") %>% ungroup() %>% select(withd,date,month,year) %>%
  left_join(d,by="date")

# Create state withdrawals
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(type=="STATE UI WITHDRAW") %>% group_by(year,month) %>%
  summarise(state_withd=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="negative") %>% ungroup() %>% select(state_withd,date) %>%
  left_join(d,by="date")

# Create other transactions
d <- dtrans %>% mutate(month = format(date, "%m"), year = format(date, "%Y")) %>% filter(type!="STATE UI WITHDRAW",type!="STATE DEPOSITS") %>% group_by(year,month) %>%
  summarise(other=sum(trans)) %>% mutate(date=as.yearmon(paste(year, month, sep = "-")),type="negative") %>% ungroup() %>% select(other,date) %>%
  left_join(d,by="date")

# Merge with balances (shifted back one month to make them closing balances)
d <- dbal %>% mutate(date=date-1/12,month = format(date, "%m"), year = format(date, "%Y")) %>% select(-date) %>%
  left_join(d,by=c("month","year"))

# --------------------------------------
# Create Annual Dataset
# --------------------------------------

# Create deposits
da <- dtrans %>% mutate(year = format(date, "%Y")) %>% filter(trans>0) %>% group_by(year) %>%
  summarise(dep=sum(trans)) %>% ungroup() %>% select(dep,year)

# Create state deposits
da <- dtrans %>% mutate( year = format(date, "%Y")) %>% filter(type=="STATE DEPOSITS") %>% group_by(year) %>%
  summarise(state_dep=sum(trans)) %>% ungroup() %>% select(state_dep,year) %>%
  left_join(da,by="year")

# Create Withdrawals
da <- dtrans %>% mutate(year = format(date, "%Y")) %>% filter(trans<0) %>% group_by(year) %>%
  summarise(withd=sum(trans)) %>% ungroup() %>% select(withd,year) %>%
  left_join(da,by="year")

# Create state withdrawals
da <- dtrans %>% mutate( year = format(date, "%Y")) %>% filter(type=="STATE UI WITHDRAW"|type=="STATE UI WITHDRAWAL") %>% group_by(year) %>%
  summarise(state_withd=sum(trans)) %>% ungroup() %>% select(state_withd,year) %>%
  left_join(da,by="year")

# Create other transactions
da <- dtrans %>% mutate( year = format(date, "%Y")) %>% filter(type!="STATE UI WITHDRAW",type!="STATE UI WITHDRAWAL",type!="STATE DEPOSITS") %>%
  group_by(year) %>% summarise(other=sum(trans)) %>% ungroup() %>% select(other,year) %>%
  left_join(da,by="year")

# Merge with balances (shifted back one month to make them closing balances)
da <- dbal %>% mutate(date=date-1/12,month = format(date, "%m"), year = format(date, "%Y")) %>% filter(month==12) %>%
  select(bal,year) %>% left_join(da,by="year")

# --------------------------------------
# Plot
# --------------------------------------

dfactor=1000000

d %>% filter(date > "2007-01-01" & date <"2011-01-01") %>%
  ggplot() + 
  geom_bar(stat="identity",aes(x = date, y = bal/dfactor),alpha=0.75) +
  geom_line(aes(x = date, y = dep/dfactor), color = "blue",size=1.2) +
  geom_line(aes(x = date, y = -withd/dfactor), color = "red",size=1.2) +
  xlab('') +
  ylab('percent.change') + theme_bw() +
  theme(legend.title=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())

da %>%
  ggplot() + 
  geom_bar(stat="identity",aes(x = year, y = bal/dfactor),alpha=0.75) +
  geom_line(aes(x = year, y = state_dep/dfactor,group=1), color = "blue",size=1.2) +
  geom_line(aes(x = year, y = -state_withd/dfactor,group=1), color = "red",size=1.2) +
  geom_line(aes(x = year, y = other/dfactor,group=1), color = "black",size=1.2) +
  xlab('') +
  ylab('percent.change') + theme_bw() +
  theme(legend.title=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",
        #panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.border = element_blank())
