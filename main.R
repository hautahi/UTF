# This program analyzes the downloaded UTF data

library(stringr)
library(ggplot2)
library(dplyr)
library(zoo)

# --------------------------------------
# Create Monthly Dataset and Plot Graph Function
# --------------------------------------

get_graph=function(s){

transname = paste0("./data/",s,"_transactions.csv")
balname = paste0("./data/",s,"_balances.csv")

# Read in data
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

# Create Plot Theme
themes <- theme_bw() +
  theme(legend.title=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank())

# Create Plots
dfactor=1000000

g1 <- d %>% filter(date > "2007-05-01" & date <="2017-05-01") %>%
  mutate(date=as.Date(date)) %>%
  ggplot() + 
  geom_bar(stat="identity",aes(x = date, y = bal/dfactor),alpha=0.75) + themes+
  xlab('') +
  ylab('Trust Fund Balance ($m)')

g <- d %>% filter(date > "2014-05-01" & date <"2017-05-05") %>%
  ggplot() + 
  geom_bar(stat="identity",aes(x = date, y = bal/dfactor),alpha=0.75) +
  geom_line(aes(x = date, y = dep/dfactor), color = "blue",size=1.2) +
  geom_line(aes(x = date, y = -withd/dfactor), color = "red",size=1.2) +
  xlab('') +
  ylab('Trust Fund Balance ($m)') + themes

return(list(g1,g))

}

# --------------------------------------
# Plot Graphs
# --------------------------------------

ny_graphs=get_graph("ny")
al_graphs=get_graph("al")

g_ny <- ny_graphs[[1]] +ggtitle("New York")
g_al <- al_graphs[[1]] +ggtitle("Alabama")
multiplot(g_ny, g_al, cols=1)

g_ny <- ny_graphs[[2]] +ggtitle("New York")
g_al <- al_graphs[[2]] +ggtitle("Alabama")
multiplot(g_ny, g_al, cols=1)

# --------------------------------------
# Plot DOL-ETA data
# --------------------------------------

# Data from https://oui.doleta.gov/unemploy/DataDownloads.asp

odie <- read.csv("./data/394.csv",stringsAsFactors = F,skip = 4) %>%
  select(state=YR, year=ST,wage=c3,cont=c8,rat=c19,cont_per=c15,rr=c19,hcm=c22) %>% mutate(state=gsub(" ", "", state, fixed = TRUE))

# Create Plot Theme
themes <- theme_bw() +
  theme(legend.title=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="bottom",
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank())

ny=odie %>% filter(state=="NY",year>2006) %>%
  mutate(date=as.Date(year)) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cont_per), color = "blue",size=1.2)+
  #geom_line(aes(x = year, y = wage), color = "blue",size=1.2) +
  themes+
  xlab('') +
  ylab('% of Total Wages ')+
  ggtitle("New York")

al=odie %>% filter(state=="AL",year>2006) %>%
  mutate(date=as.Date(year)) %>%
  ggplot() + 
  geom_line(aes(x = year, y = cont_per), color = "blue",size=1.2)+
  #geom_line(aes(x = year, y = wage), color = "blue",size=1.2) +
  themes+
  xlab('') +
  ylab('% of Total Wages ')+
  ggtitle("Alabama")

multiplot(ny, al, cols=1)

# --------------------------------------
# Plot Solvency ratios
# --------------------------------------

rr=odie %>% filter(state %in% c("MA","AL","CA","TX"),year>1990) %>%
  mutate(date=as.Date(year)) %>%
  ggplot(aes(x=year)) + 
  geom_line(aes(y=rr,colour = factor(state)), size = 1.5)+
  theme_bw() +
  theme(legend.title=element_blank(),
        axis.ticks.x=element_blank(),
        legend.position="none",
        #panel.grid.major = element_blank(),
        #panel.grid.minor = element_blank(),
        panel.border = element_blank())+
  xlab('') +
  ylab('Reserve Ratio')

hcm=odie %>% filter(state %in% c("MA","AL","CA","TX"),year>1990) %>%
  mutate(date=as.Date(year)) %>%
  ggplot(aes(x=year)) + 
  geom_line(aes(y=hcm,colour = factor(state)), size = 1.5)+
  themes+
  xlab('') +
  ylab('High Cost Multiple')
plist=list(rr)

plist[[2]]=hcm
multiplot(plotlist=plist, cols=1)

# --------------------------------------
# Define Multiplot Function
# --------------------------------------

multiplot <- function(..., plotlist=NULL, file, cols=1, layout=NULL) {
  library(grid)
  
  # Make a list from the ... arguments and plotlist
  plots <- c(list(...), plotlist)
  
  numPlots = length(plots)
  
  # If layout is NULL, then use 'cols' to determine layout
  if (is.null(layout)) {
    # Make the panel
    # ncol: Number of columns of plots
    # nrow: Number of rows needed, calculated from # of cols
    layout <- matrix(seq(1, cols * ceiling(numPlots/cols)),
                     ncol = cols, nrow = ceiling(numPlots/cols))
  }
  
  if (numPlots==1) {
    print(plots[[1]])
    
  } else {
    # Set up the page
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(nrow(layout), ncol(layout))))
    
    # Make each plot, in the correct location
    for (i in 1:numPlots) {
      # Get the i,j matrix positions of the regions that contain this subplot
      matchidx <- as.data.frame(which(layout == i, arr.ind = TRUE))
      
      print(plots[[i]], vp = viewport(layout.pos.row = matchidx$row,
                                      layout.pos.col = matchidx$col))
    }
  }
}
