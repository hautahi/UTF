# This program downloads the UI trust and files from the treasury website:
# https://www.treasurydirect.gov/govt/reports/tfmp/tfmp_utf.htm
# and combines them into csv files

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