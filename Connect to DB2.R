library(ibmdbR)
host.name <- "sasm1p.jacksonnational.com"
port <-"8561" # 50000 if not using SSL or 50001 if using SSL
user.name <-"qqz0117"
pwd <- "ElsieLuna2!"
con.text <- paste("placeholderForYourDSNName;DRIVER=BLUDB",
                  ";Database=BLUDB",
                  ";Hostname=",host.name,
                  ";Port=",port,
                  ";PROTOCOL=TCPIP",
                  ";UID=", user.name,
                  ";PWD=",pwd,sep="")
# Connect to using a odbc Driver Connection string to a remote database
con <- idaConnect(con.text)