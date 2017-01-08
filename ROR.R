##################################################################################
###########################    Input Guide    ####################################
######   Country    : [Uganda    = 1, Swaziland = 2, Tanzania  = 3]         ######
##################################################################################
###########################    Input Below    ####################################

Country = 2

###########################################################
###########################################################
####   Below are Codes that don't need to be changed   ####
###########################################################
###########################################################
######################################################################################################################
library(plyr)
library(dplyr)
library(data.table)

if(Country == 1 | 2){
  Start.Date = "2016-08-27"
}

if(Country == 3){
  Start.Date = "2016-09-05"
}
End.Date   = Sys.Date()-5*7

country.list <- c("Uganda", "Swaziland", "Tanzania")
deadline.list <- c(10*7, 12*7, 10*7) 
deadline <- deadline.list[Country]
country <- country.list[Country]

##########################################################################      Master List     #############################################################################

## Set working directory and imoport files into list
setwd(paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Results/RoR_Log", sep = ""))
file.list <- dir()[grep("Master", dir())]
file.list <- file.list[!grepl("~|extended", file.list)]


###################### funtion to recognize month#############################
num.month <- c(1:12)                                        
chr.month <- month.abb[num.month]
CHR.month <- toupper(chr.month)

simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1,1)), substring(s, 2),
        sep="", collapse=" ")
}
###############################################################################

## generate list of files to be read (latest updated fies)
details = file.info(file.list)
details = details[with(details, order(as.POSIXct(mtime))), ]
files = rownames(details)
file.table <- data.frame(files, details, month = NA)
rownames(file.table) <- NULL

for (i in 1:12) {
  file.table[grep(CHR.month[i], file.table$files), 9] <- simpleCap(tolower(CHR.month[i])) %>% match(., month.abb)
}

file.table <- with(file.table, file.table[order(month, ctime, decreasing = TRUE),])

file.table <- file.table[with(file.table, !duplicated(month)) %>% which(), ]

file.final.list <- file.table[,1] %>% as.vector()

mlist.data.o <- lapply(file.final.list, function(X) {data.frame(id = basename(X), readxl::read_excel(X, sheet = 1))})

## Preparing data before appending
mlist.data <- list()

for (i in 1:length(file.final.list)) {
  mlist.data[[i]] <- mlist.data.o[[i]] %>% as.data.frame()
  colnames(mlist.data[[i]]) <- tolower(names(mlist.data[[i]]))
  id.col <- which(names(mlist.data[[i]])=="id")
  ptid.col <- which(names(mlist.data[[i]])=="ptid")
  
  if(Country == 1 | 3 ){
  guspec.col <- Reduce(`&`, lapply(c("guspec"), grepl, names(mlist.data[[i]]))) %>% which()
  }
  
  if(Country == 2 ){
  guspec.col <- which(names(mlist.data[[i]])=="guspec")
  }
  
  if(length(guspec.col)>1){
    guspec.col <- guspec.col[grep("correct", colnames(mlist.data[[i]])[guspec.col])]
  }
  
  
  a <- Reduce(`&`, lapply(c("sent", "facility"), grepl, names(mlist.data[[i]]))) %>% which()
  b <- Reduce(`&`, lapply(c("data", "collection", "date"), grepl, names(mlist.data[[i]]))) %>% which()
  c <- Reduce(`&`, lapply(c("test", "name"), grepl, names(mlist.data[[i]]))) %>% which()
  d <- Reduce(`&`, lapply(c("merged", "date"), grepl, names(mlist.data[[i]]))) %>% which()
  e <- Reduce(`&`, lapply(c("upload"), grepl, names(mlist.data[[i]]))) %>% which()
  f <- Reduce(`&`, lapply(c("pdf", "ready"), grepl, names(mlist.data[[i]]))) %>% which()
  variable.wantted <- c(a, b, c, d, e, f)
  mlist.data[[i]] <- mlist.data[[i]][, c(id.col, ptid.col, guspec.col, variable.wantted)]
  
  
  if(length(e)==0){
    mlist.data[[i]]$upload.date=NA
  }
  
  if(length(f)==0){
    mlist.data[[i]]$pdf.ready.date=NA
  }
  
  
  colnames(mlist.data[[i]]) <- c("id", "ptid", "guspec", "date.sent.to.facility", "data.collection.date", "test.name", "date.merged", "upload.date", "pdf.ready.date")
  
  
  #Dropping time from date
  mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., date.sent.to.facility = substr(mlist.data[[i]][,4], 1, 10))
  mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., data.collection.date = substr(mlist.data[[i]][,5], 1, 10))
  
  num.datind <- nchar(as.character(mlist.data[[i]][,4]))==5
  
  mlist.data[[i]][which(num.datind), 4] <- as.Date(as.numeric(mlist.data[[i]][which(num.datind), 4]), origin = "1899-12-30") %>% as.character()
  
  if(is.character(mlist.data[[i]][,7])){
    mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., date.merged = as.Date(mlist.data[[i]][,7], "%d%b%Y"))
  }
  
  if(is.numeric(mlist.data[[i]][,8])){
    mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., upload.date = as.Date(as.numeric(mlist.data[[i]][,8]), origin = "1899-12-30"))
  }else if(is.character(mlist.data[[i]][,8])){
    mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., upload.date = as.Date(NA))
  }
  
  if(is.numeric(mlist.data[[i]][,9])){
    mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., pdf.ready.date = as.Date(as.numeric(mlist.data[[i]][,9]), origin = "1899-12-30"))
  }else if(is.character(mlist.data[[i]][,9])){
    mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., pdf.ready.date = as.Date(NA))
  }
  
  mlist.data[[i]] <- mlist.data[[i]][!is.na(mlist.data[[i]][,2]),]
  
  mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., date.merged = substr(mlist.data[[i]][,7], 1, 10))
  mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., upload.date = substr(mlist.data[[i]][,8], 1, 10))
  mlist.data[[i]] <- mlist.data[[i]] %>% mutate(., pdf.ready.date = substr(mlist.data[[i]][,9], 1, 10))
  
}

## Append master lists
mlist <- do.call("rbind", mlist.data)

## Format appended master list
mlist <- mlist %>% select(., file.name.mlist = id, ptid, guspec, date.sent.to.facility, data.collection.date, test.name, date.merged, upload.date, pdf.ready.date)

mlist <- mlist[!is.na(mlist$ptid),]

mlist <- mlist %>% mutate(., GUSPEC.Index = substr(mlist$guspec, 1, 8))

##########################################################################        VL        #############################################################################
setwd(paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Results/Returning", sep = ""))
file.list <- dir()
file.list <- file.list[!grepl("~|EID|DBS|approved|QC|Thumbs|original|AlereData|xlsx|zip|TXT|ignore|MISSING|DVL", file.list)]

VL.data <- lapply(file.list, function(X) {data.frame(id = basename(X), read.csv(X)) %>% select(., file.name.vl = id, Sample.ID, Order.Date.Time, Result)})
VL <- do.call("rbind", VL.data)

VL1 <- VL[grep("FSQ", VL$Sample.ID), ]

VL2 <- VL[!grepl("FSQ|CDC", VL$Sample.ID), ]

VL1 <- VL1 %>% mutate(., Sample.ID = substr(VL1$Sample.ID, 4, 14))

VL1 <- VL1 %>% mutate(., GUSPEC.Index = substr(VL1$Sample.ID, 1, 8))

VL2 <- VL2 %>% mutate(., GUSPEC.Index = substr(VL2$Sample.ID, 1, 8))

VL1 <- VL1[!duplicated(VL1$Sample.ID),]

VL2 <- VL2[!duplicated(VL2$Sample.ID),]

VL <- rbind(VL1, VL2)

VL <- VL[!duplicated(VL$Sample.ID),]

##########################################################################      Approved     #############################################################################
setwd(paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Results/Returning", sep = ""))
file.list <- dir()[grep("approved", dir())]
file.list <- file.list[!grepl("~|EID|DBS|original|Instrument|instrument", file.list)]

approved.vl.data <- lapply(file.list, function(X) {data.frame(id = basename(X), readxl::read_excel(X, sheet = 1)) %>% select(., file.name.approved = id, Sample.ID)})

approved.vl <- do.call("rbind", approved.vl.data)


approved.vl1 <- approved.vl[grep("FSQ", approved.vl$Sample.ID), ]

approved.vl2 <- approved.vl[!grepl("FSQ|CDC", approved.vl$Sample.ID), ]

approved.vl1 <- approved.vl1 %>% mutate(., Sample.ID = substr(approved.vl1$Sample.ID, 4, 14))

approved.vl1 <- approved.vl1 %>% mutate(., GUSPEC.Index = substr(approved.vl1$Sample.ID, 1, 8))

approved.vl2 <- approved.vl2 %>% mutate(., GUSPEC.Index = substr(approved.vl2$Sample.ID, 1, 8))

approved.vl1 <- approved.vl1[!duplicated(approved.vl1$Sample.ID),]

approved.vl2 <- approved.vl2[!duplicated(approved.vl2$Sample.ID),]

approved.vl <- rbind(approved.vl1, approved.vl2)

approved.vl <- approved.vl[!duplicated(approved.vl$Sample.ID),]


##########################################################################        EID         #############################################################################
setwd(paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Results/Returning", sep = ""))
file.list <- dir()
file.list <- file.list[!grepl("~|VL|approved|QC|Thumbs|original|AlereData|xlsx|zip|WS|DBS|ignore|MISSING", file.list)]

EID.data <- lapply(file.list, function(X) {data.frame(id = basename(X), read.csv(X)) %>% select(., file.name.vl = id, Sample.ID, Accepted.Date.Time, Result)})


EID <- do.call("rbind", EID.data)

### For Uganda and Swaziland

if(Country == 1 | 2){

EID <- EID[!grepl("CDC", EID$Sample.ID),]

EID <- EID[as.character(EID$Sample.ID)!="",]

EID1 <- EID[grep("FSQ", EID$Sample.ID), ]

EID2 <- EID[!grepl("FSQ|CDC", EID$Sample.ID), ]

EID1 <- EID1 %>% mutate(., Sample.ID = substr(EID1$Sample.ID, 4, 14))

EID1 <- EID1 %>% mutate(., GUSPEC.Index = substr(EID1$Sample.ID, 1, 8))

EID2 <- EID2 %>% mutate(., GUSPEC.Index = substr(EID2$Sample.ID, 1, 8))

EID1 <- EID1[!duplicated(EID1$Sample.ID),]

EID2 <- EID2[!duplicated(EID2$Sample.ID),]

EID <- rbind(EID1, EID2)


EID <- EID %>% mutate(., Accepted.Date.Time = as.Date(substr(EID$Accepted.Date.Time, 1, 
                                                             nchar(as.character(EID$Accepted.Date.Time))-5), "%m/%d/%Y"))

EID <- EID %>% mutate(., GUSPEC.Index = substr(EID$Sample.ID, 1, 8))

EID <- EID[!duplicated(EID$Sample.ID),]

}


### For Tanzania

if(Country == 3){
EID <- EID[!grepl("CDC", EID$Sample.ID),]

EID <- EID[as.character(EID$Sample.ID)!="",]

EID1 <- EID[grep("FSQ", EID$Sample.ID), ]

EID2 <- EID[!grepl("FSQ|CDC", EID$Sample.ID), ]

EID1 <- EID1 %>% mutate(., Sample.ID = substr(EID1$Sample.ID, 4, 14))

EID1 <- EID1 %>% mutate(., GUSPEC.Index = substr(EID1$Sample.ID, 1, 8))

EID2 <- EID2 %>% mutate(., GUSPEC.Index = substr(EID2$Sample.ID, 1, 8))

EID1 <- EID1[!duplicated(EID1$Sample.ID),]

EID2 <- EID2[!duplicated(EID2$Sample.ID),]

EID <- rbind(EID1, EID2)


EID <- EID %>% mutate(., Accepted.Date.Time = substr(EID$Accepted.Date.Time, 1, 10))

EID <- EID %>% mutate(., GUSPEC.Index = substr(EID$Sample.ID, 1, 8))

EID <- EID[!duplicated(EID$Sample.ID),]
}



##########################################################################        DBS         #############################################################################
setwd(paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Results/Returning", sep = ""))
file.list <- dir()
file.list <- file.list[!grepl("~|EID|VL|approved|QC|Thumbs|original|AlereData|xlsx|zip|WS|ignore|MISSING", file.list)]

DBS.data.o <- lapply(file.list, function(X) {data.frame(id = basename(X), read.csv(X))})

DBS.data <- list()


############ For Uganda

if(Country == 1){

for (i in 1:length(file.list)) {
  a <- DBS.data.o[[i]] %>% as.data.frame()
  
  names(a) <- c("id", apply(a[20,2:ncol(a)], 2, as.character))
  
  a <- a %>% mutate(., Accepted.Date.Time = as.Date(substr(a[5, 3], 1, 10), "%d/%m/%Y"))
  
  a <- a[-c(1:20),]
  
  a <- a %>% select(., file.name.vl = id, Sample.ID = `SAMPLE ID`, Result = RESULT, Accepted.Date.Time)
  
  DBS.data[[i]] <- a
}

DBS <- do.call("rbind", DBS.data)

DBS <- DBS[!grepl("HIV", DBS$Sample.ID),]

DBS <- DBS[!duplicated(DBS$Sample.ID),]

names(DBS)[2] <- "PTID" 

}


############# For Tanzania

if(Country == 3){
for (i in 1:length(file.list)) {
  a <- DBS.data.o[[i]] %>% as.data.frame()
  
  a <- a %>% mutate(., Accepted.Date.Time = as.Date(substr(a$Accepted.Date.Time, 1, 10), "%Y/%m/%d"))
  
  a <- a %>% select(., file.name.vl = id, Sample.ID, Result, Accepted.Date.Time)
  
  DBS.data[[i]] <- a
}

DBS <- do.call("rbind", DBS.data)


DBS <- DBS[!grepl("CDC", DBS$Sample.ID),]

DBS <- DBS[as.character(DBS$Sample.ID)!="",]

DBS1 <- DBS[grep("FSQ", DBS$Sample.ID), ]

DBS2 <- DBS[!grepl("FSQ|CDC", DBS$Sample.ID), ]

DBS1 <- DBS1 %>% mutate(., Sample.ID = substr(DBS1$Sample.ID, 4, 14))

DBS1 <- DBS1 %>% mutate(., GUSPEC.Index = substr(DBS1$Sample.ID, 1, 8))

DBS2 <- DBS2 %>% mutate(., GUSPEC.Index = substr(DBS2$Sample.ID, 1, 8))

DBS1 <- DBS1[!duplicated(DBS1$Sample.ID),]

DBS2 <- DBS2[!duplicated(DBS2$Sample.ID),]

DBS <- rbind(DBS1, DBS2)


DBS <- DBS[!duplicated(DBS$Sample.ID),]
}


##########################################################################     EID Approved     #############################################################################
setwd(paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Results/Returning", sep = ""))
file.list <- dir()[grep("approved", dir())]
file.list <- file.list[!grepl("~|VL|original|Instrument|WS|DBS|instrument", file.list)]

temp <- readxl::read_excel("SHIMS2 EID W14_approved.xlsx", col_names=FALSE)
nrow(temp)

approved.eid.data.temp <- lapply(file.list, function(X) {data.frame(id = basename(X), readxl::read_excel(X, sheet = 1, col_names=FALSE))})

list.ind <- lapply(approved.eid.data.temp, function(X){nrow(X)})

file.list <- file.list[-which(list.ind == 1)]

approved.eid.data <- lapply(file.list, function(X) {data.frame(id = basename(X), readxl::read_excel(X, sheet = 1)) %>% select(., file.name.approved = id, Sample.ID)})

approved.eid <- do.call("rbind.fill", approved.eid.data)


approved.eid1 <- approved.eid[grep("FSQ", approved.eid$Sample.ID), ]

approved.eid2 <- approved.eid[!grepl("FSQ|CDC", approved.eid$Sample.ID), ]

approved.eid1 <- approved.eid1 %>% mutate(., Sample.ID = substr(approved.eid1$Sample.ID, 4, 14))

approved.eid1 <- approved.eid1 %>% mutate(., GUSPEC.Index = substr(approved.eid1$Sample.ID, 1, 8))

approved.eid2 <- approved.eid2 %>% mutate(., GUSPEC.Index = substr(approved.eid2$Sample.ID, 1, 8))

approved.eid1 <- approved.eid1[!duplicated(approved.eid1$Sample.ID),]

approved.eid2 <- approved.eid2[!duplicated(approved.eid2$Sample.ID),]

approved.eid <- rbind(approved.eid1, approved.eid2)

##########################################################################     DBS Approved     #############################################################################
setwd(paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Results/Returning", sep = ""))
file.list <- dir()[grep("approved", dir())]
file.list <- file.list[!grepl("~|VL|original|Instrument|WS|EID|PILOT", file.list)]

DBS.approved.data.o <- lapply(file.list, function(X) {data.frame(id = basename(X), readxl::read_excel(X, sheet = 1))})

DBS.approved.data <- list()

### For Uganda

if(Country == 1){

for (i in 1:length(file.list)) {
  a <- DBS.approved.data.o[[i]] %>% as.data.frame()
  
  names(a) <- c("id", apply(a[18,2:ncol(a)], 2, as.character))
  
  a <- a %>% mutate(., Accepted.Date.Time = as.Date(substr(a[4, 3], 1, 10), "%d/%m/%Y"))
  
  a <- a[-c(1:18),]
  
  a <- a %>% select(., file.name.approved = id, Sample.ID = `SAMPLE ID`)
  
  DBS.approved.data[[i]] <- a
}

DBS.approved <- do.call("rbind", DBS.approved.data)

DBS.approved <- DBS.approved[!grepl("HIV", DBS.approved$Sample.ID),]


DBS.approved <- DBS.approved[!duplicated(DBS.approved$Sample.ID),]

names(DBS.approved) <- c("file.name.approved", "PTID")

DBS.approved <- DBS.approved[!is.na(DBS.approved$PTID),]

}

#### For Tanzania

if(Country == 3){

for (i in 1:length(file.list)) {
  a <- DBS.approved.data.o[[i]] %>% as.data.frame()
  
  a <- a %>% mutate(., Accepted.Date.Time = substr(a$Accepted.Date.Time, 1, 10))
  
  a <- a %>% select(., file.name.approved = id, Sample.ID)
  
  DBS.approved.data[[i]] <- a
}

DBS.approved <- do.call("rbind", DBS.approved.data)


DBS.approved <- DBS.approved[!grepl("CDC", DBS.approved$Sample.ID),]

DBS.approved <- DBS.approved[as.character(DBS.approved$Sample.ID)!="",]

DBS.approved1 <- DBS.approved[grep("FSQ", DBS.approved$Sample.ID), ]

DBS.approved2 <- DBS.approved[!grepl("FSQ|CDC", DBS.approved$Sample.ID), ]

DBS.approved1 <- DBS.approved1 %>% mutate(., Sample.ID = substr(DBS.approved1$Sample.ID, 4, 14))

DBS.approved1 <- DBS.approved1 %>% mutate(., GUSPEC.Index = substr(DBS.approved1$Sample.ID, 1, 8))

DBS.approved2 <- DBS.approved2 %>% mutate(., GUSPEC.Index = substr(DBS.approved2$Sample.ID, 1, 8))

DBS.approved1 <- DBS.approved1[!duplicated(DBS.approved1$Sample.ID),]

DBS.approved2 <- DBS.approved2[!duplicated(DBS.approved2$Sample.ID),]

DBS.approved <- rbind(DBS.approved1, DBS.approved2)


DBS.approved <- DBS.approved[!duplicated(DBS.approved$Sample.ID),]

names(DBS.approved)[2] <- "Sample.ID.approved"

}




#### EID + approved.eid

## For UG and SW

if(Country == 1|2){
EID_approved <- merge(EID, approved.eid, by = c("GUSPEC.Index"), all = TRUE)
}
## For TZ
if(Country == 3){
EID_approved <- merge(EID, approved.eid, by = c("GUSPEC.Index"), all = TRUE)

EID_approved <- EID_approved[,-7]

names(EID_approved)[3] <- "Sample.ID"
}

#### DBS + approved.dbs


# For Uganda
if(Country == 1){
DBS_approved <- merge(DBS, DBS.approved, by = c("PTID"), all = TRUE)
}

# For Tanzania
if(Country == 3){
DBS_approved <- merge(DBS, DBS.approved, by = c("GUSPEC.Index"), all = TRUE)
}

#### EID + DBS + approved.dbs

## For UG
if(Country == 1){
EID_DBS_approved <- rbind.fill(EID_approved, DBS_approved)
names(EID_DBS_approved) <- c("GUSPEC.Index", "file.name.vl","Sample.ID.vl","Order.Date.Time","Result","file.name.approved", "Sample.ID.approved", "PTID")
}

## For SW
if(Country == 2){
EID_DBS_approved <- EID_approved
names(EID_DBS_approved) <- c("GUSPEC.Index", "file.name.vl","Sample.ID.vl","Order.Date.Time","Result","file.name.approved", "Sample.ID.approved")
}

## For TZ
if(Country == 3){
EID_DBS_approved <- rbind.fill(EID_approved, DBS_approved)
names(EID_DBS_approved) <- c("GUSPEC.Index", "file.name.vl","Sample.ID.vl","Order.Date.Time","Result","file.name.approved", "Sample.ID.approved")
}

########################### VL + Approved######################

VL_approved.vl <- merge(VL, approved.vl, by = c("GUSPEC.Index"), all = TRUE)

VL_approved.vl <- VL_approved.vl %>% select(., GUSPEC.Index, file.name.vl, Sample.ID.vl = Sample.ID.x, Order.Date.Time, Result, file.name.approved, Sample.ID.approved = Sample.ID.y)

dtparts = t(as.data.frame(strsplit(as.character(VL_approved.vl$Order.Date.Time),' ')))
row.names(dtparts) = NULL
dtparts <- as.data.frame(dtparts)
VL_approved.vl <- VL_approved.vl %>% mutate(., Order.Date.Time = as.Date(dtparts$V1, "%m/%d/%Y"))

### VL + EID + DBS + approved

VL_EID_DBS_approved <- rbind.fill(VL_approved.vl, EID_DBS_approved)

#############################mlist + VL + Approved

# For Uganda and Tanzania

if(Country == 1){

names(VL_EID_DBS_approved)[8] <- "ptid"

VL_approved1 <- VL_EID_DBS_approved[is.na(VL_EID_DBS_approved$GUSPEC.Index), ]
VL_approved2 <- VL_EID_DBS_approved[!is.na(VL_EID_DBS_approved$GUSPEC.Index), ]



mlist_VL_approved1 <- merge(mlist, VL_approved1, by = "ptid", all = TRUE)

mlist_VL_approved1 <- mlist_VL_approved1[,-11]

names(mlist_VL_approved1)[10] <- "GUSPEC.Index"




mlist_VL_approved2 <- mlist_VL_approved1[is.na(mlist_VL_approved1$file.name.vl),][,1:10]


mlist_VL_approved2 <- merge(mlist_VL_approved2, VL_approved2, by = "GUSPEC.Index", all = TRUE)

mlist_VL_approved2 <- mlist_VL_approved2[,-17]

names(mlist_VL_approved2)[2] <- "ptid" 

mlist_VL_approved1 <- mlist_VL_approved1[!is.na(mlist_VL_approved1$file.name.vl),]


mlist_VL_approved <- rbind(mlist_VL_approved1, mlist_VL_approved2)

}

######for Swaziland and Tanzania
if(Country == 2|3){
mlist_VL_approved <- merge(mlist, VL_EID_DBS_approved, by = "GUSPEC.Index", all = TRUE)
}

#Specify start date and end date
stripped_mlist_VL_approved <- mlist_VL_approved[mlist_VL_approved$data.collection.date>=Start.Date & mlist_VL_approved$data.collection.date<=End.Date,]

ind <- apply(stripped_mlist_VL_approved, 1, function(x) all(is.na(x)))

stripped_mlist_VL_approved <- stripped_mlist_VL_approved[!ind, ]

dat <- stripped_mlist_VL_approved
dat <- dat %>% mutate(., time.lapse = as.Date(dat$date.sent.to.facility, origin = "1899-12-30")-as.Date(dat$data.collection.date,  origin = "1899-12-30"))

dat <- dat[!duplicated(dat$GUSPEC.Index),]
###Keep full data

dat.full <- mlist_VL_approved

result.missing.orders <- dat.full[!is.na(dat.full$file.name.vl) & is.na(dat.full$file.name.mlist), ]
orders.without.results <- dat.full[!is.na(dat.full$file.name.mlist) & is.na(dat.full$file.name.vl), ]

dat.full <- mlist_VL_approved[!is.na(mlist_VL_approved$file.name.mlist),]
dat.full <- dat.full[!duplicated(dat.full$guspec ),]

########################################   LDMS ###############################################

setwd("P:/PHIA Project/SI/Curated Files/LDMS")
file.list <- dir()[grep("csdb", dir())]
file.list <- file.list[!grepl("2016|MALAWI|ZAMBIA|ZIMBABWE", file.list)]

ldms.data <- lapply(file.list, function(X) {data.frame(id = basename(X), fread(X))})

ldms <- do.call("rbind", ldms.data)



##########################################################  Reconciliation Folder  #############################################################

library(readxl)

setwd(paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Reconciliation", sep = ""))


latest.file <- function(pattern, path){
  
  file.with.pattern <- dir(path)[grep(pattern, dir(path))]
  file.with.pattern <- file.with.pattern[!grepl("~", file.with.pattern)]
  
  file.name <- file.with.pattern
  
  months.regex <- paste(month.abb[c(1:12)], collapse='|')
  
  dates <- vector()
  for (i in 1:length(file.name)) {
    a <- strsplit(file.name, "_")[[i]]
    date.str <- a[grep(months.regex, a, ignore.case = TRUE)] %>% substr(., 1, 9)
    dates <- rbind(dates, date.str)
  }
  
  files <- vector()
  
  file.table <- data.frame(file.name = file.name, date = as.Date(dates[,1], "%d%b%Y"))
  file.table <- with(file.table, file.table[order(date, decreasing = TRUE),])
  file.single <- file.table[1,1] %>% as.vector()
  print(file.single)
}

pattern.list <- c("Missing_Specimen", "Missing_ODK_Pos", "PTID_Mismatch", "Retest_List")

file.list.missing.data <- list()
for (i in 1:length(pattern.list)) {
  a <- pattern.list[i]
  latest <- latest.file(a, paste("P:/PHIA Project/SI/Westat files/PHIA/", country, "/Reconciliation", sep = ""))
  latest.file.data <- data.frame(read_excel(latest, sheet = 1, col_names = FALSE) )
  colnames(latest.file.data) <- latest.file.data[1,] %>%  as.character()
  if(nrow(latest.file.data)==1){
    latest.file.data[1,] <- rep(NA, ncol(latest.file.data))
  }else{
    latest.file.data <- latest.file.data[-1,]
  }
  file.list.missing.data[[i]] <- latest.file.data
}

Specimen <- file.list.missing.data[[1]]
Specimen <- Specimen %>% mutate(., `Specimen Collection Date` = as.Date(as.numeric(Specimen$`Specimen Collection Date`), origin = "1899-12-30"))
Specimen.stripped <- Specimen[Specimen$`Specimen Collection Date`>=Start.Date & Specimen$`Specimen Collection Date`<= End.Date, ]
ind <- apply(Specimen.stripped, 1, function(x) all(is.na(x)))
Specimen.stripped <- Specimen.stripped[!ind, ]


ODK <- file.list.missing.data[[2]]
PTID.Mismatch <- file.list.missing.data[[3]]
Retest.List <- file.list.missing.data[[4]]


##### Summarise and generate report

# Total #/% orders (pull lists) submitted by Westat
line6 <- sum(!is.na(dat$file.name.mlist))

# Total #/% of discordant field result (ODK POS vs. LDMS NEG)
line7 <- sum(!is.na(Specimen.stripped$GUSPEC))

# Total #/% of mismatched PTID (ODK vs. LDMS)
line9 <- sum(!is.na(PTID.Mismatch$PTID))

# Total #/% of Pending cases (missing LDMS)
line10 <- sum(is.na(Specimen.stripped$GUSPEC))

# Total # of VL results received by Westat (among those ordered)
line11 <- sum(!is.na(dat$Sample.ID.vl)) + sum(is.na(dat$Sample.ID.vl) & !is.na(dat$date.sent.to.facility))
# line11 <- sum(!is.na(dat$Sample.ID.vl))

# b <- dat[dat[!is.na(dat$Sample.ID.vl),]$ptid%in%dat[!is.na(dat$date.sent.to.facility),]$ptid==F,]

# Total #/% of results returned
line12 <- sum(!is.na(dat$date.sent.to.facility))

# Total #/% of VL result Returned on time
line13 <- sum(dat$time.lapse<=deadline, na.rm = TRUE)

# Total #/% of VL result Returned late
line14 <- line12 - line13

## Quantile for those Returned late
line15 <- quantile(dat$time.lapse[dat$time.lapse > deadline], na.rm = TRUE)[2:4]

# Total #/% of VL result waiting for ILB approval
line16 <- sum(!is.na(dat$Sample.ID.vl))-sum(!is.na(dat$Sample.ID.approved))

# Total #/% of results not yet returned/unknown status
line17 <- line11 - sum(!is.na(dat$date.sent.to.facility))

## Already late
line17.2 <- sum(Sys.Date() - as.Date(dat[is.na(dat$date.sent.to.facility) & !is.na(dat$file.name.approved), ]$data.collection.date, origin = "1899-12-30") > deadline)

# Not yet late
# line17.1 <- sum(Sys.Date() - as.Date(dat[is.na(dat$date.sent.to.facility) & !is.na(dat$file.name.approved), ]$data.collection.date, origin = "1899-12-30") <= deadline)
line17.1 <- line17 - line17.2

# Total # of VL not received from UVRI
# line18 <- sum(is.na(dat$file.name.approved) & is.na(dat$file.name.vl) & !is.na(dat$file.name.mlist)) - sum(is.na(dat$Sample.ID.approved) & !is.na(dat$date.sent.to.facility))
line18 <- line6-line11

## Worring
w <- sum(is.na(dat$file.name.approved) & is.na(dat$file.name.vl) & !is.na(dat$file.name.mlist) & is.na(dat$date.sent.to.facility) &
           Sys.Date()-as.Date(dat$data.collection.date, origin = "1899-12-30")>49)

## No worring
# nw <- sum(is.na(dat$file.name.approved) & is.na(dat$file.name.vl) & !is.na(dat$file.name.mlist)& is.na(dat$date.sent.to.facility) & 
#             Sys.Date()-as.Date(dat$data.collection.date, origin = "1899-12-30")<=49)
nw <- line18-w





report <- data.frame(value = c(line6, line7, line9, line10, line11, line12, line13, line14, line15, line16, line17, line17.1, line17.2, line18, w, nw), 
                     row.names = c("Total #/% orders (pull lists) submitted by Westat",
                                   "Total #/% of discordant field result (ODK POS vs. LDMS NEG)",
                                   "Total #/% of mismatched PTID (ODK vs. LDMS)",
                                   "Total #/% of Pending cases (missing LDMS)",
                                   "Total # of VL results received by Westat (among those ordered)",
                                   "Total #/% of results returned",
                                   "Total #/% of VL result Returned on time",
                                   "Total #/% of VL result Returned late (>70 days)",
                                   "Q1",
                                   "Q2",
                                   "Q3",
                                   "Total #/% of VL result waiting for ILB approval",
                                   "Total #/% of results not yet returned/unknown status",
                                   "Not yet late",
                                   "Aready late",
                                   "Total # of VL not received from UVRI",
                                   "Worring",
                                   "No worries"))

### Save files for reference
list.not.yet.return <- dat[is.na(dat$date.sent.to.facility) & !is.na(dat$Sample.ID.approved),] %>% select(., -Result)
dat <- dat %>% select(., -Result)
list.over.seven.weeks <- dat[is.na(dat$file.name.approved) & is.na(dat$file.name.vl) & !is.na(dat$file.name.mlist) & 
                               Sys.Date()-as.Date(dat$data.collection.date, origin = "1899-12-30")>49,]

write.csv(report, paste("P:/PHIA Project/SI/Reports - Internal SI/RoR-prelim/Raw SAS or Excel Data/Report_", country, ".csv", sep = ""), na = "")
write.csv(list.not.yet.return, paste("P:/PHIA Project/SI/Reports - Internal SI/RoR-prelim/Raw SAS or Excel Data/Not_yet_return_", country, ".csv", sep = ""), row.names = FALSE, na = "")
write.csv(dat.full, paste("P:/PHIA Project/SI/Reports - Internal SI/RoR-prelim/Raw SAS or Excel Data/fulltable_", country, ".csv", sep = ""), row.names = FALSE, na = "")
write.csv(list.over.seven.weeks, paste("P:/PHIA Project/SI/Reports - Internal SI/RoR-prelim/Raw SAS or Excel Data/VL_not_received_", country, ".csv", sep = ""), row.names = FALSE, na = "")

returned.but.not.approved <- dat[is.na(dat$Sample.ID.approved) & !is.na(dat$date.sent.to.facility),]

write.csv(returned.but.not.approved, paste("P:/PHIA Project/SI/Reports - Internal SI/RoR-prelim/Raw SAS or Excel Data/returned_but_not_approved_", country, ".csv", sep = ""), row.names = FALSE, na = "")


