#script to extract and rank paper titles to relevance for the lab research
rm(list = ls())
#install.packages('rvest')
#install.packages('xml2')
#devtools::install_github("collectivemedia/tictoc")
library(plyr)
library(xml2)
library(rvest)
library(tictoc)
setwd('/Users/thiemia/papers')

tic()

#things to do:
year=19
month=9

#for AJM month has to be multiple of 3
#update cell links

#quaterly 1. of quarter
ajyear <- 80+year
ajissue <- month/3+1
phrase <- c('https://www.physiology.org/toc/physrev/',ajyear,'/',ajissue)
phrase <- paste(phrase, sep='',collapse = '')
ajm <- xml2::read_html(phrase)
ajmt <- ajm %>%
  html_nodes('.issue-item__title a') %>%   
  html_text()
ajmd <- ajm %>%
  html_nodes('.date') %>%
  html_text()
ajml <- ajm %>%
  html_nodes('.issue-item__title a') %>%    html_attr('href')
ajmlf <-NULL
for ( i in seq_along(ajml)) {
  linkphrase <- c('http://www.physiology.org',ajml[i])
  ajmlf[i] <- paste(linkphrase, sep='',collapse = '')
}
ajmd1 <- format(ajmd, format="%Y %b %d")
ajmd1 <- as.Date(ajmd1," %Y %b %d" )
aj <- as.data.frame(ajmt)
aj$date <- ajmd1
aj$journal <- 'AJPhys'
aj$links <- ajmlf
ajmabs <- NULL
for ( f in seq_along(ajml)) {
  ajmfull <- html(ajmlf[f])
  aux1 <- ajmfull %>% 
    html_nodes('.abstractInFull p') %>% 
    html_text()
  determinator <- identical(aux1, character(0))  
  #use if statement to prevent error due to no abstract present
  if (determinator==T) {
    aux1 <-'none'
  }
  ajmabs[f] <- aux1
}
#ajmabs <- c(ajmabs,'none','none')
aj$abstract <- ajmabs
names(aj)[names(aj) == "ajmt"] <- 'Title'

#monthly 1. of month
natby <- year+18
phrase <- c('http://www.nature.com/nbt/journal/v',natby,'/n',month,'/index.html')
phrase <- paste(phrase, sep='',collapse = '')
natb <- xml2::read_html(phrase)
natbt <- natb %>%
  html_nodes('.atl') %>%  
  html_text()
natbl <- natb %>%
  html_nodes('.atl') %>%  html_attr('id')
natblf <-NULL
for ( i in seq_along(natbl)) {
  linkphrase <- c('https://www.nature.com/articles/',natbl[i])
  natblf[i] <- paste(linkphrase, sep='',collapse = '')
}
natbt <- sub(' - p.*','',as.character(natbt))
natbd <- c('20',year,'-',month,'-','01')
natbd <- paste(natbd, collapse = '',sep = '')
natbd <- as.Date(natbd)
nb <- as.data.frame(natbt)
nb$date <- natbd
nb$journal <- 'NatureBT'
nb$links <- natblf
natbabs <- NULL
for ( f in seq_along(natbl)) {
  natbfull <- html(natblf[f])
  aux1 <- natbfull %>% 
    html_nodes('#abstract-content p') %>% 
    html_text()
  determinator <- identical(aux1, character(0))  
  #use if statement to prevent error due to no abstract present
  if (determinator==T) {
    aux1 <-'none'
  }
  natbabs[f] <- aux1
}

#natbabs <- c(natbabs,'none')
nb$abstract <- natbabs
names(nb)[names(nb) == "natbt"] <- 'Title'

#weekly wednesday
stc<-NULL
week <- month*3
for ( i in 1:5) {
  issue <- 421+week-1+i
  phrase <- c('http://stm.sciencemag.org/content/',year-8,'/',issue)
phrase <- paste(phrase, sep='',collapse = '')
stm <- html(phrase)
stmt <- stm %>%
  html_nodes('.media__headline__title') %>%
  html_text()
stmd <- stm %>%
  html_nodes('.section-title__tagline') %>%
  html_text()
stml <- stm %>% html_nodes(".variant-full-text") %>% html_attr('href')
stmlf <-NULL

for ( s in seq_along(stml)) {
  linkphrase <- c('http://stm.sciencemag.org',stml[s])
  stmlf[s] <- paste(linkphrase, sep='',collapse = '')
}

stmd <- sub('\\\n','',as.character(stmd))
stmd <- sub('\\\n','',as.character(stmd))
phrase2 <- c('\\Vol ',year-8,', Issue ',issue)
phrase2 <- paste(phrase2, sep='',collapse = '')
stmd <- sub(phrase2,'',as.character(stmd))
stmd1 <- format(stmd, format="%d %B %Y")
stmd1 <- as.Date(stmd1," %d %B %Y" )
st <- as.data.frame(stmt)
st$date <- stmd1
st$journal <- 'ScienceTM'
names(st)[names(st) == "stmt"] <- 'Title'
st$links <- stmlf


stmabs <- NULL
for ( f in seq_along(stml)) {
  stmfull <- html(stmlf[f])
  naming <- stmfull %>%
    html_nodes('.abstract') %>% #html_nodes('.content')  %>%
    html_text()
  if (identical(naming,character(0))) {
    naming <- NA
  } 
  stmabs[f] <- naming
}
stmabs1 <- stmabs
stmabs1[length(stmabs)] <- NA
stmabs1[length(stmabs)] <- stmabs[length(stmabs)]
st$abstract <- stmabs1
stc <- rbind.fill(stc,st)
}
# abstract <- NULL
# stmlinks <- stm %>% html_nodes(".variant-full-text") %>% html_attr('href')
# series <- seq.int(4,4000,by=4)
# weblinks=NULL
# for ( i in seq_along(stmt)) {
#   link = stmlinks[i]
#   word = 'abstract'
#   if (pmatch(rev(link),rev(word))) {
#     
#   }
#   chars <- "test"
#   value <- "est"
#   phrase3 <- c('http://stm.sciencemag.org',stmlinks[link])
# phrase3 <- paste(phrase3, sep='',collapse = '')
# stma <- html(phrase3)
# weblinks[i] <- phrase3
# abs <- stma %>% html_nodes(".abstract") %>%
#   html_text()
# abstract[i] <- abs[1]
# 
# }
# st$abstract <- abstract
# stc <- rbind.fill(stc,st)


# library(devtools)
# install_github('akshaynagpal/rgscholar')
# library(RgScholar)
# query_2 <- google_Scholar("heart rate",year_low = 1989, year_high = 2015,journal = "nature")

#biweekly Thursday 2./4.

celllinks <- c('https://www.cell.com/cell/issue?pii=S0092-8674(17)X0009-3',
'https://www.cell.com/cell/issue?pii=S0092-8674(17)X0008-1')
allce <- NULL
for (i in seq_along(celllinks)) {
  
  cell <- html(celllinks[i])   #'http://www.cell.com/cell/current'
cellt <- cell %>%
  html_nodes('.title a') %>% 
  html_text()
celll <- cell %>%
  html_nodes('.title a') %>% html_attr('href')
celllf <-NULL
for ( s in seq_along(celll)) {
  linkphrase <- c(celll[s])  #'http://www.cell.com'
  celllf[s] <- paste(linkphrase, sep='',collapse = '')
}

celld <- cell %>%
  html_nodes('.date') %>%
  html_text()
celld <- sub('\\\n','',as.character(celld))
celld <- sub('\\\n','',as.character(celld))
celld1 <- format(celld, format="%B %d, %Y")
celld1 <- as.Date(celld1," %b %d, %Y" )
ce <- as.data.frame(cellt)
ce$date <- celld1
ce$journal <- 'cell'
ce$links <- celllf
names(ce)[names(ce) == "cellt"] <- 'Title'

cellabs <- 'None'
for ( f in seq_along(celllf)) {
  cellfull <- html(celllf[f])
  naming <- cellfull %>%
    html_nodes('.abstract') %>% html_nodes('.content')  %>%
   html_text()
  if (identical(naming,character(0))) {
    naming <- NA
  } 
  cellabs[f] <- naming
}
ce$abstract <- cellabs
allce <- rbind.fill(allce,ce)
}


#continuos
page <- c(1,1,1,1,1,1,1,1,1,1,1,1,1,1,1)
page2 <- c(10,10,10,10,10,10,10,10,10,10,10,10,10,10,10)
words <- c('#listing .teaser__header_text_link',rep('.teaser__header_text',length(page)-1))
wordsl <-  c('#listing .teaser__header_text_link',rep('.teaser__header_text_link',length(page)-1))
elc<-NULL
for ( i in seq_along(page)) {
  el<-NULL
  phrase <- c('https://elifesciences.org/?page=',i)
  phrase <- paste(phrase, sep='',collapse = '')
  elife <- html(phrase)
  elifet <- elife %>%
    html_nodes(words[i])  %>% 
    html_text()
  elifel <- elife %>%
    html_nodes(wordsl[i]) %>% html_attr('href')
  elifelf <-NULL
  for ( s in seq_along(elifel)) {
    linkphrase <- c('https://elifesciences.org',elifel[s])
    elifelf[s] <- paste(linkphrase, sep='',collapse = '')
  }
  elifet1 <- sub('\\\n','',as.character(elifet))
  elifet2 <- sub('\\\n','',as.character(elifet1))
  elifet2 <- sub('\\        ','',as.character(elifet2))
  elifed <- elife %>%
    html_nodes(' .date') %>%
    html_text()
  elifed <- elifed[page[i]:page2[i]]
  elifed <- sub('\\Updated','',as.character(elifed))
  elifed1 <- format(elifed, format="%b %d, %Y")
  elifed1 <- as.Date(elifed1," %b %d, %Y" )
  el <- as.data.frame(elifet2)
  el$date <- elifed1
  el$journal <- 'elife'
  names(el)[names(el) == "elifet2"] <- 'Title'
  el$links <- elifelf
  
  elifeabs <- NULL
  for ( f in seq_along(elifel)) {
    elifefull <- html(elifelf[f])
    elifeabs[f] <- elifefull %>%
      html_nodes('#abstract .paragraph') %>% 
      html_text()
  }
  el$abstract <- elifeabs
  elc <- rbind.fill(elc,el)
}


#biweekly 1./3. wednesday
emc <-NULL
for (i in 1:2) {
  issue <- month+2+i
  phrase <- c('http://emboj.embopress.org/content/',year+19,'/',issue)
  phrase <- paste(phrase, sep='',collapse = '')
  embo <- html(phrase)
  embot <- embo %>%
    html_nodes('.highwire-cite-title') %>% 
    html_text()
  embol <- embo %>%
    html_nodes('.highlight-image-linked') %>% html_attr('href')
  embolf <-NULL
  for ( s in seq_along(embol)) {
    linkphrase <- c('http://emboj.embopress.org',embol[s])
    embolf[s] <- paste(linkphrase, sep='',collapse = '')
  }
  embod <- embo %>%
    html_nodes('.pane-1 p') %>%
    html_text()
  phrase2 <- c('\\Volume ',year+19,', Number ',issue,',')
  phrase2 <- paste(phrase2, sep='',collapse = '')
  embod <- sub(phrase2,'',as.character(embod))
  embod1 <- format(embod, format="%d %B %Y")
  embod1 <- as.Date(embod1," %d %B %Y" )
  em <- as.data.frame(embot)
  em$date <- embod1
  em$journal <- 'embo'
  names(em)[names(em) == "embot"] <- 'Title'
  em$links <- embolf
  emboabs <- NULL
  for ( f in seq_along(embol)) {
    embofull <- html(embolf[f])
    emboabs[f]
    aux <- embofull %>%
      html_nodes('.section') 
    emboabs[f] <- aux[3] %>% 
      html_text()
  }
  # embofullt <- NULL
  # for ( f in seq_along(embol)) {
  #   embofull <- html(embolf[f])
  #   embofullt[f] <- embofull %>%
  #     html_nodes('.fulltext-view') %>%
  #     html_text()
  # }
  # em$fulltext <- embofullt
  em$abstract <- emboabs
  emc <- rbind.fill(emc,em)
}


#monthly 1. of month
phrase <- c('http://embor.embopress.org/content/',year+1,'/',month)
phrase <- paste(phrase, sep='',collapse = '')
embor <- html(phrase)
embort <- embor %>%
  html_nodes('.highwire-cite-title') %>%
  html_text()
embord <- embor %>%
  html_nodes('.pane-1 p') %>%
  html_text()
emborl <- embor %>%
  html_nodes('.highlight-image-linked') %>% html_attr('href')
emborlf <-NULL
for ( s in seq_along(emborl)) {
  linkphrase <- c('http://embor.embopress.org',emborl[s])
  emborlf[s] <- paste(linkphrase, sep='',collapse = '')
}
embord <- sub('\\; volume 19, issue 1','',as.character(embord))
embord1 <- format(embord, format="%d %B %Y")
embord1 <- as.Date(embord1," %d %B %Y" )
emr <- as.data.frame(embort)
emr$date <- embord1
emr$journal <- 'embo reports'
emr$links <- emborlf
names(emr)[names(emr) == "embort"] <- 'Title'

emborabs <- NULL
for ( f in seq_along(emborl)) {
  emborfull <- html(emborlf[f])
  emborabs[f]
  aux <- emborfull %>%
    html_nodes('.section') 
  emborabs[f] <- aux[3] %>% 
    html_text()
}
emr$abstract <- emborabs
# emborfullt <- NULL
# for ( f in seq_along(emborl)) {
#   emborfull <- html(emborlf[f])
# emborfullt[f] <- emborfull %>%
#   html_nodes('.fulltext-view') %>%
#   html_text()
# #strsplit(emborfullt, ' ')
# }
# emr$fulltext <- emborfullt

#monthly irregular
phrase <- c('http://msb.embopress.org/content/',year-4,'/',month)
phrase <- paste(phrase, sep='',collapse = '')
msb <- html(phrase)
msbt <- msb %>%
  html_nodes('.highwire-cite-title') %>%
  html_text()
msbd <- msb %>%
  html_nodes('.pane-1 p') %>%
  html_text()
msbl <- msb %>%
  html_nodes('.highlight-image-linked') %>% html_attr('href')
msblf <-NULL
for ( s in seq_along(msbl)) {
  linkphrase <- c('http://msb.embopress.org',msbl[s])
  msblf[s] <- paste(linkphrase, sep='',collapse = '')
}
msbd <- sub('\\; volume 14, issue 3','',as.character(msbd))
msbd1 <- format(msbd, format="%d %B %Y")
msbd1 <- as.Date(msbd1," %d %B %Y" )
molsys <- as.data.frame(msbt)
molsys$date <- msbd1
molsys$journal <- 'Molecular Systems Biology'
molsys$links <- msblf
names(molsys)[names(molsys) == "msbt"] <- 'Title'

msbabs <- NULL
for ( f in seq_along(msbl)) {
  msbfull <- html(msblf[f])
  msbabs[f]
  aux <- msbfull %>%
    html_nodes('.section') 
  msbabs[f] <- aux[3] %>% 
    html_text()
}
molsys$abstract <- msbabs




allj <- rbind.fill(aj,nb,allce,emc,emr,elc,stc,molsys)

thetitle <- allj$Title
newtitle <-NULL
thetitle <- as.vector(thetitle)
for (i in seq_along(thetitle)) {
  newtitle[i] <- strsplit(thetitle[[i]], ' ')
}

theabs <- allj$abstract
newabs <- NULL
theabs <- as.vector(theabs)
for ( i in seq_along(theabs)) {
  newabs[i] <- strsplit(theabs[[i]], ' ')
}

mission <- c('kinetic','kinetically','time','variability','environment','environmental','processes','process','phenotype','expression',
             'mechanisms','population','immunotherapy','immunological','stochastic','noisy','extracellular','noise','modeling', 'cells', 
             'genome','yeast','sequencing','rna','cerevisiae','sense','antisense','transcription','respond','signals',
             'quantitative','dynamics','predictive','transduction','noncoding','individual','computational','regulation','fundamental','biophysical',
             'molecular','perturbation','perturbations','system','systems','physiologically','predictions','health','mapk','p38',
             'hog1','jnk','jnk1','jnk2','erk','cancer','cell','hematopoietic','stem','immunity',
             'single-cell','phosphorylation','t','macrophage','dendritic','monocytic','monocyte','immune','signaling','stress',
             'osmotic','nacl','hyperosmotic','hyperosmolarity','crispr','reprogramming','immune','chromatin','lymphoid','apoptosis',
             'signalosome','signal','rate','temporal','proteome','development','mrna','mrnas','network','pathway',
             'kinase','autoimmune','dna','crispr-cas9','nucleosome', 'non-coding')
missionweight <- c(10,10,20,10,20,20,10,10,5,10,
                   5,10,10,10,15,20,20,20,20,5,
                   10,50,10,10,20,20,10,20,10,20,
                   20,25,25,20,15,10,20,10,10,10,
                   10,20,20,25,25,10,20,10,20,50,
                   50,50,20,20,20,10,10,5,10,15,
                   15,20,15,15,15,15,15,15,25,20,
                   20,20,25,25,20,15,20,15,10,20,
                   10,20,25,30,10,5,10,10,20,20,
                   15,10,5,20,10,20)
Amanda <- c('transcription','factor','chromatin','polymerase','initiation','mrna','mrnas','cerevisiae','rna','rnas','histone','deacetylation','yeast','cerevisiae','methylation')
Amw <- c(rep(50,length(Amanda)),missionweight)
missionAm <- c(Amanda,mission)
Alex <- c('immune','nfat5','p38','t','monocyte','thp-1','thp1','jurkat','salt','hypertension','cardiovascular','pulse')
Alw <- c(rep(50,length(Alex)),missionweight)
missionAl <- c(Alex,mission)
Ben <- c('embryonic','stem','inactivation','x','chromosome','x-chromosome','mouse','murine','non-coding')
Bw <- c(rep(50,length(Ben)),missionweight)
missionB <- c(Ben,mission)
Hossein <- c('modeling','prediction','mathematical','distribution','model','stochastic','oscillations','oscillation')
Hw <- c(rep(50,length(Hossein)),missionweight)
missionH <- c(Hossein,mission)
Rohit <- c('antisense','bidirectional','rna','yeast','sequencing','chip','chip-exo','mrna','mrnas','cerevisiae','noncoding','lncrna','non-coding')
Rw <- c(rep(50,length(Rohit)),missionweight)
missionR <- c(Rohit,mission)

labm <- list(missionR,missionAl,missionH,missionAm,missionB)
weights <- list(Rw,Alw,Hw,Amw,Bw)
lbs <- c('Rohit','Alex','Hossein','Amanda','Ben')

# members <- NULL
# score <- NULL
# for (e in seq_along(labm)){
#   curlabm <- labm[[e]]
#   curw <- weights[[e]]
#   for (i in seq_along(newtitle)) {
#   score[i] <- 0
#   for (j in seq_along(curlabm)) {
#     determinator <- any(curlabm[j] %in% tolower(newtitle[[i]]))
# #use if statement to add value to data frame 
#     if (determinator==T) {
#        score[i] <-score[i]+ curw[j]/length(newtitle[[i]])
#     }
#   }
#   }
#   allj$score <-score
#   alljs <- allj[rev(order(allj$score)),]
#   alljs$lm <- lbs[e]
#   members <- rbind.fill(members,alljs)
# }

membersabs <- NULL
scoreabs <- NULL
for (e in seq_along(labm)){
  curlabm <- labm[[e]]
  curw <- weights[[e]]
  for (i in seq_along(newabs)) {
    scoreabs[i] <- 0
    for (j in seq_along(curlabm)) {
      determinator <- any(curlabm[j] %in% tolower(newabs[[i]]))
      #use if statement to add value to data frame 
      if (determinator==T) {
        scoreabs[i] <-scoreabs[i]+ curw[j]
      }
    }
  }
  allj$score <-scoreabs
  alljs <- allj[rev(order(allj$score)),]
  alljs$lm <- lbs[e]
  membersabs <- rbind.fill(membersabs,alljs)
}

newmemberabs <- subset(membersabs, select=-abstract)

setwd("/Users/thiemia/Box Sync/journalwatching")

Alext<- subset(newmemberabs, lm=='Alex')
Alext<-Alext[1:5]
write.csv(Alext, 'Alex04.csv', row.names = F)
Amandat<- subset(newmemberabs, lm=='Amanda')
Amandat<-Amandat[1:5]
write.csv(Amandat, 'Amanda04.csv', row.names = F)
Bent <- subset(newmemberabs, lm=='Ben')
Bent<-Bent[1:5]
write.csv(Bent, 'Ben04.csv', row.names = F)
Hosseint<- subset(newmemberabs, lm=='Hossein')
Hosseint<-Hosseint[1:5]
write.csv(Hosseint, 'Hossein04.csv', row.names = F)
Rohitt <- subset(newmemberabs, lm=='Rohit')
Rohitt<-Rohitt[1:5]
write.csv(Rohitt, 'Rohit04.csv', row.names = F)

Alex10 <- Alext[1:10,]
Alex10$name <- 'Alex'
Ben10 <- Bent[1:10,]
Ben10$name <- 'Ben'
Amanda10 <- Amandat[1:10,]
Amanda10$name <- 'Amanda'
Hossein10 <- Hosseint[1:10,]
Hossein10$name <- 'Hossein'
all10 <- rbind.fill(Alex10, Ben10,Amanda10,Hossein10)
all10$jtitle <- paste(all10$Title,all10$links, sep = ' ; ')
all10$jtitle <- paste(all10$jtitle,all10$journal, sep = ' ; ')

all10agg <- aggregate(name ~ jtitle, data=all10, c)
all10df <- data.frame(do.call('rbind', strsplit(as.character(all10agg$jtitle),' ; ',fixed=TRUE)))
all10df$name <- as.character(all10agg$name)
all10df <- all10df[order(all10df$X3),]
names(all10df)[1]<-paste("Title")
names(all10df)[2]<-paste("Links")
names(all10df)[3]<-paste("Journal")
all10df <- all10df[,c(3,1,2,4)]
write.csv(all10df, 'toptentable.csv', row.names = F)



# messi <-NULL
#  for (i in seq_along(all10df$Title)) {
#    
#    messi[2*i-1] <- as.character(all10df$Title[i])
#    messi[2*i] <- all10df$Links[[i]]
#    
#  }

toc()



# for (i in seq_along(newtitle)) {
#   score[i] <- 0
#   for (j in seq_along(mission)) {
#     determinator <- any(mission[j] %in% tolower(newtitle[[i]]))
#     #use if statement to add value to data frame 
#     if (determinator==T) {
#       score[i] <-score[i]+ missionweight[j]/length(newtitle[[i]])
#     }
#   }
# }
# allj$score <-score
# alljs <- allj[rev(order(allj$score)),]
find.java <- function() {
  for (root in c("HLM", "HCU")) for (key in c("Software\\JavaSoft\\Java Runtime Environment", 
                                              "Software\\JavaSoft\\Java Development Kit")) {
    hive <- try(utils::readRegistry(key, root, 2), 
                silent = TRUE)
    if (!inherits(hive, "try-error")) 
      return(hive)
  }
  hive
}
library(XML)
#install.packages(c("devtools", "rJython", "rJava", "rjson"))
library(devtools)

Sys.setenv(JAVA_HOME='C:/Program Files/Java/')
library(rJava)
library(rjson)
library(rJython)
#install.packages("rJava",dependencies = T, "http://rforge.net/",type="source")
#install_github("trinker/gmailR")
install.packages('mailR')
library(mailR)
library(gmailR)
mess <-Alext[1:10,c(1,3,4)]
messi<-NULL
for (i in 1:10) {
  
  messi[2*i-1] <- as.character(mess$Title[i])
  messi[2*i] <- mess$links[i]

}


#sort by journal,merge by title

messin <- paste(messi, collapse = '','\n',sep = '')
cat(messin)


gmail('athiemicke@gmail.com', password='algotrading',subject='journal alert', 
      message=messin, from='arthiemicke@gmail.com',
      server = "smtp.gmail.com:587", username = 'arthiemicke@gmail.com',
      confirmBeforeSend = F, clear.username = F)


