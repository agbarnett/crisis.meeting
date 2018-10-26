# find.schools.pubmed.R
# find every medical and public health school based on searching pubmed
# Oct 2018
library(stringr)
library(rentrez)
source('rpubmed-master/R/rpubmed_fetch.R') # could not install rpubmed, but this function is useful, because it has affiliations

# search for recent papers with an Australian affilation
query = "2018[PDAT] AND Australia[AFFL]"
# get the pubmed IDs
max.ids = 10000
ids = entrez_search("pubmed", query, retmax = max.ids)$ids 

# now extract affiliation data
all.records  = list()
# search in loops
num = 100
upper.loop = (max.ids / num) - 1
for (k in 1:upper.loop){
  start = (k-1)*num + 1
  stop = k*num
  records = fetch_in_chunks(ids[start:stop])
  Sys.sleep(30) # sleep for this many seconds; avoid overloading system
  all.records = c(all.records, records)
  if(k%%20==0){cat('Up to',k,'\n')}
}

# accumulate Australian affiliations
Affiliations = NULL
for (outer.loop in 1:length(all.records)){
  affiliations = all.records[[outer.loop]]$MedlineCitation$Article$AuthorList
  for (inner.loop in 1:length(affiliations)){
    if('AffiliationInfo' %in% names(affiliations[[inner.loop]])){ # if some affiliation data
      this.aff = affiliations[[inner.loop]]$AffiliationInfo$Affiliation
      if(nchar(this.aff) < 1000){ # needed to avoid dumps of huge author numbers
        if(str_detect(this.aff, pattern='Australia')){ # if find Australia
          if(str_detect(string=this.aff, pattern='Rashedi')){cat(outer.loop, '\n')}
          this.aff = paste(strsplit(this.aff, split=',')[[1]][1:2], collapse=',', sep='') # take first two parts after comma
          Affiliations = c(Affiliations, this.aff)}
      }
    }
  }
}
# tidy up
Affiliations = str_replace_all(string=Affiliations, pattern=', Australia\\.', replace='')
Affiliations = str_replace_all(string=Affiliations, pattern=', Sydney|, Brisbane|, Adelaide|, Perth|, Melbourne|, Darwin|, Newcastle|, Hobart.', replace='')
Affiliations = str_replace_all(string=Affiliations, pattern=' $', replace='')
# order table by frequency
tab = table(Affiliations)
tab = tab[order(-tab)]
head(tab, 20)

# save top 1000
table = tab[1:1000]
save(table, file='Table.RData')

# output for github; top 100; '|' used to make table in github
out = file('githubTable.txt', 'w')
cat('|Place|Frequency|Person|Invited|\n', file=out)
cat('|:---- |:---- |:---------- | :---------- |\n', file=out)
for (k in 1:100){
  cat('|',names(tab[k]), '|', as.numeric(tab[k]), '| |\n', file=out, sep='')
}
close(out)

to.out = data.frame(head(tab, 50))
pander(to.out, style='simple')
