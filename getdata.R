#!/usr/bin/R

library(Matrix)
library(TunePareto)

#get the sparse matrix and docment labels
getmetadata<-function(datapath) 
	{
	last <- function(x) { tail(x, n = 1) }
	
	metaFile <- paste(datapath, 'meta.dat', sep="")
	mf  <- file(metaFile, open = "r")
	cntdocs<-as.integer(readLines(mf, n = 1, warn = FALSE))
	cntterms<-as.integer(readLines(mf, n = 1, warn = FALSE))
	cnttopics<-as.integer(readLines(mf, n = 1, warn = FALSE))
	
	docids<-vector("integer",cntdocs) # each item is a the id of the doc
	rows<-vector("integer",cntterms) # each item is a doc index
	cols<-vector("integer",cntterms)  # each item is a term index
	datas<-vector("integer",cntterms) # count of term in the doc
	doclabels<-vector("integer",cntdocs) # each item is a the class label of the doc belonging to 
	
	topics<-vector(length=cnttopics)
	for (ii in 1:cnttopics) topics[ii]<-strsplit(readLines(mf, n = 1, warn = FALSE),':')[[1]][1]
	close(mf)

	dataFile <- paste(datapath, 'corpus.dat', sep="")
	dat  <- file(dataFile, open = "r")
	docpos<-1
	termpos<-1
	while (length(oneLine <- readLines(dat, n = 1, warn = FALSE)) > 0) 
		{
		docstr<- strsplit(oneLine, " ")[[1]]

		nostr <- strsplit(docstr[1],':')[[1]][2]
		nostr <- substr( nostr,2, nchar(nostr)-1 )
		docids[docpos]=as.integer(nostr)

		datstr<-sapply( docstr[2:(length(docstr)-1)], strsplit,':' )
		datmat<-sapply(datstr, as.integer)

		rows[termpos:(termpos+ncol(datmat)-1)]<-docpos
		cols[termpos:(termpos+ncol(datmat)-1)]<-as.vector(datmat[1,])
		
		#datas[termpos:(termpos+ncol(datmat)-1)]<-as.vector(datmat[2,])
		datas[termpos:(termpos+ncol(datmat)-1)]<-1
		
		termpos <- termpos + ncol(datmat)
		
		topic<-last(docstr)
		topicpos<-match(topic,topics)
		doclabels[docpos]<-topicpos
		
		docpos<-docpos+1
		} 
	close(dat)


	dictFile <- paste(datapath, 'vocabulary', sep="")
	dict  <- file(dictFile, open = "r")
	readLines(dict, n = 1, warn = FALSE)
	cnt <- as.integer(readLines(dict, n = 1, warn = FALSE))
	cnames<-vector(length=cnt)
	for (i in 1:cnt) 
		{
		oneLine <- readLines(dict, n = 1, warn = FALSE) 
		cnames[i]<-oneLine
		}
	close(dict)


	clabels<-vector(length=cntdocs)
	for (i in 1:cntdocs) clabels[i]<-topics[doclabels[i]]

	#get the sparse matrix
	sparsem<- sparseMatrix( rows, cols, x=datas ,dimnames=list(docids, cnames ) )
	
	print('getmetadata finished')
	return( list('sparsem'=sparsem,'labels'=clabels,'docids'=docids) )
	}
	
#training and testing partition
ttpartition<-function(metadata,nfolds)
	{
	clabels<-metadata$'labels'
	foldList <- generateCVRuns(labels = clabels,ntimes = 1, nfold = nfolds, stratified=TRUE)
	print('ttpartition finished')
	return(foldList) #foldList[[1]][i] is the ith fold . 
	}




