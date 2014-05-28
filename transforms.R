#!/urs/bin/R

library(Matrix)

gettransformset<-function(metadata,features,foldList,iter)
	{
	print('enter transforms')
	sparsem<-metadata$'sparsem'
	clabels<-metadata$'labels'
	docids<-metadata$'docids'
	klasses<- levels(factor(clabels))
	#print(klasses)
	
	testingset<-foldList[[1]][iter][[1]]
	testingset<-as.integer(levels(factor(testingset)))
	trainingset<-setdiff(1:nrow(sparsem),testingset)
	
	trainingsparsem<-sparsem[trainingset,features]
	trainingids<-as.integer(rownames(trainingsparsem))   #convert ids into integer for rapidminer using
	traininglabels<-match(clabels[trainingset],klasses)	#convert labels into integer for rapidminer using

	testingsparsem<-sparsem[testingset,features]
	testingids<-as.integer(rownames(testingsparsem))   #convert ids into integer for rapidminer using
	testinglabels<-match(clabels[testingset],klasses)	#convert labels into integer for rapidminer using
	
	######################################get training set########################################################
	trainsetmtx<- matrix(data = 0, nrow =nrow(trainingsparsem), ncol = ncol(trainingsparsem), byrow = FALSE,dimnames = list(rownames(trainingsparsem),colnames(trainingsparsem)) )
	for (colidx in 1:ncol(trainingsparsem))
		trainsetmtx[,colidx]<-trainingsparsem[,colidx]
	
	######################################get testing set########################################################
	testsetmtx<- matrix(data = 0, nrow =nrow(testingsparsem), ncol = ncol(testingsparsem), byrow = FALSE,dimnames = list(rownames(testingsparsem),colnames(testingsparsem)) )
	for (colidx in 1:ncol(testingsparsem))
		testsetmtx[,colidx]<-testingsparsem[,colidx]
		
	trainingweight<-1/nrow(trainsetmtx)
	testingweight<-1/nrow(testsetmtx)

	traindf<-as.data.frame(cbind(trainingids,cbind(traininglabels,cbind(trainingweight,trainsetmtx))))
	testdf<-as.data.frame(cbind(testingids,cbind(testinglabels,cbind(testingweight,testsetmtx))))
	
	print('transforms finished')
	return(list('traindf'=traindf,'testdf'=testdf))
	}
	