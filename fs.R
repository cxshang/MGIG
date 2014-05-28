#!/urs/bin/R

dyn.load("overallentropy.so")

library(Matrix)


getfeatures<-function(metadata, foldList, iter,topk)
	{
	sparsem<-metadata$'sparsem'
	clabels<-metadata$'labels'
	docids<-metadata$'docids'
	
	testingset<-foldList[[1]][iter][[1]]
	testingset<-as.integer(levels(factor(testingset)))
	trainingset<-setdiff(1:nrow(sparsem),testingset)
		
	klasses<- levels(factor(clabels))
	traininglabels<-clabels[trainingset]
	trainingklasses<-levels(factor(traininglabels))
	
	####################################################get  A, B, C ,D ######################################
	N<-length(trainingset)
		
	traintermsums<-colSums(sparsem[trainingset,])
	trainingterms<-colnames(sparsem)[which(traintermsums!=0)]

	#A_plus_B is total occurences of term=1:
	A_plus_B<-traintermsums[traintermsums!=0]
	#C_plus_D is total occurences of term=0:
	#C_plus_D<-N - A_plus_B
	#A_plus_C is total occurences of class:
	A_plus_C<-matrix(data = 0, nrow =length(trainingklasses), ncol =1 , byrow = TRUE, dimnames = list(trainingklasses,'value'))
	
	A<-matrix(data = 0, nrow =length(trainingklasses), ncol = length(trainingterms), byrow = TRUE, dimnames = list(trainingklasses,trainingterms))
	for (klass in trainingklasses )
		{
		thisklassset<-which(traininglabels==klass)
		A_plus_C[klass,1]<- length( thisklassset )
		if (length(thisklassset)>1 )
			A[klass,]<-colSums(sparsem[trainingset,trainingterms][thisklassset,])
		else
			A[klass,]<-sparsem[trainingset,trainingterms][thisklassset,]
		}

	#B[klass,]<-A_plus_B - A[klass,]
	#C[klass,]<-A_plus_C[klass] - A[klass,]
	#D[klass,]<-N- A_plus_B - A_plus_C[klass] + A[klass,]
	
	####################################################midi codes######################################
	trainmtx<-A
	termscnt<-A_plus_B
	bgdistr<-as.vector(A_plus_C/N)
	totaltermscnt<-sum(termscnt)
	
	nTERM<-ncol(trainmtx)
	nCLASS<-nrow(trainmtx)
	stopifnot(topk<=nTERM)
	stopifnot(topk>=1)
	
	#strtip<-paste('sparse ratio is ', 1-length(which(trainmtx!=0))/(nTERM*nCLASS), sep='')
	#print(strtip)
	
	selected<-vector('logical',nTERM)
	selectedorder<-vector('integer',nTERM)

	#===================================calculate  weighted entropy of each term.===============================
	weighted_termsentropy<-vector('numeric',nTERM)
	for (klaster in 1:nCLASS)
		{
		nonzeros<-which(trainmtx[klaster,]!=0)
		weighted_termsentropy[nonzeros]<-weighted_termsentropy[nonzeros]-trainmtx[klaster,nonzeros]*log(trainmtx[klaster,nonzeros])   
		}
	weighted_termsentropy<-weighted_termsentropy+termscnt*log(termscnt)
	
	#===================================calculate  mutual information of each term.===============================
	mis<-vector('numeric',nTERM)
	for (klaster in 1:nCLASS)
		{
		nonzeros<-which(trainmtx[klaster,]!=0)
		nnzprobs<-trainmtx[klaster,nonzeros]/termscnt[nonzeros]
		mis[nonzeros]<-mis[nonzeros] + nnzprobs*(log(nnzprobs)-log(bgdistr[klaster]))
		}
	mis<-mis*termscnt	
	
	#===================================find the first feature .===============================
	pickedterm<-which.max(mis)
	selectedorder[1]<-pickedterm
	selected[pickedterm]<-TRUE
	
	predistr<-trainmtx[,pickedterm] #previous overall distribution
	precnts<-termscnt[pickedterm] #previous overall weight
	
	#===================================find the rest of features .===============================
	for (k in 2:topk)
		{		
		unselected<-which(selected==FALSE)
		lengthunselected<-length(unselected)

		overallentropy<-vector('numeric',length(unselected) )
		.Call("get_overallentropy",  nCLASS,  trainmtx,  unselected,  lengthunselected,  termscnt,  predistr,  precnts, totaltermscnt, overallentropy)
		
		pickidx_in_unselected<-which.max(overallentropy-weighted_termsentropy[unselected])
		pickedterm<-unselected[pickidx_in_unselected]
		selectedorder[k]<-pickedterm
		selected[pickedterm]<-TRUE
		
		predistr<-trainmtx[,pickedterm]+predistr 
		precnts<-termscnt[pickedterm]+precnts
		}
	
	selectedterms<-colnames(trainmtx)[selected]
	return(selectedterms)
	}
	
	
