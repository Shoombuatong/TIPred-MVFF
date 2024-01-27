#######################################

Pos = read.fasta('Pos.fasta', seqtype="AA", as.string = TRUE)
Neg = read.fasta('Neg.fasta', seqtype="AA", as.string = TRUE)
Pos <- Pos[(sapply(Pos, protcheck))]
Neg <- Neg[(sapply(Neg, protcheck))]
length(Pos)
length(Neg)
DD = c(Pos, Neg)

##############################################
################ Feature extraction ##########
##############################################

AAC = t(sapply(DD, extractAAC))
Class = c(rep("Pos", times= length(Pos)), rep("Neg", times= length(Neg)))

DPC = t(sapply(DD, extractDC))
##TPC = t(sapply(DD, extractTC))
CTDC = t(sapply(DD, extractCTDC))
CTDD = t(sapply(DD, extractCTDD))
CTDT = t(sapply(DD, extractCTDT))
CTD = cbind(CTDC, CTDD, CTDT)

lamda = 1
PAAC1 <- matrix(nrow = length(DD), ncol = 20 + lamda)
for(i in 1 : length(DD)) { 
PAAC1[i, ] = extractPAAC(DD[[i]][1], lambda = lamda,  props = c("Hydrophobicity", "Hydrophilicity", "SideChainMass"))
}


lamda = 1
APAAC1 <- matrix(nrow = length(DD), ncol = 20 + 2* lamda)
for(i in 1 : length(DD)) { 
APAAC1[i, ] = extractAPAAC(DD[[i]][1], lambda = lamda)
}