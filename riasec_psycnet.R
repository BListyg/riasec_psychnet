library(reshape2)
library(dplyr)
library(glasso)
library(psych)
library(cowplot)
library(qgraph)

riasec_data <- data.frame(read.csv('https://raw.githubusercontent.com/BListyg/Vocational-Interests-Data/master/data.csv',header=T))[,c(2:49)]

riasec_data[riasec_data == -1] <- NA

R <- riasec_data[,c(1:8)]
I <- riasec_data[,c(9:16)]
A <- riasec_data[,c(17:24)]
S <- riasec_data[,c(25:32)]
E <- riasec_data[,c(33:40)]
C <- riasec_data[,c(41:48)]

################################################################# #########################Setting up data for graphing################## #################################################################
#colnames(JDIW2009, do.NULL=TRUE) #Reads you the column names so you can verify that this is what you want your nodes to be named
riasec_col<-colnames(riasec_data, do.NULL=TRUE) #Name the nodes based on their column names
RIASECcors<-cor_auto(riasec_data, detectOrdinal=FALSE) #Create correlation matrix
RIASEC_C<-EBICglasso(RIASECcors, n=nrow(riasec_data)) #Create Gaussian graphical model using graphical lasso based on extended BIC criterium
#Create graph structure; telling R how many groups and which columns/nodes belong to which groups.

riasec_groups <-structure(
  list(
    Realistic=c(1:8), 
    Investigative=c(9:16), 
    Artistic=c(17:24), 
    Social=c(25:32), 
    Enterprising=c(33:40),
    Conventional = c(41:48)),
  Names=c("Realistic","Investigative", "Artistic", "Social","Enterprising","Conventional"))

RIASEC_Graph<-qgraph(RIASEC_C, layout="spring", vsize=3, esize=10, labels=riasec_col, label.cex=4, groups=riasec_groups) #Graphing the network. Spring indicates fructerman- reingold algorithm. vsize indicates the size of the node. esize indicates the size of the largest edge. label.cex indicates the scale of the label size of the nodes.

title(main = paste("Psychometric Network Visualization\nHolland's Vocational Interest Data\nn =",nrow(riasec_data),sep = " "))

#R#

par(mfrow=c(3,3))

#colnames(JDIW2009, do.NULL=TRUE) #Reads you the column names so you can verify that this is what you want your nodes to be named
riasec_col<-colnames(R, do.NULL=TRUE) #Name the nodes based on their column names
RIASECcors<-cor_auto(R, detectOrdinal=FALSE) #Create correlation matrix
RIASEC_R<-EBICglasso(RIASECcors, n=nrow(riasec_data)) #Create Gaussian graphical model using graphical lasso based on extended BIC criterium
#Create graph structure; telling R how many groups and which columns/nodes belong to which groups.

riasec_groups <-structure(
  list(
    Realistic=c(1:8)), 
  Names=c("Realistic"))

RIASEC_Graph<-qgraph(RIASEC_R, layout="spring", vsize=3, esize=10, labels=riasec_col, label.cex=4, groups=riasec_groups, layout = layout_in_circle) #Graphing the network. Spring indicates fructerman- reingold algorithm. vsize indicates the size of the node. esize indicates the size of the largest edge. label.cex indicates the scale of the label size of the nodes.

title(main = paste("Psychometric Network Visualization\nHolland's Realistic Interest Data\nn =",nrow(R),sep = " "))

#I#

#colnames(JDIW2009, do.NULL=TRUE) #Reads you the column names so you can verify that this is what you want your nodes to be named
riasec_col<-colnames(I, do.NULL=TRUE) #Name the nodes based on their column names
RIASECcors<-cor_auto(I, detectOrdinal=FALSE) #Create correlation matrix
RIASEC_I<-EBICglasso(RIASECcors, n=nrow(riasec_data)) #Create Gaussian graphical model using graphical lasso based on extended BIC criterium
#Create graph structure; telling R how many groups and which columns/nodes belong to which groups.

riasec_groups <-structure(
  list(
    Investigative=c(1:8)), 
  Names=c("Investigative"))

RIASEC_Graph<-qgraph(RIASEC_I, layout="spring", vsize=3, esize=10, labels=riasec_col, label.cex=4, groups=riasec_groups, layout = layout_in_circle) #Graphing the network. Spring indicates fructerman- reingold algorithm. vsize indicates the size of the node. esize indicates the size of the largest edge. label.cex indicates the scale of the label size of the nodes.

title(main = paste("Psychometric Network Visualization\nHolland's Investigative Interest Data\nn =",nrow(I),sep = " "))

#A

#colnames(JDIW2009, do.NULL=TRUE) #Reads you the column names so you can verify that this is what you want your nodes to be named
riasec_col<-colnames(A, do.NULL=TRUE) #Name the nodes based on their column names
RIASECcors<-cor_auto(A, detectOrdinal=FALSE) #Create correlation matrix
RIASEC_A<-EBICglasso(RIASECcors, n=nrow(riasec_data)) #Create Gaussian graphical model using graphical lasso based on extended BIC criterium
#Create graph structure; telling R how many groups and which columns/nodes belong to which groups.

riasec_groups <-structure(
  list(
    Artistic=c(1:8)), 
  Names=c("Artistic"))

RIASEC_Graph<-qgraph(RIASEC_A, layout="spring", vsize=3, esize=10, labels=riasec_col, label.cex=4, groups=riasec_groups, layout = layout_in_circle) #Graphing the network. Spring indicates fructerman- reingold algorithm. vsize indicates the size of the node. esize indicates the size of the largest edge. label.cex indicates the scale of the label size of the nodes.

title(main = paste("Psychometric Network Visualization\nHolland's Artistic Interest Data\nn =",nrow(A),sep = " "))

#S

#colnames(JDIW2009, do.NULL=TRUE) #Reads you the column names so you can verify that this is what you want your nodes to be named
riasec_col<-colnames(S, do.NULL=TRUE) #Name the nodes based on their column names
RIASECcors<-cor_auto(S, detectOrdinal=FALSE) #Create correlation matrix
RIASEC_S<-EBICglasso(RIASECcors, n=nrow(riasec_data)) #Create Gaussian graphical model using graphical lasso based on extended BIC criterium
#Create graph structure; telling R how many groups and which columns/nodes belong to which groups.

riasec_groups <-structure(
  list(
    Social=c(1:8)), 
  Names=c("Social"))

RIASEC_Graph<-qgraph(RIASEC_S, layout="spring", vsize=3, esize=10, labels=riasec_col, label.cex=4, groups=riasec_groups, layout = layout_in_circle) #Graphing the network. Spring indicates fructerman- reingold algorithm. vsize indicates the size of the node. esize indicates the size of the largest edge. label.cex indicates the scale of the label size of the nodes.

title(main = paste("Psychometric Network Visualization\nHolland's Social Interest Data\nn =",nrow(A),sep = " "))

#E

#colnames(JDIW2009, do.NULL=TRUE) #Reads you the column names so you can verify that this is what you want your nodes to be named
riasec_col<-colnames(E, do.NULL=TRUE) #Name the nodes based on their column names
RIASECcors<-cor_auto(E, detectOrdinal=FALSE) #Create correlation matrix
RIASEC_E<-EBICglasso(RIASECcors, n=nrow(riasec_data)) #Create Gaussian graphical model using graphical lasso based on extended BIC criterium
#Create graph structure; telling R how many groups and which columns/nodes belong to which groups.

riasec_groups <-structure(
  list(
    Enterprising=c(1:8)), 
  Names=c("Enterprising"))

RIASEC_Graph<-qgraph(RIASEC_E, layout="spring", vsize=3, esize=10, labels=riasec_col, label.cex=4, groups=riasec_groups, layout = layout_in_circle) #Graphing the network. Spring indicates fructerman- reingold algorithm. vsize indicates the size of the node. esize indicates the size of the largest edge. label.cex indicates the scale of the label size of the nodes.

title(main = paste("Psychometric Network Visualization\nHolland's Enterprising Interest Data\nn =",nrow(A),sep = " "))

#C

#colnames(JDIW2009, do.NULL=TRUE) #Reads you the column names so you can verify that this is what you want your nodes to be named
riasec_col<-colnames(C, do.NULL=TRUE) #Name the nodes based on their column names
RIASECcors<-cor_auto(C, detectOrdinal=FALSE) #Create correlation matrix
RIASEC_C<-EBICglasso(RIASECcors, n=nrow(riasec_data)) #Create Gaussian graphical model using graphical lasso based on extended BIC criterium
#Create graph structure; telling R how many groups and which columns/nodes belong to which groups.

riasec_groups <-structure(
  list(
    Conventional=c(1:8)), 
  Names=c("Conventional"))

RIASEC_Graph<-qgraph(RIASEC_C, layout="spring", vsize=3, esize=10, labels=riasec_col, label.cex=4, groups=riasec_groups, layout = layout_in_circle) #Graphing the network. Spring indicates fructerman- reingold algorithm. vsize indicates the size of the node. esize indicates the size of the largest edge. label.cex indicates the scale of the label size of the nodes.

title(main = paste("Psychometric Network Visualization\nHolland's Conventional Interest Data\nn =",nrow(A),sep = " "))
