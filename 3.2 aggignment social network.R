## Use the attached twitter data in r data format and do as follows:
  
# 1. Load the attached "termDocMatrix.rdata" file in R/R studio
load(file="E:\\Milan\\MDS\\R by Sital Bhandary\\assignmnt\\Milan Paudel - termDocMatrix.rdata")

# 2. Check the structure of this data and comment on it carefully
str(termDocMatrix)

# 3. Inspect the first 10 rows and first 10 columns of this data using sub-setting
termDocMatrix[1:10,1:10]


# 4. Convert the loaded termDocMatrix data to as matrix with appropriate R code
matrixA<- as.matrix(termDocMatrix)

# 5. Change this matrix to a Boolean matrix with same name (Hint: [termDocMatrix >=1] < 1)
matrixA[termDocMatrix >=1] <- 1

# 6. Transform this matrix to a term-term adjacency matrix as termMatrix object (Hint: Use %*% and t(termDocMatrix)
termMatrix<- matrixA %*% t(termDocMatrix)

# 7. Inspect the first 10 rows and first 10 columns of this matrix using sub-setting
termMatrix[1:10,1:10]

# 8. Define a graph object g of termMatrix using graph.adjacency function of "igraph" package with weighted=T and mode="undirected" arguments in the function
library(igraph)
g<- graph.adjacency(termMatrix,weighted = TRUE,mode = "undirected")

# 9. Remove loops of g using simplify function
g<- simplify(g,remove.loops = TRUE)

# 10. Set vertices labels with names of g
V(g)$labels<- row.names(termMatrix)

# 11. Set degree with degree of g
V(g)$degree<- degree(g)

# 12. Find degree of g for 'all", "in" and "out" modes and interpret them carefully
degree(g,mode = "all")
degree(g,mode="in")
degree(g,mode="out")

#https://visiblenetworklabs.com/2021/04/16/understanding-network-centrality/

# 13. Find diameter of g and interpret it carefully
diameter(g)

# 14. Find edge density of g and interpret it carefully
edge_density(g)

# 15. Find reciprocity of g and interpret it carefully
reciprocity(g)

# 16. Find closeness of g and interpret it carefully
closeness(g)

# 17. Find betweeness of g and interpret it carefully
betweenness(g)

# 18. Plot histogram of node degree and interpret it carefully
hist(degree(g), main = "Histogram of Node Degree")

# 19. Set seed as per your class roll number
set.seed(18)

# 20. plot g with vertex.size of degree*0.4 and interpret it carefully
plot(g,vertex.size= degree(g)*0.4)

# 21. plot g with fruchterman.reingold layout and interpret it carefully
plot(g, layout=layout_with_fr)

# 22. plot g with kamada.kawai layout and interpret it carefully
plot(g, layout=layout_with_kk)

# 23. plot hub and authority of g and interpret them carefully
hub<- hub.score(g)
authority<- authority_score(g)
plot(hub$vector,main="Hub Vector of Graph g",type = "l",  ylab ="Hub" )
plot(authority$vector,main="Authority Vector of Graph g",type="l", ylab="Authority")

# 24. Plot community cluster of g and interpret it carefully
cnet <- cluster_edge_betweenness(g)
plot(cnet,g,vetex.size = 10, vertex.label.cex = 0.8)

# 25. Write a summary of SNA for the twitter data

