## Glasso example
install.packages('qgraph')
library(qgraph)
library("psych")
data(bfi)

# Compute correlations:
CorMat = cor_auto(bfi[,1:25])

# Compute graph with gamma = 0 (BIC):
BICgraph = EBICglasso(CorMat, nrow(bfi), 0) # 상관관계행렬을 input, gamma 가 0인경우는 그냥 BIC

# Compute graph with gamma = 0.5 (EBIC) BIC보다 더 쎈거.. 더 많이 잘라냄~
EBICgraph = EBICglasso(CorMat, nrow(bfi), 0.5) # 보통 gamma = 0.5로 줌 EBIC는..

# Plot both:
layout(t(1:2))
BIC = qgraph(BICgraph, layout = "spring", title = "BIC", details = TRUE)
EBIC = qgraph(EBICgraph, layout = "spring", title = "EBIC")

summary(BIC)
# Number of edges:	 224 
# Number of directed edges:	 0 
# Number of unique weights:	 224 

summary(EBIC)
# Number of edges:	 181 
# Number of directed edges:	 0 
# Number of unique weights:	 181 
# 엣지가 EBIC가 더 적음
?EBICglasso

# common edges
BIC.edge = paste(BIC$Edgelist$from, "-", BIC$Edgelist$to)
EBIC.edge = paste(EBIC$Edgelist$from, "-", EBIC$Edgelist$to)

setdiff(BIC.edge, EBIC.edge)
setdiff(EBIC.edge, BIC.edge) # EBIC가 BIC의 완전한 서브셋이다~
EBIC.edge
# http://web.stanford.edu/~hastie/ElemStatLearn/