###########################################################
# MakeTreeDiagram
#
# Date   : 2013/10/28
# Author : FEG
# Version: 0.0.1
# Update : 
# TODO   : 
###########################################################


###########################################################
# Args
# 
#  tmp.rpart.tree    : a fitted object of class "rpart" (2-class classification tree)
#  tmp.rpart.outfile : output csv file  (Default is "rpart_tree_out.csv")
# 
###########################################################



MakeTreeDiagram <- function(tmp.rpart.tree, tmp.rpart.outfile = "rpart_tree_out.csv"){
  if (!inherits(tmp.rpart.tree, "rpart"))
    stop("Not a legitimate \"rpart\" object")
  
  sink("tmp_rpart_tree_out.txt")
  print(tmp.rpart.tree)
  sink()
  
  tmp.rpart.00 <- read.table("tmp_rpart_tree_out.txt", header=FALSE, sep="|", col.names="ch", stringsAsFactors=FALSE)
  
  tmp.rpart.01 <- tmp.rpart.00[regexpr("[0-9]+)", tmp.rpart.00$ch) > 0, , drop=FALSE]
  colnames(tmp.rpart.01) <- c("ch")
  rownames(tmp.rpart.01) <- 1:nrow(tmp.rpart.01)
  
  # The total number of nodes.
  tmp.rpart.nrow <- nrow(tmp.rpart.01)
  
  
  tmp.rpart.02 <- strsplit(tmp.rpart.01$ch, " ")
  
  tmp.rpart.03 <- lapply(tmp.rpart.02, function(v) {v[v != ""]})
  
  tmp.rpart.03 <- lapply(tmp.rpart.03,
                         function(v){if(v[length(v)] == "*")
                           return(v)
                           else
                             return(c(v, "-"))})
  
  
  tmp.rpart.03.row.len<- vector()
  for (i in 1:length(tmp.rpart.03)){
    tmp.rpart.03.row.len[i] <- length(tmp.rpart.03[[i]])
  }
  
  # terminal node
  tmp.rpart.terminal <- vector()
  for (i in 1:length(tmp.rpart.03)){
    tmp.rpart.terminal[i] <- ifelse(tmp.rpart.03[[i]][tmp.rpart.03.row.len[i]] == "*", TRUE, FALSE)
  }
  
  # category of the target variable
  tmp.classv <- vector()
  for (i in 1:length(tmp.rpart.03)){
    tmp.classv[i] <- tmp.rpart.03[[i]][tmp.rpart.03.row.len[i] - 3]
  }
  
  
  tmp.tgt <- as.data.frame(matrix(0, length(tmp.rpart.03), 3))
  colnames(tmp.tgt) <- c("n", "sum", "mean")
  
  tmp.tgt[, "n"]    <- unlist(lapply(tmp.rpart.03,
                                     function(v){as.numeric(v[length(v) - 5])}))
  sum.pre       <- unlist(lapply(tmp.rpart.03,
                                 function(v){as.numeric(v[length(v) - 4])}))
  tmp.tgt[, "sum"]  <- ifelse(tmp.classv == tmp.classv[1], sum.pre, tmp.tgt[, "n"] - sum.pre)
  
  tmp.tgt[, "mean"] <- tmp.tgt[, "sum"] / tmp.tgt[, "n"]
  
  # split conditions
  tmp.split.cond <- vector()
  for (i in 1:length(tmp.rpart.03)){
    tmp.cond.rnum <- length(tmp.rpart.03[[i]]) - 6
    tmp.split.cond[i] <- ""
    for (j in 2:tmp.cond.rnum){
      tmp.split.cond[i] <- paste(tmp.split.cond[i], tmp.rpart.03[[i]][j], sep="")
    }
  }
  
  
  # node id of the rpart object
  tmp.node.rpart.id <- unlist(lapply(tmp.rpart.03,
                                     function(v) {as.numeric(sub("[^0-9]", "", v[1]))}))
  
  # node depth (stratum)　root : 1 
  tmp.rpart.st1 <- floor(log2(tmp.node.rpart.id) + 1)
  
  tmp.max.strat <- max(tmp.rpart.st1)
  
  
  
  # node path
  tmp.rpart.node.path <- matrix(0, nrow=tmp.rpart.nrow, ncol=tmp.max.strat)
  tmp.rpart.node.path[1,1] <- 1
  for (i in 2:tmp.rpart.nrow){
    for(j in 1:(tmp.rpart.st1[i] - 1)){
      tmp.rpart.node.path[i, j] <- tmp.rpart.node.path[i - 1, j]
    }
    tmp.rpart.node.path[i, tmp.rpart.st1[i]] <- i
  }
  
  # parent node
  tmp.rpart.node.parent <- matrix(0, nrow = tmp.rpart.nrow, ncol = 2)
  colnames(tmp.rpart.node.parent) <- c("node.rno", "parent.node.rno")
  tmp.rpart.node.parent[1, 1] <- 1
  for (i in 2:tmp.rpart.nrow){
    tmp.rpart.node.parent[i, 1] <- i
    tmp.rpart.node.parent[i, 2] <- tmp.rpart.node.path[i, tmp.rpart.st1[i] - 1]
  }
  
  tmp.tgt.mean <- tmp.tgt[["mean"]]
  tmp.rpart.node.parent2 <- cbind(tmp.rpart.node.parent, tmp.tgt.mean, tmp.rpart.st1, tmp.rpart.terminal)
  
  # sort
  tmp.rpart.node.parent3 <- tmp.rpart.node.parent2[order(tmp.rpart.node.parent2[, "parent.node.rno"], tmp.rpart.node.parent2[, "tmp.tgt.mean"]), ]
  
  
  # representing order
  
  node.ud.cd1 <- c(c("b"), rep(c("c", "a"), ((tmp.rpart.nrow - 1) / 2)) )
  
  node.ud.cd1.ndord <- node.ud.cd1[order(tmp.rpart.node.parent3[, "node.rno"])]
  
  tmp.rpart.node.path.ud <- matrix("b",nrow=tmp.rpart.nrow,ncol=tmp.max.strat)
  for (i in 2:tmp.rpart.nrow){
    for(j in 1:(tmp.rpart.st1[i])){
      tmp.path.node <- tmp.rpart.node.path[i, j]
      tmp.path.node.ud <- node.ud.cd1.ndord[tmp.path.node]
      tmp.rpart.node.path.ud[i,j] <- tmp.path.node.ud
    }
  }
  
  tmp.rpart.node.path.ud1 <- vector(length=tmp.rpart.nrow)
  for (i in 1:tmp.rpart.nrow){
    tmp.rpart.node.path.ud1[i] <- ""
    for(j in 1:tmp.max.strat){
      tmp.rpart.node.path.ud1[i] <- paste(tmp.rpart.node.path.ud1[i], tmp.rpart.node.path.ud[i,j], sep="")
    }
  }
  
  
  
  tmp.rpart.node.rno <- c(1:tmp.rpart.nrow)
  
  tmp.rpart.node.rno.ord.asc <- tmp.rpart.node.rno[order(tmp.rpart.node.path.ud1)]
  
  tmp.rpart.node.rno.ord.des <- tmp.rpart.node.rno.ord.asc[c(tmp.rpart.nrow:1)]
  
  tmp.rpart.node.rno.ord.des.inv <- tmp.rpart.node.rno[order(tmp.rpart.node.rno.ord.des)]
  
  tmp.rpart.node.id0 <- matrix(0, nrow=tmp.rpart.nrow,ncol = tmp.max.strat)
  tmp.rpart.node.id0[1, tmp.rpart.st1[tmp.rpart.node.rno.ord.des[1]]] <- 1
  for (i in 2:tmp.rpart.nrow){
    for(j in 1:tmp.max.strat){
      tmp.rpart.node.id0[i, j] <- tmp.rpart.node.id0[i-1, j]
    }
    tmp.rpart.node.id0[i, tmp.rpart.st1[tmp.rpart.node.rno.ord.des[i]]] <-
      tmp.rpart.node.id0[i, tmp.rpart.st1[tmp.rpart.node.rno.ord.des[i]]] + 1
  }
  
  tmp.rpart.node.st.ord <- vector()
  for (i in 1:tmp.rpart.nrow){
    tmp.rpart.node.st.ord[i] <- tmp.rpart.node.id0[tmp.rpart.node.rno.ord.des.inv[i], tmp.rpart.st1[i]]
  }
  
  # NODE ID
  tmp.rpart.node.id02 <- matrix("", nrow=tmp.rpart.nrow,ncol = 4)
  colnames(tmp.rpart.node.id02) <- c("node.cid.1", "node.cid.2", "node.cid.3", "node.cid")
  for (i in 1:tmp.rpart.nrow){
    # T:terminal  N:internal
    ifelse(tmp.rpart.node.parent3[i, "tmp.rpart.terminal"] == TRUE,
           tmp.rpart.node.id02[i, "node.cid.1"] <- "T",
           tmp.rpart.node.id02[i, "node.cid.1"] <- "N")
    
    # stratum
    tmp.id.st <- tmp.rpart.node.parent3[i, "tmp.rpart.st1"] - 1
    tmp.id.st.c0 <- paste("00", as.character(tmp.id.st), sep="")
    tmp.id.st.c1 <- substr(tmp.id.st.c0, nchar(tmp.id.st.c0) - 1, nchar(tmp.id.st.c0))
    tmp.rpart.node.id02[i, "node.cid.2"] <- tmp.id.st.c1
    
    # number in stratum
    tmp.id.stno <- tmp.rpart.node.st.ord[tmp.rpart.node.parent3[i, "node.rno"]]
    tmp.id.stno.c0 <- paste("000", as.character(tmp.id.stno), sep="")
    tmp.id.stno.c1 <- substr(tmp.id.stno.c0, nchar(tmp.id.stno.c0) - 2, nchar(tmp.id.stno.c0))
    tmp.rpart.node.id02[i, "node.cid.3"] <- tmp.id.stno.c1
    
    # NODE ID
    tmp.rpart.node.id02[i, "node.cid"] <- paste(tmp.rpart.node.id02[i, "node.cid.1"],
                                                tmp.rpart.node.id02[i, "node.cid.2"],
                                                tmp.rpart.node.id02[i, "node.cid.3"],
                                                sep="")
  }
  
  tmp.rpart.node.cid <- tmp.rpart.node.id02[order(tmp.rpart.node.parent3[, "node.rno"]), "node.cid"]
  
  
  # preparing output variable
  tmp.tgt.n   <- tmp.tgt[["n"]]
  tmp.tgt.sum <- tmp.tgt[["sum"]]
  tmp.rpart.out0 <- data.frame(tmp.rpart.node.parent2, tmp.rpart.node.cid, tmp.node.rpart.id,
                               tmp.split.cond, tmp.rpart.node.path.ud1, tmp.tgt.n, tmp.tgt.sum, stringsAsFactors=FALSE)
  
  tmp.rpart.out0 <- transform( tmp.rpart.out0,
                               tmp.rpart.outv = 
                                 paste(tmp.rpart.node.cid,
                                       " ( N= ",
                                       format(tmp.tgt.n, big.mark = ",", trim=TRUE),
                                       ", P2= ",
                                       format(round(tmp.tgt.mean, digits=4)*100, trim=TRUE),
                                       "% ) ",
                                       tmp.split.cond,
                                       sep="")
                               , stringsAsFactors=FALSE
  )
  
  # sort
  tmp.rpart.out02 <- tmp.rpart.out0[order(tmp.rpart.out0$tmp.rpart.node.path.ud1), ]
  
  
  #  0：nothing
  #  1：terminal node
  #  2：internal node
  #  3: upper stratum of nodes (start)
  #  4: continuing　"│"
  #  5: upper stratum of nodes (end)
  
  tmp.rpart.out.m1 <- matrix(0, nrow = tmp.rpart.nrow, ncol = tmp.max.strat)
  # first line
  tmp.rpart.out.m1[1, (tmp.rpart.out02[["tmp.rpart.st1"]][1] - 1)] <-3
  tmp.rpart.out.m1[1, tmp.rpart.out02[["tmp.rpart.st1"]][1]] <- 1
  
  for (i in 2:tmp.rpart.nrow){
    for(j in 1:tmp.max.strat){
      # retain
      ifelse(any(tmp.rpart.out.m1[i-1, j] == c(0, 1, 5)), tmp.rpart.out.m1[i,j] <- 0, tmp.rpart.out.m1[i, j] <- 4)
      # process
      tmp.row.st <- tmp.rpart.out02[["tmp.rpart.st1"]][i]
      ifelse( tmp.rpart.out02[["tmp.rpart.terminal"]][i] == 1,
              tmp.rpart.out.m1[i, tmp.row.st] <- 1,
              tmp.rpart.out.m1[i, tmp.row.st] <- 2)
      ifelse( tmp.rpart.out.m1[i-1, tmp.row.st-1] == 0,
              tmp.rpart.out.m1[i, tmp.row.st-1] <- 3,
              tmp.rpart.out.m1[i, tmp.row.st-1] <- 5)
    }
  }
  
  
  tmp.rpart.out.m2 <- matrix("", nrow=tmp.rpart.nrow, ncol=tmp.max.strat)
  for (i in 1:tmp.rpart.nrow){
    for(j in 1:tmp.max.strat){
      if      (tmp.rpart.out.m1[i, j] == 1){
        tmp.rpart.out.m2[i, j] <- paste("-", tmp.rpart.out02[["tmp.rpart.outv"]][i], sep="")
      }
      else if (tmp.rpart.out.m1[i, j] == 2){
        tmp.rpart.out.m2[i, j] <- paste("┤", tmp.rpart.out02[["tmp.rpart.outv"]][i], sep="")
      }
      else if (tmp.rpart.out.m1[i, j] == 3){
        tmp.rpart.out.m2[i, j] <- "┌─────────────────────────────"
      }
      else if (tmp.rpart.out.m1[i, j] == 4){
        tmp.rpart.out.m2[i, j] <- "│"
      }
      else if (tmp.rpart.out.m1[i, j] == 5){
        tmp.rpart.out.m2[i, j] <- "└─────────────────────────────"
      }
      
    }
  }
  
  tmp.rpart.out.m2.colnm.pre <- c(0:(tmp.max.strat - 1))
  tmp.rpart.out.m2.colnm <- c(paste("st", tmp.rpart.out.m2.colnm.pre, sep = ""))
  colnames(tmp.rpart.out.m2) <- tmp.rpart.out.m2.colnm
  
  tmp.rpart.out.m3pre <- data.frame(tmp.rpart.out02[["tmp.rpart.node.cid"]],
                                    tmp.rpart.out02[["tmp.tgt.n"]],
                                    tmp.rpart.out02[["tmp.tgt.sum"]],
                                    tmp.rpart.out02[["tmp.tgt.mean"]],
                                    tmp.rpart.out02[["tmp.rpart.node.cid"]],
                                    tmp.rpart.out02[["tmp.tgt.n"]],
                                    tmp.rpart.out02[["tmp.tgt.sum"]],
                                    tmp.rpart.out02[["tmp.tgt.mean"]],
                                    tmp.rpart.out02[["tmp.split.cond"]],
                                    tmp.rpart.out02[["tmp.node.rpart.id"]],
                                    tmp.rpart.out02[["node.rno"]],
                                    stringsAsFactors=FALSE)
  colnames(tmp.rpart.out.m3pre) <- c("node.cid",     "tgt.n",     "tgt.sum",     "tgt.mean", 
                                     "all.node.cid", "all.tgt.n", "all.tgt.sum", "all.tgt.mean",
                                     "node.split.cond", "node.rpart.id", "node.rpart.rowno")
  
  tmp.rpart.out.m3 <- data.frame(tmp.rpart.out.m2,
                                 tmp.rpart.out.m3pre,
                                 stringsAsFactors=FALSE)
  
  
  tmp.rpart.out.tree.diagram <- tmp.rpart.out.m3
  for (i in 1:tmp.rpart.nrow){
    if (substr(tmp.rpart.out.tree.diagram$node.cid[i], 1, 1) == "N"){
      tmp.rpart.out.tree.diagram$node.cid[i] <- ""
      tmp.rpart.out.tree.diagram$tgt.n[i]    <- ""
      tmp.rpart.out.tree.diagram$tgt.sum[i]  <- ""
      tmp.rpart.out.tree.diagram$tgt.mean[i] <- ""
    }
  }  
  
  # output csv
  write.csv(tmp.rpart.out.tree.diagram, file = tmp.rpart.outfile ,row.names = FALSE)
  
  # return data
  invisible(tmp.rpart.out.tree.diagram)
}



