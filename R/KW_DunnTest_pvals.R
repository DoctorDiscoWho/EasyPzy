#' StatTest and Posthoc P values of multiple interdependent variables in a
#' dataframe (based on Rstatix)
#' @param x - Your dataset
#' @param VC1_col - first grouping variable
#' @param VC2_col - second grouping variable (This is used as the factor in the test, column ~ VC2_col)
#' @param p.adj.method - method to adjust p values for multiple comparisons. Used when pairwise comparisons are performed. Allowed values include "holm", "hochberg", "hommel", "bonferroni", "BH", "BY", "fdr", "none". If you don't want to adjust the p value (not recommended), use p.adjust.method = "none".
#' @import dplyr
#' @import reshape2
#' @import rstatix
#' @import tidyverse
#' @description
#' Runs sttistical significance tests and its associated Posthoc and gives the
#' results in a dataframe. To use the package and the associated command add the
#'  write the command and add the data neccessary data set.The data set should
#'  have two variable/grouping columns, eg: Treatment and Sampling Day. Add your
#'  grouping variable to VC1_col eg: VC1_col = "Day" and variable to VC2_col
#'  eg: VC2_col = "Treatment" and what correction you want eg, holm, hummel,
#'  bonferr.. etc. . The package currently only has one command but I
#'  will update soon (If you need something urgent please write to me on the
#'  provided email)
#'
#' @returns Data frame of p values
#' @export
KW_DunnTest_pvals<- function(x, VC1_col, VC2_col, p.adj.method) {
  c1 = x[which(colnames(x) != VC1_col & colnames(x) != VC2_col)]
  c2 = x[[VC1_col]]
  c3 = x[[VC2_col]]
  n1<-paste(c("value"),as.character(VC1_col), sep = "~")
  n2<-paste(c("value"),as.character(VC2_col), sep = "~")
  y <- data.frame(cbind(c2, c3, c1))
  ym <- data.frame(reshape2::melt(y, na.rm = TRUE))

  Kruskal_result <- ym %>% group_by(c2, variable) %>% rstatix::kruskal_test(value ~
                                                                              c3)
  Kruskal_result2 <- cbind(
    Kruskal_result[,8],
    Kruskal_result[,1],
    Kruskal_result[,2],
    Kruskal_result[,7],
    c(n2)
  )
  Kruskal_result3 <- data.frame(
    dcast(
      Kruskal_result2,
      Kruskal_result2[,1] +
        Kruskal_result2[,2] +
        Kruskal_result2[,5] ~ Kruskal_result2[,3],
      value.var = "p"
    )
  )
  P_ADJ_Method<-c("")

  Kruskal_result4<-cbind(Kruskal_result3, P_ADJ_Method)

  Dunntest_result <- ym %>% group_by(c2, variable) %>% rstatix::dunn_test(value ~
                                                                            c3, p.adjust.method = p.adj.method)
  Dunntest_result <- Dunntest_result %>% mutate(Group = paste(group1, "-", group2))
  Dunntest_result2 <- cbind(
    Dunntest_result[,1],
    Dunntest_result[,2],
    Dunntest_result[,12],
    Dunntest_result[,10],
    c("DunnTest")
  )

  Dunntest_result3 <- data.frame(
    dcast(
      Dunntest_result2,
      Dunntest_result2[, 5] +
        Dunntest_result2[, 1] + Dunntest_result2[, 3] ~
        Dunntest_result2[, 2],
      value.var = "p.adj"
    )
  )
  P_ADJ_Method<-c(p.adj.method)

  Dunntest_result4<-cbind(Dunntest_result3, P_ADJ_Method)

  Kruskal_result4<-stats::setNames(Kruskal_result4, replace(names(Kruskal_result4), 1:3, c("Test", VC1_col, "Formula/Group")))
  Dunntest_result4<-stats::setNames(Dunntest_result4, replace(names(Dunntest_result4), 1:3, c("Test", VC1_col, "Formula/Group")))


  return(rbind(Kruskal_result4, Dunntest_result4))
}
