source("C:/Users/fmura/Documents/groningen/New folder/network.R")
source("C:/Users/fmura/Documents/groningen/New folder/all_parameters.R")
library(igraph)
library(openxlsx)

#potential issues:
### it produces 1 list of random nodes, which is reused with every run
### change length to sample size

##remove same trait part

cols <- c("node", "trait", "connections")
properties_1 <- data.frame(
  node = V(g),
  trait = rep("resident", length(g)),
  connections = deg_seq_1
)
print(migrate)
vary_migration <- function (migrate, random, properties_1, g, length){
  for(x in migrate){ 
  #randomly select an individual from the network, place in vector
  random <- sample(1:length(g), size = length, replace = F)

   interaction_1 <- function (random, properties_1, g, x) {
  for (i in random) {
    migration <- rbinom(1, size = 1, prob = x)
      if (migration == 1){ 
        properties_1$trait[properties_1$node == i] <- "immigrant"
        cat(i, "replaced by immigrant\n")
      }
    else {
      connected_nodes <- neighbors(g, i)
      s <- sample(connected_nodes, 1, F)
      cat("current node:", i, "\n")
      cat("connections:", connected_nodes, "\n")
      cat("chosen:", s, "\n")
      if (properties_1$trait[properties_1$node == i] == "resident"){ 
        if (properties_1$trait[properties_1$node == s] == "resident"){
          interact <- rbinom(1, size = 1, prob = rwithr)
          cat("both residents\n")
          if (interact == 1) {
            take_over <- rbinom(1, size = 1, prob = c_r)
            cat("they were social\n")
            if (take_over == 1){
              properties_1$trait[properties_1$node == i] <- properties_1$trait[properties_1$node == s]
              cat("focal took over partner's culture\n")
            }
            else {cat("focal conserved their culture\n")}
          }
          else {cat("they were antisocial\n")}
        }
        else {
            interact <- rbinom(1, size = 1, prob = rwithi)
            cat("focal = resident, partner = immigrant\n")
            if (interact == 1) {
              take_over <- rbinom(1, size = 1, prob = c_r)
              cat("they were social\n")
              if (take_over == 1){
                properties_1$trait[properties_1$node == i] <- properties_1$trait[properties_1$node == s]
                cat("focal took over partner's culture\n")
              }
              else {cat("focal conserved their culture\n")}
            }
            else {cat("they were antisocial\n")}
        }
      }
      else {
          if (properties_1$trait[properties_1$node == s] == "resident"){
            interact <- rbinom(1, size = 1, prob = iwithr)
            cat("both residents\n")
            if (interact == 1) {
              take_over <- rbinom(1, size = 1, prob = c_i)
              cat("they were social\n")
              if (take_over == 1){
                properties_1$trait[properties_1$node == i] <- properties_1$trait[properties_1$node == s]
                cat("focal took over partner's culture\n")
              }
              else {cat("focal conserved their culture\n")}
            }
            else {cat("they were antisocial\n")}
          }
          else {
            interact <- rbinom(1, size = 1, prob = iwithi)
            cat("focal = immigrant, partner = immigrant\n")
            if (interact == 1) {
              take_over <- rbinom(1, size = 1, prob = c_i)
              cat("they were social\n")
              if (take_over == 1){
                properties_1$trait[properties_1$node == i] <- properties_1$trait[properties_1$node == s]
                cat("focal took over partner's culture\n")
              } else {cat("focal conserved their culture\n")}
            } else {cat("they were antisocial\n")}
          }
        }
      }
     } 
     return(properties_1)
    }
    
    cat("\n", "with migration rate", x, "\n")
    properties_1 <- interaction_1(random, properties_1, g, x)
    
  }
  return(properties_1)
}

properties_1 <- vary_migration(migrate, NULL, properties_1, g, samplesize)


#write.csv(properties_1, "C:\Users\fmura\Documents\groningen\New folder\test_output.xlsx")
#write.xlsx(properties_1, "C:/Users/fmura/Documents/groningen/New folder/test.xlsx", sheetName = "Sheet1", overwrite = TRUE)
#write.xlsx(graph, "C:/Users/fmura/Documents/groningen/New folder/plot.xlsx", sheetName = "Sheet2", overwrite = TRUE)


