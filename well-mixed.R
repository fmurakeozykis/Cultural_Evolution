source("C:/Users/fmura/Documents/groningen/New folder/all_parameters.R")


##pick one random inside the loop 
##put code up on github


cols <- c("individual", "trait")
properties_2 <- data.frame(
  individual = m,
  trait = rep("resident", length(m))
)

vary_migration <- function (migrate, random, properties_2, m, samplesize){
  for(x in migrate){ 
    #randomly select given # of individuals from the population, place in vector
    random <- sample(1:length(m), size = samplesize, replace = F)
    
    interaction_1 <- function (random, properties_2, m, x) {
      for (i in random) {
        migrate_now <- rbinom(1, size = 1, prob = x)
        if (migrate_now == 1){ 
          properties_2$trait[properties_2$individual == i] <- "immigrant"
          cat(i, "replaced by immigrant\n")
          
        } else {
          partners <- m[m!=i]
          s <- sample(partners, 1, F)
          cat("current individual:", i, "\n")
          cat("partner:", s, "\n")
          
          if (properties_2$trait[properties_2$individual == i] == "resident"){ 
            if (properties_2$trait[properties_2$individual == s] == "resident"){
              interact <- rbinom(1, size = 1, prob = rwithr)
              cat("both residents\n")
              if (interact == 1) {
                take_over <- rbinom(1, size = 1, prob = c_r)
                cat("they were social\n")
                if (take_over == 1){
                  properties_2$trait[properties_2$individual == i] <- properties_2$trait[properties_2$individual == s]
                  cat("focal took over partner's culture\n")
                } else {cat("focal conserved their culture\n")}
              } else {cat("they were antisocial\n")}
            } else {
              interact <- rbinom(1, size = 1, prob = rwithi)
              cat("focal = resident, partner = immigrant\n")
              if (interact == 1) {
                take_over <- rbinom(1, size = 1, prob = c_r)
                cat("they were social\n")
                if (take_over == 1){
                  properties_2$trait[properties_2$individual == i] <- properties_2$trait[properties_2$individual == s]
                  cat("focal took over partner's culture\n")
                } else {cat("focal conserved their culture\n")}
              } else {cat("they were antisocial\n")}
            }
          } 
          
          else {
            if (properties_2$trait[properties_2$individual == s] == "resident"){
              interact <- rbinom(1, size = 1, prob = iwithr)
              cat("both residents\n")
              if (interact == 1) {
                take_over <- rbinom(1, size = 1, prob = c_i)
                cat("they were social\n")
                if (take_over == 1){
                  properties_2$trait[properties_2$individual == i] <- properties_2$trait[properties_2$individual == s]
                  cat("focal took over partner's culture\n")
                } else {cat("focal conserved their culture\n")}
              } else {cat("they were antisocial\n")}
            } else {
              interact <- rbinom(1, size = 1, prob = iwithi)
              cat("focal = immigrant, partner = immigrant\n")
              if (interact == 1) {
                take_over <- rbinom(1, size = 1, prob = c_i)
                cat("they were social\n")
                if (take_over == 1){
                  properties_2$trait[properties_2$individual == i] <- properties_2$trait[properties_2$individual == s]
                  cat("focal took over partner's culture\n")
                } else {cat("focal conserved their culture\n")}
              } else {cat("they were antisocial\n")}
            }
          }
        }
      }
      return(properties_2)
    }
    
    cat("\n", "with migration rate", x, "\n")
    properties_2 <- interaction_1(random, properties_2, m, x)
    
  }
  return(properties_2)
}


properties_2 <- vary_migration(migrate, NULL, properties_2, m, samplesize)


