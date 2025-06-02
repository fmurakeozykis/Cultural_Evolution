source("C:/Users/fmura/Downloads/all_parameters (1).R")

if (!requireNamespace("dplyr", T)) {install.packages("dplyr")}
if (!requireNamespace("tidyr", T)) {install.packages("tidyr")}
if (!requireNamespace("ggplot2", T)) {install.packages("ggplot2")}
if (!requireNamespace("stringr", T)) {install.packages("stringr")}
if (!requireNamespace("progress", T)) {install.packages("progress")}
if (!requireNamespace("beepr",T))    {install.packages("beepr")}
library(beepr)
library(dplyr)
library(tidyr)
library(ggplot2)
library(stringr)
library(progress)


# Create initial population
properties_2 <- data.frame(
  individual = m,
  trait = rep("resident", length(m))
)

vary_migration <- function(migrate, num_events, properties_2, m) {
  
  pb_c <- progress_bar$new(
    format = "[:bar] :percent ETA: :eta",
    total = length(c_i),
    clear = FALSE,
    width = 60
  )
  
  for (y in c_i){
    cat("\n...... Starting c_i:", y, "......\n")

    pb_migration <- progress_bar$new(
      format = "[:bar] :percent ETA: :eta",
      total = length(migrate),
      clear = FALSE,
      width = 60)
    
    for (x in migrate) {
      cat("\n--- Starting migration rate:", x, "---\n")
      pb_events <- progress_bar$new(
        format = "[:bar] :percent ETA: :eta",
        total = num_events,
        clear = FALSE,
        width = 60
      )

      
      for (event in 1:num_events) {

        
        # Pick a random individual
        i <- sample(m, 1)
        
        migrate_now <- rbinom(1, 1, x)
        if (migrate_now == 1) {
          properties_2$trait[properties_2$individual == i] <- "immigrant"
          # cat(i, "replaced by immigrant\n")
        } else {
          partners <- m[m != i]
          s <- sample(partners, 1)
          
          trait_i <- properties_2$trait[properties_2$individual == i]
          trait_s <- properties_2$trait[properties_2$individual == s]
          
         if (trait_i == "resident" && trait_s == "immigrant") {
            interact <- rbinom(1, 1, rwithi)
            if (interact == 1) {
              take_over <- rbinom(1, 1, 1 - c_r)
              if (take_over == 1) {
                properties_2$trait[properties_2$individual == i] <- trait_s
              }
            }
          } else if (trait_i == "immigrant" && trait_s == "resident") {
            interact <- rbinom(1, 1, rwithi)
            if (interact == 1) {
              take_over <- rbinom(1, 1, 1 - c_i)
              if (take_over == 1) {
                properties_2$trait[properties_2$individual == i] <- trait_s
              }
            }
          } 
        }
        pb_events$tick()
      }
      pb_migration$tick()
      
      
      migr <- paste("mig", x, "c", y, sep = "_")  # "mig_0.1_c_0.5"
      properties_2[[migr]] <- properties_2$trait
      
      cat("Saved traits for migration rate:", x, "\n")
      properties_2$trait <- rep("resident", length(m))
     
      trait_counts <- table(properties_1$trait)
      if (any(trait_counts == 0)) {
        fixation <- event
        cat("Fixation reached at event", fixation, "\n")
        break
      }
      
      
    }
    pb_c$tick()   
    cat("Saved traits for c_i:", y, "\n")
   
  }
    
    return(properties_2)
}

system.time({
properties_2 <- vary_migration(migrate, num_events, properties_2, m)})
beep()
write.csv(properties_2, "wm_2x2_3_c_rreally01.csv", row.names = FALSE)
  
#----------------------------------------------------------------------
## PLOTS ##
#----------------------------------------------------------------------
trait_counts <- wm_2x2_3_c_rreally01 %>%
  select(starts_with("mig_")) %>%
  pivot_longer(
    cols = everything(),
    names_to = "param_combo",
    values_to = "trait"
  ) %>%
  separate(param_combo, into = c("mig", "c"), sep = "_c_") %>%
  mutate(
    mig = as.numeric(str_remove(mig, "mig_")),
    c = as.numeric(str_remove(c, "c_"))
  )%>%
  group_by(mig, c, trait) %>%
  summarise(count = n(), .groups = "drop")

trait_props <- trait_counts %>%
  group_by(mig, c) %>%
  mutate(total = sum(count)) %>%  # total should be n, sanity check
  ungroup() %>%
  filter(trait == "immigrant") %>%
  mutate(prop = count / total)

ggplot(trait_counts_prop %>% filter(trait == "immigrant"), 
       aes(x = mig, y = c, fill = prop)) +
  geom_tile() +
  scale_fill_gradient(low = "darkblue", high = "lightblue") +
  labs(title = "Proportion of Immigrants by Migration Rate and Conservatism",
       fill = "Proportion Immigrant")

ggplot(trait_props, aes(x = mig, y = c, fill = prop)) +
  geom_tile() +
  scale_fill_gradient(low = "darkblue", high = "yellow", limits = c(0,1)) +
  labs(title = "Proportion of Immigrants in Population",
       x = "Migration rate",
       y = "Immigrant conservatism",
       fill = "Proportion\nImmigrants") +
  theme_minimal()

trait_props_complete <- all_combinations %>%
  left_join(trait_props, by = c("mig", "c")) %>%
  mutate(prop = ifelse(is.na(prop), 0, prop))

ggplot(trait_props_complete, aes(x = mig, y = c, fill = prop)) +
  geom_tile() +
  scale_fill_gradient(low = "darkblue", high = "yellow", limits = c(0,1)) +
  labs(title = "Proportion of Immigrants in Population",
       x = "Migration rate",
       y = "Immigrant conservatism",
       fill = "Proportion\nImmigrants") +
  theme_minimal()

ggplot(trait_props_complete, aes(x = mig, y = c, fill = prop)) +
  geom_raster() +
  scale_fill_gradientn(
    colours = c("darkblue", "yellow"),
    values = scales::rescale(c(0, 1)),
    limits = c(0, 1),
    na.value = "white"
  ) +
  labs(
    title = "Proportion of Immigrants in Population",
    x = "Migration rate",
    y = "Immigrant conservatism",
    fill = "Proportion\nImmigrants"
  ) +
  theme_minimal()



