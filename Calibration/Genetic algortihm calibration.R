#using the nlrx package
library(nlrx)
?simdesign_GenAlg
?simdesign_GenSA


nl <- nl(nlversion = "6.1.1",
         nlpath = "/Users/daniel/Desktop/NetLogo 6.1.1",
         modelpath = "/Users/daniel/OneDrive - University of Warwick/Dissertation/final model.nlogo")

?experiment

#numbers for north region
nl@experiment <- experiment(expname="genalg_diss_1.txt",
                            outpath = "/Users/daniel/OneDrive - University of Warwick/Dissertation/diss analysis/",
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup",
                            idgo="go",
                            idfinal=NA_character_,
                            idrunnum=NA_character_,
                            runtime=0,
                            evalticks=seq(52, 200),
                            stopcond="not any? turtles",
                            
                            metrics = c("unem-rate",
                                        "ltu-rate",
                                        "theta"),
                            
                            variables =  list("base-search-units" = 
                                                list(min=3, max=8, qfun="qunif"),
                                              "growth-rate" = 
                                                list(min=0, max=0.1, qfun="qunif"),
                                              "efficiency" =
                                                list(min=0.1, max=0.5, qfun="qunif"),
                                              "benefits" =
                                                list(min = 0.2, max = 1, qfun="qunif"),
                                              "shock" = 
                                                list(min=0,max=0.05, qfun="qunif"),
                                              "prd-comp" =
                                                list(min=0, max=2, qfun="qunif")),
                            
                            constants = list("platform" = "\"off\"",
  
                                             "number_of_job_seekers" = 600,
                                             "number_of_vacancies" = 550,
                                             "number_of_firms" = 250,
                                             "max_vacancies" = 3,
                                             
                                             "minimum-wage" = 1,
                                             "rc-cost" = 1,
                                             
                                             "search-bonus" = 4,
                                             "ltu-search-minus" = 4))

params <- report_model_parameters(nl)

unem_rate_target <- 0.16
ltu_rate_target <- 0.12
theta_target <- 0.11


?mean
critfun <- function(nl) {
  results <- nl@simdesign@simoutput
  
  unem_rate <- mean(results$"unem-rate")
  ltu_rate <- mean(results$"ltu-rate")
  theta <- mean(results$"theta")
  
  cond1 <- (unem_rate - unem_rate_target)^2
  cond2 <- (ltu_rate - ltu_rate_target)^2
  cond3 <- (theta - theta_target)^2
  crit <- cond1 + cond2 + cond3
  return(crit)
}

?simdesign_lhs
nl@simdesign <- simdesign_lhs(nl=nl, samples=5, nseeds=1,precision=3)


nl@simdesign <- simdesign_GenAlg(nl = nl,
                                 popSize = 5, 
                                 iters = 1, 
                                 evalcrit = critfun,
                                 elitism = NA,
                                 mutationChance = NA,
                                 nseeds = 1)
print(nl)
results <- run_nl_dyn(nl = nl,
                      seed = 44)

setsim(nl, "simoutput") <- results

pmarg <- 10

library(ggplot2)
fitness.plot <- data.frame(generation=seq(1:length(nl@simdesign@simoutput$mean)), evaluation=nl@simdesign@simoutput$mean)
print(fitness)
fitness.plot

fitness.plot2 <- ggplot(fitness, aes(x=generation, y=evaluation)) +
  geom_line(size=1) +
  #theme_ipsum(base_size = 14, axis_text_size = 14, axis_title_size = 14, strip_text_size = 14) +
  theme(plot.margin = margin(t = pmarg, r = pmarg, b = pmarg, l = pmarg, unit = "pt")) +
  ylab("evaluation [ticks]")
fitness.plot2

library(dplyr)
results.summary <- tibble(parameter = names(nl@experiment@variables),
                          value = round(nl@simdesign@simoutput$population[nrow(nl@simdesign@simoutput$population), ], digits = 2)) %>% 
  mutate(value=c(floor(value[1]), value[2:6]))
results.summary

library(grid)
library(gridExtra)
fitness.best.table <- tableGrob(results.summary, rows=rep("",nrow(results.summary)))
fitness.plot3 <- fitness.plot + annotation_custom(fitness.best.table, xmin=50, ymin=2000)
fitness.plot3

setsim(nl, "simoutput") <- results
print(mean(nl@simdesign@simoutput$"ltu-rate"))
results_unnest <- unnest_simoutput(nl)
