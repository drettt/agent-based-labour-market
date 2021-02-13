library(nlrx)
library(future)
library(dplyr)

nl <- nl(nlversion = "6.1.1",
         nlpath = "/Users/daniel/Desktop/NetLogo 6.1.1",
         modelpath = "/Users/daniel/OneDrive - University of Warwick/Dissertation/final model.nlogo")

#no variables
#constants for north
nl@experiment <- experiment(expname="diss_sim_north.txt",
                            outpath = "/Users/daniel/OneDrive - University of Warwick/Dissertation/diss analysis/",
                            repetition=1,
                            tickmetrics="true",
                            idsetup="setup",
                            idgo="go",
                            idfinal=NA_character_,
                            idrunnum=NA_character_,
                            runtime=0,
                            evalticks = seq(1,208),
                            stopcond="not any? turtles",
                            
                            metrics = c("unem-rate",
                                        "ltu-rate",
                                        "theta",
                                        "mismatch",
                                        "dur",
                                        "users.dur",
                                        "non.users.dur",
                                        "wage"
                                        
                                        ),
                            constants = list("platform" = "\"on\"",
                                             
                                             "number_of_job_seekers" = 600,
                                             "number_of_vacancies" = 550,
                                             "number_of_firms" = 220,
                                             "max_vacancies" = 3,
                                             
                                             "minimum-wage" = 1,
                                             "rc-cost" = 1,
                                             
                                             "search-bonus" = 3, #4,
                                             "ltu-search-minus" = 4,
                                             "base-search-units"= 4,
                                             "growth-rate"= 0.0778, #0.0246
                                             "efficiency" =0.466, # 0.321,
                                             "benefits" = 0.314, #0.613,
                                             "shock" = 0.00336, #0.00175,
                                             "prd-comp" = 0.291)) #1.59
aa <- report_model_parameters(nl)

nl@simdesign <- simdesign_simple(nl=nl, nseeds = 50)
plan(multisession)
results <- run_nl_all(nl=nl, silent=FALSE)

setsim(nl, "simoutput") <- results
saveRDS(nl, file.path(nl@experiment@outpath, "results.south.on2.rds"))


results.best.north.off <- results.best
results.best.north.on <- results.best.on

results.best.south.off <- results.best.off
result.best.south.on



names(results.best.on) <- gsub(x = names(results.best.on), pattern = "\\-", replacement = ".")  
names(results.best.on) <- gsub(x = names(results.best.on), pattern = "\\[", replacement = "")  
names(results.best.on) <- gsub(x = names(results.best.on), pattern = "\\]", replacement = "")  
names(results.best.on)

#aggregate data by random seed
results.best.agg.on <- results.best.on %>% dplyr::select(random.seed,step,unem.rate, ltu.rate,mismatch,dur,wage)
names(results.best.agg)
results.best.agg

results.best.agg.on <- results.best.agg.on %>%
  group_by(step) %>%
  summarise_all(funs(mean))
results.best.agg.on

g <- ggplot()+
  geom_line(data=results.best.agg ,aes(x=step,y=unem.rate))+
  geom_line(data=results.best.agg ,aes(x=step,y=ltu.rate))+
  geom_line(data=results.best.agg ,aes(x=step,y=mismatch))+
  geom_line(data=results.best.agg.on ,aes(x=step,y=unem.rate,color="red"))+
  geom_line(data=results.best.agg.on ,aes(x=step,y=ltu.rate,color="red"))+
  geom_line(data=results.best.agg.on ,aes(x=step,y=mismatch,color="red"))
g

#aggregate data over step, averaging over random seed
results.best.step <- nl@simdesign@simoutput %>% select('[step]','random-seed', nl@experiment@metrics)
names(results.best.step) <- gsub(x=names(results.best.step), pattern = "\\-", replacement = ".")
names(results.best.step) <- gsub(x=names(results.best.step), pattern = "\\[", replacement = ".")
names(results.best.step) <- gsub(x=names(results.best.step), pattern = "\\]", replacement = ".")
names(results.best.step)

results.best.step
table2 <- results.best.step %>%
  group_by(.step.)%>%
  summarise_all(.funs=(mean))
table2

g <- ggplot()+
  geom_line(data=table2, aes(x=c(1:149),y=unem.rate,color="red"))+
  geom_line(data=table2, aes(x=c(1:149),y=ltu.rate, color="steelblue"))+
  geom_line(data=table2, aes(x=c(1:149),y=theta, color="brown"))
g



