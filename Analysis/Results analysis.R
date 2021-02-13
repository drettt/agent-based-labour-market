library(nlrx)

nlnorth <- genAlg_calibration_North_2
nlsouth <- genAlg_calibration_south

library(ggplot2)
fitnesssouth <- data.frame(generation=seq(1:length(nlsouth@simdesign@simoutput$mean)), evaluation=nlsouth@simdesign@simoutput$mean)
fitnessnorth <-data.frame(generation=seq(1:length(nlnorth@simdesign@simoutput$mean)), evaluation=nlnorth@simdesign@simoutput$mean)
fitness.total

pmarg <- 10

plot <- ggplot() +
  geom_line(data=fitnesssouth,aes(x=generation, y=evaluation, color="south"),size=1)+
  geom_line(data=fitnessnorth,aes(x=generation, y=evaluation,size=1, color="north"),size=1)+
  ggtitle("Evolution of the genetic algorithm")+
  theme(plot.title = element_text(size=14, hjust = 0),legend.position = "bottom")+
  scale_color_manual(name="Region",values=c("#1f78b4", "#4daf4a"))+
  labs(y="Value of loss function",
       x="Generation")
plot

ggsave(filename = "results genalg.png", dpi=320,units="mm",width=150,height=130)

fitness.plot <- ggplot(fitnessnorth, aes(x=generation, y=evaluation)) +
  geom_line(size=1) +
  #theme_ipsum(base_size = 14, axis_text_size = 14, axis_title_size = 14, strip_text_size = 14) +
  theme(plot.margin = margin(t = pmarg, r = pmarg, b = pmarg, l = pmarg, unit = "pt")) +
  ylab("fitness") +
  labs(title="Genetic algorithm calibration for loss function")
fitness.plot




library(dplyr)
results.summary.north <- tibble(parameter = names(nlnorth@experiment@variables),
                          value = (nlnorth@simdesign@simoutput$population[nrow(nlnorth@simdesign@simoutput$population), ])) %>% 
  mutate(value=c(floor(value[1]), value[2:6]))
results.summary.north

results.summary.south <- tibble(parameter = names(nlsouth@experiment@variables),
                                value = (nlsouth@simdesign@simoutput$population[nrow(nlsouth@simdesign@simoutput$population), ])) %>% 
  mutate(value=c(floor(value[1]), value[2:6]))
results.summary.south



library(grid)
library(gridExtra)
fitness.best.table <- tableGrob(results.summary.north, rows=rep("",nrow(results.summary.north)))
fitness.plot3 <- fitness.plot + annotation_custom(fitness.best.table, xmin=50, ymin=2000)
