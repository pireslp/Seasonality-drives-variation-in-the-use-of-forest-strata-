#set working directory#

#required packages#
install.packages("dabestr")
install.packages("grid")
install.packages("devtools")
devtools::install_github("ACCLAB/dabestr")
install.packages("circular")
library(circular)
library(dabestr)
library(ggplot2)
library(forcats)
library(cowplot)
library(grid)

#Importing files
strata=read.delim("foraging.txt", h=T) 
phenology=read.table("phenology.txt", h=T)

#Defining relevant data for the analyses
males=subset(strata, Phenotype1=="Adult male") #data of adult males only
green=subset(strata, Phenotype1=="Green") # data of greens only
dry=subset(strata, Season=="Dry") #data of dry season only
wet=subset(strata, Season=="Wet ") #data of wet season only

max(strata$Foraging) #maximum foraging height
min(strata$Foraging) #minimum foraging height

length(which(strata$Strata=="midstory")) #number of individuals foraging at midstory
length(which(males$Season=="Wet ")) #number of adult males recorded in the wet season
length(which(males$Season=="Dry")) #number of adult males recorded in the dry season
length(which(green$Season=="Wet ")) #number of greens recorded in the wet season
length(which(green$Season=="Dry")) #number of greens recorded in the dry season
min(males$Foraging, males$Season=="Wet ") #


#create a frequency distribution
cut=seq(0,19, by=1)

#Figure 1
myplot <- ggplot(strata, aes(cut(Foraging, breaks = cut), group = Phenotype1)) + 
  geom_bar(aes(y = ..prop.., fill = factor(Phenotype1))) +
  labs(y = "Percent", fill="Phenotype1") +
  facet_grid(~Phenotype1) +
  scale_y_continuous(labels=scales::percent) +
  scale_fill_manual(values=c("black", "darkgreen")) +
  scale_x_discrete(labels=c('1','2','3','4','5','6','7','8','9','10','11','12','13',
                            '14','15','16','17','18','19'))+
  geom_vline(xintercept = 3, linetype="dotted", size=1.5)+
  geom_vline(xintercept = 15, linetype="dotted", size=1.5)+
  ylab("Relative frequencies") +
  xlab("Foraging height (m)")+
  theme_classic()+
  facet_grid(~Phenotype1)+
  theme(axis.text.x = element_text(color = "black", size = 11, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, face = "plain"),
        axis.title.y = element_text(color="black", size=12, face="bold", vjust=3),
        axis.title.x = element_text(color="black", size=12, face="bold", vjust=-1),
        axis.line = element_line(colour = 'black', size = 0.8),
        strip.background = element_blank(),
        strip.text.x = element_blank(),
        legend.position = "none",
        plot.margin = margin(1, 1, 1, 1, "cm"))
myplot
ggsave("Figure 1.1.tiff", plot=myplot, dpi = 300, width = 10, height=7)

#Estimation statistics for foraging height
between <- 
  strata %>%
  dabest(Phenotype, Foraging, 
         idx = list(c("Adult male-dry","Green-dry"),
                    c("Adult male-wet","Green-wet")))

within= strata %>%
  dabest(Phenotype, Foraging, 
         idx = list(c("Adult male-dry","Adult male-wet"),
                    c("Green-dry","Green-wet")))

# Compute the mean difference.
between.meandiff <- mean_diff(between)
within.meandiff <- mean_diff(within)

#estimation plots
between.plot= between.meandiff %>% 
  plot(rawplot.ylabel = "Foraging height (m)",
       rawplot.markersize =4, rawplot.type = "swarmplot",
       rawplot.groupwidth = 0.4,effsize.ylabel = "Mean Difference",
       effsize.markersize = 4, palette = c("black", "darkgreen", "black", "darkgreen"),
       axes.title.fontsize = 12, tick.fontsize = 12, theme= theme_classic()+theme(axis.title.y = element_text(face="bold"),
                                                                                  axis.text.x = element_text(color = "black", size = 12, face = "plain"),
                                                                                  axis.text.y = element_text(color = "black", size = 12, face = "plain")))


within.plot=
  within.meandiff %>% 
  plot(rawplot.ylabel = "Foraging height (m)",
       rawplot.markersize =4, rawplot.type = "swarmplot",
       rawplot.groupwidth = 0.4,effsize.ylabel = "Mean Difference",
       effsize.markersize = 4, palette = c("black", "black", "darkgreen", "darkgreen"),
       axes.title.fontsize = 12, tick.fontsize = 12, theme= theme_classic()+theme(axis.title.y = element_text(face="bold"),
                                                                                  axis.text.x = element_text(color = "black", size = 12, face = "plain"),
                                                                                  axis.text.y = element_text(color = "black", size = 12, face = "plain")))
 
dev.off
ggsave("between.tiff", plot=between.plot, dpi = 300, height = 5.66)
ggsave("within.tiff", plot=within.plot, dpi = 300, height = 5.66, width = 6.5)

plot_grid(between.plot,within.plot, nrow=2,ncol=1,axis = "l", align = "v")

#correlation test between foraging height and time
cor.test(males$Time,males$Foraging, method = "spearman") #for males
cor.test(green$Time,green$Foraging, method = "spearman") # for greens

#Figure 2
ND=ggplot(strata, aes(x=Foraging, y=Time))+ 
  geom_point(size=4,aes(color = Phenotype1), shape = 19 ) +
  geom_smooth(method="lm", se=F,  aes(color = Phenotype1))+ 
  theme_classic() + 
  theme(axis.text.x = element_text(color = "black", size = 12, face = "plain"),
        axis.text.y = element_text(color = "black", size = 12, face = "plain"),
        axis.title.y = element_text(color="black", size=12, face="bold", vjust=3),
        axis.title.x = element_text(color="black", size=12, face="bold", vjust=-1),
        axis.line = element_line(colour = 'black', size = 0.8),
        legend.position = "none") +
  labs(x="Foraging height (m)", y = "Time (s)")+ theme(aspect.ratio=1/1.5)+
  scale_color_manual(values=c("black", "darkgreen"))
plot(ND)
ggsave("time x forage.tiff", plot=ND, dpi = 300)

#Estimation statistics for phenology
fruit.int <- 
  phenology %>%
  dabest(Season, Percentage, 
         idx = list(c("WET","DRY")))

fruit.int.meandiff <- mean_diff(fruit.int)
fruit.int.plot= fruit.int.meandiff %>% 
  plot(rawplot.ylabel = "Mean fruiting intensity (%)",
       rawplot.markersize =4, rawplot.type = "swarmplot",
       rawplot.groupwidth = 0.4,effsize.ylabel = "Mean Difference",
       effsize.markersize = 4, palette = c("slateblue4", "orange"),
       axes.title.fontsize = 12, tick.fontsize = 12,
       theme= theme_classic()+
         theme(axis.title.y = element_text(face="bold"),
                axis.text.x = element_text(color = "black", size = 12, face = "plain"),
                axis.text.y = element_text(color = "black", size = 12, face = "plain")))
fruit.int.plot

plant.height=phenology %>%
  dabest(Season, Height, 
         idx = list(c("WET","DRY")))

plant.height.meandiff <- mean_diff(plant.height)
plant.height.plot= plant.height.meandiff %>% 
  plot(rawplot.ylabel = "Mean fruit height (m)",
       rawplot.markersize =4, rawplot.type = "swarmplot",
       rawplot.groupwidth = 0.4,effsize.ylabel = "Mean Difference",
       effsize.markersize = 4, palette = c("slateblue4", "orange"),
       axes.title.fontsize = 12, tick.fontsize = 12,
       theme= theme_classic()+theme(axis.title.y = element_text(face="bold"),
                                    axis.text.x = element_text(color = "black", size = 12, face = "plain"),
                                    axis.text.y = element_text(color = "black", size = 12, face = "plain")))
plant.height.plot

ggsave("fig3.tiff", plot=plot_grid(fruit.int.plot,plant.height.plot, nrow=2,ncol=1,axis = "l", align = "v", labels = c("A)", "B)")), dpi = 300,  height = 7)
