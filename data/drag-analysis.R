library('tidyverse')
library('ggplot2')
library('latex2exp')
displacements <- read_csv('displacement.csv')
leaf.data <- read_csv('leaf-data.csv')

# switch to SI units
displacements <- mutate(displacements,
                        displacement.m = displacement_in*0.0254)
leaf.data <- mutate(leaf.data,
                    area.m2 = area_ft2*0.093903)

# join datasets
data <- full_join(displacements,leaf.data)

# compute drag and D/A
data <- mutate(data,
               D.N = 0.05886061*4.44822/0.0254*displacement.m,
               DA.Nm2 = D.N/area.m2);

# do analysis of variance (ANOVA)
foo <- aov(D.N~as.factor(speed)+as.factor(leaf),data)
#summary(foo)
#                 Df  Sum Sq  Mean Sq F value   Pr(>F)    
#as.factor(speed)  2 0.00066 0.000330   5.674   0.0137 *  
#as.factor(leaf)   1 0.01028 0.010277 176.796 4.59e-10 ***
#Residuals        16 0.00093 0.000058                     
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


bar <- aov(DA.Nm2~as.factor(speed)+as.factor(leaf),data)
#                 Df Sum Sq Mean Sq F value Pr(>F)  
#as.factor(speed)  2 10.972   5.486   4.653 0.0255 *
#as.factor(leaf)   1  1.238   1.238   1.050 0.3207  
#Residuals        16 18.864   1.179                 
#---
#Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# now make two figures
drag.summary <- data %>%
    group_by(leaf,speed) %>%
    summarize(mean.D = mean(D.N),
              sd.D = sd(D.N),
              mean.DA = mean(DA.Nm2),
              sd.DA = sd(DA.Nm2))

drag.plot <- ggplot(drag.summary, aes(x=as.factor(speed),y=mean.D,fill=leaf))+
    geom_col(position=position_dodge2(preserve="single"))+
    geom_errorbar(aes(ymin=mean.D-sd.D,ymax=mean.D+sd.D),width=0.2,
                  position=position_dodge(width=0.9))+
    xlab("fan setting")+
    ylab("drag, N")+
    scale_fill_manual(values=c("green","gray50"))+
    annotate("text",x=3.23,y=0.025,label="*")+
    theme_bw(base_size=8)+
    theme(legend.position="none")
pdf('results1.pdf',width=3,height=2)
print(drag.plot)
dev.off()

da.plot <- ggplot(drag.summary, aes(x=as.factor(speed),y=mean.DA,fill=leaf))+
    geom_col(position=position_dodge2(preserve="single"))+
    geom_errorbar(aes(ymin=mean.DA-sd.DA,ymax=mean.DA+sd.DA),width=0.2,
                  position=position_dodge(width=0.9))+
    xlab("fan setting")+
    ylab("drag/area, N/m$^2$")+
    scale_fill_manual(values=c("green","gray50"))+
    annotate("segment",x=1-0.2,xend=2+0.2,y=7,yend=7)+
    annotate("segment",x=2-0.2,xend=3,y=7.25,yend=7.25)+
    theme_bw(base_size=8)+
    theme(legend.position="none")
pdf('results2.pdf',width=3,height=2)
print(da.plot)
dev.off()

#png('results2.png',width=300,height=200)
#print(da.plot)
#dev.off()

ggsave(file="results1.svg",plot=drag.plot,width=3,height=2);
ggsave(file="results2.svg",plot=da.plot,width=3,height=2);




# get some group stats
grouped <- group_by(data, speed, leaf)
grouped.results <- summarise(grouped,
                             mean.D=mean(D.N),
                             sd.D=sd(D.N),
                             mean.DA=mean(DA.Nm2),
                             sd.DA=sd(DA.Nm2))

                         
