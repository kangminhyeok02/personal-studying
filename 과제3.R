install.packages("tidyverse")

options(scipen=999) # turn off scientific notation like 1e+06
library(ggplot2)
kbo_all<-read.csv('kbobattingdata.csv')
View(kbo_all)




#1 Scatterplot
install.packages("ggalt")
library(ggalt)
kbo_select<-kbo_all[kbo_all$homeruns==max(kbo_all$homeruns),]
gg <- ggplot(kbo_all, aes(x=year, y=homeruns)) + 
  geom_point() + 
  geom_encircle(aes(x=year, y=homeruns), 
                data=kbo_select, 
                color="red", 
                size=3, 
                expand=0.01)+
  stat_smooth(method='lm',color='blUE') +
  labs(subtitle="homeruns vs Year", 
       y="homeruns", 
       x="Year", 
       title="Scatterplot", 
  )

plot(gg)
#2 Jlittered
g <- ggplot(kbo_all, aes(year, average_batter_age)) + 
  geom_jitter(width = .5, size=1) +
  labs(subtitle="age vs Year", 
       y="average_batter_age", 
       x="Year", 
       title="Jittered Points")
plot(g)
#3 Counts plot
g <- ggplot(kbo_all, aes(year, average_batter_age)) + 
  geom_count(col="tomato3", show.legend=F) +
  geom_smooth(method="lm",se=FALSE)+
  labs(subtitle="age vs Year", 
       y="average_batter_age", 
       x="Year", 
       title="Counts Plot")
plot(g)
#4 Bubble plot 
kbo_compare<- kbo_all[kbo_all$team %in% c("LG Twins", "Samsung Lions","Hanwha Eagles")&kbo_all$year>1994, ]

theme_set(theme_bw())  # pre-set the bw theme.
g <- ggplot(kbo_compare, aes(year, hits)) + 
  labs(subtitle="LG Twins vs Lotte Giants vs Hanwa Eagles",
       title="Bubble chart")

g + geom_jitter(aes(col=team, size=hits))+ geom_smooth(aes(col=team),method="lm", se=FALSE)



#5 Bar plot 
kbo_lg<-kbo_all[kbo_all$team %in% "LG Twins"&kbo_all$year>2010,]
kbo_lg<-kbo_lg[c(order(kbo_lg$year)),]
barplot(cbind(hits,doubles,triples)~year,data=kbo_lg,main= 'LG Twins HITS/Doubles/Triples ',ylab='Hits & doubles & triples',legend=TRUE,border='red',beside=TRUE,col=c('lavender','red','black'),args.legend=list(x='topleft'))

#6 Lollipop Chart
kbo_win<-kbo_all[kbo_all$year==2020,]
kbo_win<-kbo_win[c(order(kbo_win$runs_per_game)),]
kbo_win$team <- factor(kbo_win$team, levels = kbo_win$team) 

ggplot(kbo_win, aes(x=team, y=runs_per_game)) + 
  geom_point(size=3) + 
  geom_segment(aes(x=team, 
                   xend=team, 
                   y=0, 
                   yend=runs_per_game)) + 
  labs(title="Lollipop Chart", 
       subtitle="Team vs Runs_per_game", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=50, vjust=0.5,size=10,color='blue',face='bold'))

#7 Slope chart 

install.packages("dplyr")
library(dplyr)
library(ggplot2)
library(scales)
p<- kbo_all[kbo_all$team %in% "LG Twins" &kbo_all$year %in% c(2017,2020), ]
total_lg<-select(p,hits,doubles,RBI,homeruns,triples,bases_on_balls)
total_lg<-t(total_lg)
total_lg<-data.frame(total_lg)
colnames(total_lg) <- c("2017", "2020")
total_lg$data<-c('hits','doubles','RBI','homeruns','triples','bases_on_balls')
total_lg
left_label <- paste(total_lg$data,round(total_lg$`2017`),sep=", ")
right_label <- paste(total_lg$data,round(total_lg$`2020`),sep=", ")
left_label
right_label
total_lg$class <- ifelse((total_lg$`2017` - total_lg$`2020`) < 0, "red", "green")

p <- ggplot(total_lg) + geom_segment(aes(x=1, xend=2, y=`2017`, yend=`2020`, col=class), size=.75, show.legend=F) + 
  geom_vline(xintercept=1, linetype="dashed", size=.1) + 
  geom_vline(xintercept=2, linetype="dashed", size=.1) +
  scale_color_manual(labels = c("Up", "Down"), 
                     values = c("green"="#00ba38", "red"="#f8766d")) +  # color of lines
  labs(x="times", y="data change") +  # Axis labels
  xlim(.5, 2.5)+ylim(0,(1.1*(max(total_lg$`2017`, total_lg$`2020`))))  # X and Y axis limits

p <- p + geom_text(label=left_label, y=total_lg$`2017`, x=rep(1, NROW(total_lg)), hjust=1.1, size=4)
p <- p + geom_text(label=right_label, y=total_lg$`2020`, x=rep(2, NROW(total_lg)), hjust=-0.2 ,size=4)

p <- p + geom_text(label="Time 1(2020)", x=1, y=1.1*(max(total_lg$`2017`, total_lg$`2020`)), hjust=1.2, size=5)  # title
p <- p + geom_text(label="Time 2(2017)", x=2, y=1.1*(max(total_lg$`2017`, total_lg$`2020`)), hjust=-0.1, size=5)  # title

plot(p)
#8 Box plot
library(ggplot2)
theme_set(theme_classic())
compare<-kbo_all[kbo_all$team %in% c("LG Twins", "Samsung Lions","Hanwha Eagles","NC Dinos","Kia Tigers",'Lotte Giants')&kbo_all$year>2011,]
compare<-compare[c(order(compare$homeruns)),]
g <- ggplot(compare, aes(team, homeruns))
g + geom_boxplot(varwidth=T, fill="red") + 
  labs(title="Box plot", 
       subtitle="Homeruns grouped by Teams",
       x="Team name",
       y="Number of HR")+
     geom_dotplot(binaxis='y', stackdir='center',dotsize = 3, fill="blue",binwidth = 1) +
theme(axis.text.x = element_text(face='bold'))

#9 Pie chart 
library(ggplot2)
kbo<-kbo_all[kbo_all$year==2020,]
runper<-data.frame(kbo[2],kbo[4])
View(runper)

ggplot(runper, aes(x = "", y=runs_per_game, fill = factor(team))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank(), 
        plot.title = element_text(hjust=0.5)) + 
  labs(fill="team", 
       x=NULL, 
       y=NULL, 
       title="Pie Chart",
  subtitle="Runs_per_game of teams in 2020")+
        geom_text(aes(label=paste0((runs_per_game),"점")),position = position_stack
                  (vjust=0.5),face='bold')+
        coord_polar(theta = "y", start=0)+
        scale_fill_manual(values=c("red","orange","Dark red","Peru","Tomato","Plum","Light Gray","Sky Blue","Dodger Blue","Indian Red"))+
  theme_void()
  
#10 density chart
library(dplyr)
kbo_density<-kbo_all[kbo_all$team %in% c("LG Twins", "Samsung Lions","Hanwha Eagles","NC Dinos","Kia Tigers",'Lotte Giants')&kbo_all$year>2011,]
kbo_density %>%
  ggplot(aes(x=homeruns))+
  geom_density(fill='gray')+
  facet_grid(~team)+
  labs(title='Density Chart',
       subtitle='Density of Homeruns after 2011')+
theme(
  panel.spacing = unit(0.5, "lines")
)  
#11 Violin plot 
library(ggplot2)
kbo_violine<-kbo_all[kbo_all$team %in% c("LG Twins", "Samsung Lions","Hanwha Eagles","NC Dinos","Kia Tigers",'Lotte Giants')&kbo_all$year>2011,]

g <- ggplot(kbo_violine, aes(team, homeruns,fill=team))
g + geom_violin() + 
  labs(title="Violin plot", 
       subtitle="Homeruns vs teams ",
       x="team",
       y="Homeruns") + geom_point()+
  stat_summary(fun.y ='median', color = 'red', size =3, geom = 'point')

#12 Waffle chart
kbo_waffle<-kbo_all[1:100,]
var <-kbo_waffle$team
nrows <- 10
df <- expand.grid(y = 1:nrows, x = 1:nrows)
categ_table <- round(table(var) * ((nrows*nrows)/(length(var))))
categ_table
df$category <- factor(rep(names(categ_table), categ_table)) 


ggplot(df, aes(x = x, y = y, fill = category)) + 
  geom_tile(color = "black", size = 0.5) +
  scale_x_continuous(expand = c(0, 0)) +
  scale_y_continuous(expand = c(0, 0), trans = 'reverse') +
  scale_fill_brewer(palette = "Set3") +
  labs(title="Waffle Chart", subtitle="numbers of team") 



