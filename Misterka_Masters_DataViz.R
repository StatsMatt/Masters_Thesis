### STA 504 Data Visualization Project ###
## Author: Matt Misterka ##

### required libraries ###
library(tidyverse)
###
setwd("C:\\Users\\Matt\\Documents\\Masters Project\\363_AB_data")

# Read in data
# the columns represent the student and 36 items on the SATS
# s4N tells us the fourth item is negatively-worded.
sta363pre_read<-read.csv("363_Apre.csv")
sta363post_read<-read.csv("363_Apost.csv")
sta363pre_nread<-read.csv("363_Bpre.csv")
sta363post_nread<-read.csv("363_Bpost.csv")

### reverse the negative items -------------
# indeces
rev_code_items <- c(5,6,8,9,12,14,16,17,19,22,25,26,27,29,31,34:37)
# function to apply
SATS_reverse <- function(callem){
  8-callem
}

### reverse the items for each class
sta363pre_read <- sta363pre_read %>%
  mutate_at(rev_code_items, SATS_reverse)
sta363post_read <- sta363post_read %>%
  mutate_at(rev_code_items, SATS_reverse)

sta363pre_nread <- sta363pre_nread %>%
  mutate_at(rev_code_items, SATS_reverse)
sta363post_nread <- sta363post_nread %>%
  mutate_at(rev_code_items, SATS_reverse)

### assign section and survey variables
sta363pre_read$Section<-"reading"
sta363pre_read$Survey<-"pre"
sta363post_read$Section<-"reading"
sta363post_read$Survey<-"post"
sta363pre_nread$Section<-"no reading"
sta363pre_nread$Survey<-"pre"
sta363post_nread$Section<-"no reading"
sta363post_nread$Survey<-"post"

### rbind all of the data sets
sta363<-rbind(sta363pre_read,
              sta363post_read,
              sta363pre_nread,
              sta363post_nread)

### calculate component averages
sta363 <- sta363 %>%
  mutate(affect=(s3+s4N+s15N+s18N+s19+s28N)/6,
         cognitive=(s5N+s11N+s26N+s31+s32+s35N)/6,
         value=(s7N+s9+s10+s13N+s16N+s17+s21N+s25N+s33N)/9,
         difficulty=(s6+s8N+s22+s24N+s30N+s34N+s36N)/7,
         interest=(s12+s20+s23+s29)/4,
         effort=(s1+s2+s14+s27)/4) %>%
  dplyr::select(student,Section,Survey,
                affect,cognitive,value,
                difficulty,interest,effort)

### use a uniminmax transformation in order to compare the components
sta363<-sta363 %>%
  mutate(affect.u=(affect-1)/6,cognitive.u=(cognitive-1)/6,
         value.u=(value-1)/6,difficulty.u=(difficulty-1)/6,
         interest.u=(interest-1)/6,effort.u=(effort-1)/6) %>%
  dplyr::select(-c(affect,cognitive,value,difficulty,interest,effort))

### rename columns to look nice in faceted plot
colnames(sta363)[4:9]
colnames(sta363)[4:9]<-c("Affect (6 items)","Cognitive (6 items)","Value (9 items)",
                         "Difficulty (7 items)","Interest (4 items)","Effort (4 items)")

### tall dataset
keycol <- "component"
valuecol <- "score"
gathercols <- colnames(sta363)[4:9]
gather<- gather_(sta363, keycol, valuecol, gathercols)

sta363_tall<-gather %>%
  mutate(Survey=factor(Survey, levels = c("pre","post")))
###

### plot creatation
attitudes<-ggplot(data = sta363_tall)+
  geom_line(aes(x=as.numeric(Survey),y=score,group=interaction(student,Section),color=Section),alpha=.5,size=1)+
  scale_x_continuous(limits = c(1,2),breaks=c(1.01,1.99),labels=c("Pre", "Post"))+
  scale_y_continuous(limits = c(0,1))+
  theme_bw() + scale_color_manual(values=c("red", "blue"))+
  labs(title = "Comparison of Student Attitudes",
subtitle = "Positive Attitudes > 0.5, Negative Attitudes < 0.5",
       y="Composite Score",x="Survey")+facet_grid(.~component)
attitudes
