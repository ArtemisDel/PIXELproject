library(tidyverse)
library(glmnet)
library(stringr)
library(rlist)
library(tidyr)
library(dplyr)
library(brms)
library(cmdstanr)
library(sjPlot)
library(gridExtra)
library(ggeffects)
library(grid)
library(lme4)
library(coefplot)
library(performance)
library(car)
library(glmmTMB)
library(blmeco)
library(fitdistrplus)


#load data
data<-read.csv("/Users/s1955786/Desktop/fonts_analysis/output.csv", na.strings=c("","NA"))

long_data<- data%>%
  pivot_longer(cols = -X,
               names_to = "condition_text",
               values_to = "cutoffs")

long_data<- long_data%>%
  separate(cutoffs, into = c("x", "y"), sep = ", ", remove = FALSE, convert = TRUE) %>%
  mutate(x = gsub("\\(", "", x),
         y = gsub("\\)", "", y))



#create empty columns to be used as variables in analysis
long_data$condition<-NA 
long_data$text<-NA
long_data$font<-NA
long_data$size<-NA

#regex to capture condition keys from name of each trial
#(cd[1-4]) matches condition
#(?<=\\_)([a-z]+) matches text group

for (value in 1:nrow(long_data)){
  cond=str_extract(long_data$X[value], regex("(cd[1-4])"))
  text=str_extract(long_data$X[value], regex("(?<=\\_)([a-z]+)"))
  # print(value)
  # print(cond)
  # print(text)
  long_data$condition[value]=cond
  long_data$text[value]=text
}

#check for NAs
if(any(is.na(long_data$condition))){print("NA found")
  }else {print("No NAs found")}

if(any(is.na(long_data$text))){print("NA found")
}else {print("No NAs found")}



long_data<-long_data%>%
  mutate(
  font = ifelse(grepl("cd1", condition, fixed = TRUE), "GoNotoCurrent", font),
  size = ifelse(grepl("cd1", condition, fixed = TRUE), "8", size),
  font= ifelse(grepl("cd2", condition, fixed = TRUE), "GoNotoCurrent", font),
  size = ifelse(grepl("cd2", condition, fixed = TRUE), "9", size),
  font = ifelse(grepl("cd3", condition, fixed = TRUE), "OsakaMono", font),
  size = ifelse(grepl("cd3", condition, fixed = TRUE), "8", size),
  font = ifelse(grepl("cd4", condition, fixed = TRUE), "OsakaMono", font),
  size = ifelse(grepl("cd4", condition, fixed = TRUE), "9", size)
)


#make condition and text factors
long_data$condition<-as.factor(long_data$condition)
long_data$text<-as.factor(long_data$text)
long_data$y<-as.numeric(long_data$y)
long_data$font<-as.factor(long_data$font)
long_data$size<-as.factor(long_data$size)


# these were used to double check the 0-256 cutoff ranges.
#ranges <- seq(0, 256, by = 16)
# ranges2<-seq(0,16, by=1)
# print(ranges)
# print(ranges2)

#add key for which group a value falls in
#for every 16 points in y coordinate, that's one cutoff line 

long_data <- long_data %>%
  mutate(line = case_when(
    between(y, 0, 15) ~ "l1",
    between(y, 16, 31) ~ "l2",
    between(y, 32, 47) ~ "l3",
    between(y, 48, 63) ~ "l4",
    between(y, 64, 79) ~ "l5",
    between(y, 80, 95) ~ "l6",
    between(y, 96, 111) ~ "l7",
    between(y, 112, 127) ~ "l8",
    between(y, 128, 143) ~ "l9",
    between(y, 144, 159) ~ "l10",
    between(y, 160, 175) ~ "l11",
    between(y, 176, 191) ~ "l12",
    between(y, 192, 207) ~ "l13",
    between(y, 208, 223) ~ "l14",
    between(y, 224, 239) ~ "l15",
    between(y, 240, 256) ~ "l16",
    TRUE ~ NA_character_
  ))

# long_data<-long_data%>%
#   mutate(line = ifelse(y >= 0 & y <= 15, "l1", ifelse(is.na(line), NA, line)),
#          line = ifelse(y >= 16 & y <= 31, "l2", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 32 & y <= 47, "l3", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 48 & y <= 63, "l4", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 64 & y <= 79, "l5", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 80 & y <= 95, "l6", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 96 & y <= 111, "l7", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 112 & y <= 127, "l8", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 128 & y <= 143, "l9", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 144 & y <= 159, "l10", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 160 & y <= 175, "l11", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 176 & y <= 191, "l12", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 192 & y <= 207, "l13", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 208 & y <= 223, "l14", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 224 & y <= 239, "l15", ifelse(is.na(line), NA, line)),
#          line=  ifelse(y >= 240 & y <= 256, "l16", ifelse(is.na(line), NA, line)))


#ensure line is a factor, in order to be used to count cutoffs

long_data$line<-as.factor(long_data$line)


#group by condition, text and x coordinate (x-coordinate defines which vertical line we are dealing with)
#count the lines to identify cutoffs at each vertical line
results<-long_data%>%
  group_by(condition, text, x, line)%>%
  mutate(n= n())

#remove NA values
results<-na.omit(results)

#create cutoff variable, this is binary, 1 means there was a cutoff, 0 means there wasn't
#though in this case with NAs gone we will only get 1s
results<- results%>%
  mutate(cutoff = ifelse(n > 0, 1, 0))

results<- results %>%
  distinct(x, line, .keep_all = TRUE)

#once again group by condition and text and line
# we just want a total sum of the cutoffs for each text in each condition

results2<-results%>%
  group_by(condition, text)%>%
  mutate(total_cutoffs=sum(cutoff))

#remove duplicate rows for text condition and total cutoffs
results3<- results2 %>%
  distinct(condition, text, total_cutoffs, .keep_all = TRUE)

#just a sanity check to ensure there's 4 of each of 25 texts
# plot_texts<-results3%>%
#   group_by(text)%>%
#   count()

descriptive_stats<-results3%>%
  group_by(font, size)%>%
  summarise(mean=mean(total_cutoffs),
            sd=sd(total_cutoffs),
            min=min(total_cutoffs),
            max=max(total_cutoffs))

descriptive_stats$condition <- paste(descriptive_stats$font, descriptive_stats$size, sep = "_size")

print(descriptive_stats)


#plot means
means_plot<-descriptive_stats%>%
  ggplot(aes(x=condition, y=mean, fill=condition))+geom_col()+ guides(fill = "none")+
  labs(title = "Bar plot of mean cutoffs per font setting condition", y="mean cutoff score")+
  theme_light()+
  scale_y_continuous(limits = c(0, 90), breaks = seq(0, 90, by = 5))
means_plot

#plot histogram
plot<-results3%>%
  ggplot(aes(x=total_cutoffs, colour=condition))+geom_histogram(binwidth = 10)
plot

plotcd1<-results3%>%
  filter(condition=="cd1")%>%
  ggplot(aes(x=total_cutoffs))+geom_histogram(binwidth = 5, fill="red")+xlim(0,150)+ylim(0,6)+ggtitle("GoNotoCurrent, size 8")+
  theme_light()+labs(x="Number of cutoffs", y="Number of trials")
plotcd1

plotcd2<-results3%>%
  filter(condition=="cd2")%>%
  ggplot(aes(x=total_cutoffs))+geom_histogram(binwidth = 5, fill="lightblue")+xlim(0,150)+ylim(0,6)+ggtitle("GoNotoCurrent, size 9")+
  theme_light()+labs(x="Number of cutoffs", y="Number of trials")
plotcd2

plotcd3<-results3%>%
  filter(condition=="cd3")%>%
  ggplot(aes(x=total_cutoffs))+geom_histogram(binwidth = 5, fill="purple")+xlim(0,150)+ylim(0,6)+ggtitle("OsakaMono, size 8")+
  theme_light()+labs(x="Number of cutoffs", y="Number of trials")
plotcd3

plotcd4<-results3%>%
  filter(condition=="cd4")%>%
  ggplot(aes(x=total_cutoffs))+geom_histogram(binwidth = 5, fill="green")+xlim(0,150)+ylim(0,6)+ggtitle("OsakaMono, size 9")+
  theme_light()+labs(x="Number of cutoffs", y="Number of trials")
plotcd4

plots<-grid.arrange(plotcd1, plotcd2, plotcd3, plotcd4, ncol = 2, 
                    top = textGrob("Distribution of total cutoffs in the 25 trials per condition", gp = gpar(fontsize = 20)))



results3$obs_index <- 1:nrow(results3)

model_old<-model_new<-glmer(total_cutoffs ~ font * size + (1|text), data=results3,
                            family="poisson")

dispersion_glmer(model_old)#dispersion is too large

    
model_new<-glmer(total_cutoffs ~ font * size + (1|obs_index), data=results3,
                 family="poisson")

plot(model_new)
summary(model_new)
tab_model(model_new) 
check_model(model_new)

coefplot(model_new)

predicted_values <- predict(model_new, type = "response", re.form = NA)


data_and_predicted<- cbind(results3, predicted_values)


ggplot(data_and_predicted, aes(x = size, y = total_cutoffs, group = font, colour=font)) +
  geom_jitter(position = position_jitter(width = 0.2), alpha = 0.5) +  # Real data points with jitter for visibility
  geom_line(aes(y = predicted_values)) +  # Predicted values
  labs(x = "Size", y = "Total_cutoffs", color = "Font") +
  theme_minimal()


# performance::check_model(model_new)
# not ideal for poisson GLMM






#assumption checks

#over-dispersion 

dispersion_glmer(model_new)


# Extract influence measures
infl <- influence(model_new)

# Extract Cook's distance values
cooks_dist <- infl$cook.distance

# Calculate the threshold for Cook's distance 
threshold <- 4 / nrow(model_new)
threshold

par(mfrow = c(2, 2))  # 2 rows, 2 columns

# Plot residuals vs. fitted values
plot(residuals(model_new) ~ fitted(model_new), main = "Residuals vs. Fitted")
abline(h = 0, col = "red", lty = 2)

# Q-Q plot
qqnorm(residuals(model_new))
qqline(residuals(model_new))

# Scale-Location plot
plot(sqrt(abs(residuals(model_new))) ~ fitted(model_new), main = "Scale-Location Plot")
abline(h = mean(sqrt(abs(resid(model_new)))), col = "red", lty = 2)


#Residuals vs Leverage
plot(hatvalues(model_new), residuals(model_new), main = "Residuals vs. Leverage")
abline(h = 0, col = "red", lty = 2)


#check for poisson distribution 
plot(fitdist(results3$total_cutoffs,"pois"))


