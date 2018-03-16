#####################################################
library(knitr)
library(ggplot2)
library(data.table)
library(plyr)
library(dplyr)
library(tidytext)
library(stringr)
library(tidyr)
library(scales)
library(glmnet)
library(wordcloud2)
library(wordcloud)
library(tm)

# Set WD
setwd("C:/Users/Maggie/OneDrive/UW - BHI/Research Ish") 

projectSubset <- fread("C:/Users/Maggie/Downloads/projectDataSubset.csv", header=TRUE, sep=',')[,-1] 
subsetLimited <- projectSubset %>% select(case, subject_id, hadm_id, text)
subsetLimited <- unique(subsetLimited[ ,1:4]) #76309
subsetLimited$subject_id <- as.character(subsetLimited$subject_id)

data(stop_words)

###################################################
#Demographics
subsetDemo <- projectSubset %>% select (case, subject_id, age, ethnicity, insurance, religion, language)
subsetDemo <- subsetDemo[!duplicated(subsetDemo$subject_id)]

#Compress demographic variables
library(car)
subsetDemo$ethnicity2 <- subsetDemo$ethnicity
subsetDemo$ethnicity2 <- recode(subsetDemo$ethnicity2, "c('BLACK/HAITIAN', 'BLACK/CAPE VERDEAN')='BLACK/AFRICAN AMERICAN'")
subsetDemo$ethnicity2 <- recode(subsetDemo$ethnicity2, "c('PATIENT DECLINED TO ANSWER', 'UNABLE TO OBTAIN')='UNKNOWN/NOT SPECIFIED'")
subsetDemo$ethnicity2 <- recode(subsetDemo$ethnicity2, "c('HISPANIC/LATINO - PUERTO RICAN', 'HISPANIC/LATINO - CUBAN', 'HISPANIC/LATINO - GUATEMALAN', 'HISPANIC/LATINO - SALVADORAN')='HISPANIC OR LATINO'")
subsetDemo$ethnicity2 <- recode(subsetDemo$ethnicity2, "c('WHITE - BRAZILIAN', 'WHITE - OTHER EUROPEAN', 'WHITE - RUSSIAN')='WHITE'")
subsetDemo$ethnicity2 <- recode(subsetDemo$ethnicity2, "c('ASIAN - ASIAN INDIAN', 'ASIAN - CAMBODIAN', 'ASIAN - CHINESE')='ASIAN'")
subsetDemo$ethnicity2 <- recode(subsetDemo$ethnicity2, "c('MIDDLE EASTERN', 'AMERICAN INDIAN/ALASKA NATIVE', 'MULTI RACE ETHNICITY', 'OTHER')='MULTIPLE ETHNICITIES/OTHER'")
detach(package:car, unload=TRUE)

#Break it down into cases and controls
subsetDemoCase <- filter(subsetDemo, case==1)
subsetDemoControl <- filter(subsetDemo, case==0)

#Calculate proportions
library(tibble)
ethnicityFunction <- function(data){
  sort(data$ethnicity2)
  data %>% count(ethnicity2, sort=FALSE) %>% mutate(proportion = n/sum(n))
}
rows <- function(dat){
  column_to_rownames(as.data.frame(dat), var="ethnicity2")
}

demoCombEth <- ethnicityFunction(subsetDemo) %>% rows()
demoCaseEth <- ethnicityFunction(subsetDemoCase) %>% rows()
demoControlEth <- ethnicityFunction(subsetDemoControl) %>% rows()
ethnicityTable <- cbind(demoCombEth, demoCaseEth, demoControlEth)
library(kableExtra)
kable(ethnicityTable, "html") %>%
  kable_styling("striped") %>%
  add_header_above(c(" "=1, "Total"=2, "Cases"=2, "Controls"=2))

###################################################

#Create dataframe with character vectors for subset
subsetDF <- subsetLimited %>%
  group_by(subject_id) %>%
  mutate(linenumber = row_number(),
         visit = hadm_id) %>%
  ungroup()
subsetDF$text <- as.character(subsetDF$text)
subsetDF$text <- tolower(subsetDF$text)

subsetTokens <- subsetDF %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z']+")) %>%
  na.omit()



##################################################

casesDF <- filter(subsetLimited, case=="1")
controlsDF <- filter(subsetLimited, case=="0")

#Cases
casesDF <- casesDF %>% 
  group_by(subject_id) %>% 
  mutate(linenumber = row_number(),
         visit= hadm_id) %>%
  ungroup()

casesDF$text <- as.character(casesDF$text)
casesDF$text <- tolower(casesDF$text)

#Separate each line of text into tokens & remove stop words
tidyCasesDF <- casesDF %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  na.omit()


#Controls
controlsDF <- controlsDF %>% 
  group_by(subject_id) %>% 
  mutate(linenumber = row_number(),
         visit= hadm_id) %>%
  ungroup()

controlsDF$text <- as.character(controlsDF$text)
controlsDF$text <- tolower(controlsDF$text)

#Separate each line of text into tokens & remove stop words
tidyControlsDF <- controlsDF %>% 
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  mutate(word = str_extract(word, "[a-z]+")) %>%
  na.omit()


#Putting them back together
frequencyDF <- bind_rows(mutate(tidyCasesDF, category="Cases"),
                         mutate(tidyControlsDF, category="Controls")) %>%
  count(category, word) %>%
  group_by(category) %>%
  mutate(proportion = n/sum(n)) %>%
  select(-n) %>%
  spread(category, proportion) %>%
  gather(category, proportion, `Controls`)

#Plotting
#expect a warning about rows with missing values being removed
ggplot(frequencyDF, aes(x=proportion, y=`Cases`, color=abs(`Cases` - proportion)))+
  geom_abline(color="gray40", lty=2) +
  geom_jitter(alpha=0.1, size=2.5, width=0.3, height=0.3) +
  geom_text(aes(label=word), check_overlap=TRUE, vjust=1.5) +
  scale_x_log10(labels=percent_format()) +
  scale_y_log10(labels=percent_format()) +
  scale_color_gradient(limits=c(0, 0.001), low="darkslategray4", high="gray75") + 
  facet_wrap(~category, ncol=1) +
  theme(legend.position="none") +
  labs(y="Cases", x=NULL)



####################################################
#Word Clouds
#Cases
tidyCasesFreq <- tidyCasesDF %>% 
  count(tidyCasesDF$word) %>%
  top_n(100)
names(tidyCasesFreq)[1] <- "word"
names(tidyCasesFreq)[2] <- "freq"
tidyCasesFreq <- tidyCasesFreq[order(-tidyCasesFreq$freq),]
wordcloud2(tidyCasesFreq)

#Controls
tidyControlsFreq <- tidyControlsDF %>% 
  count(tidyControlsDF$word) %>%
  top_n(100)
names(tidyControlsFreq)[1] <- "word"
names(tidyControlsFreq)[2] <- "freq"
tidyControlsFreq <- tidyControlsFreq[order(-tidyControlsFreq$freq),]
# if(dev.cur()==1) getOption("device")()
# pdf("C:/Users/Maggie/OneDrive/UW - BHI/2018 Winter/BIOS 544/projectControlCloud.pdf", width=1200, height=900)
wordcloud2(tidyControlsFreq)
# while (!is.null(dev.list())) dev.off()
# dev.off()

#Combined
tidyCombinedFreq <- subsetTokens %>%
  count(subsetTokens$word) %>%
  top_n(100)
names(tidyCombinedFreq)[1] <- "word"
names(tidyCombinedFreq)[2] <- "freq"
tidyCombinedFreq <- tidyCombinedFreq[order(-tidyCombinedFreq$freq),]
wordcloud2(tidyCombinedFreq)




#More Clouds
cases <- paste(tidyCasesFreq$word, collapse=" ")
controls <- paste(tidyControlsFreq$word, collapse=" ")
casecontrol <- c(cases, controls) #single vector
corpus <- Corpus(VectorSource(casecontrol)) #create corpus
tdm <- TermDocumentMatrix(corpus) #create term-document matrix
tdm <- as.matrix(tdm) #convert as matrix
colnames(tdm) <- c("Cases", "Controls") #add column names

#Plot comparison cloud
# png("C:/Users/Maggie/OneDrive/UW - BHI/2018 Winter/BIOS 544/projectComparisonCloud.png", width=1000, height=1000)
comparison.cloud(tdm, 
                 random.order=FALSE, 
                 colors=c("#00B2FF", "#6600CC"),
                 title.size=1,
                 max.words=50)
# dev.off()

#Plot commonality cloud
# png("C:/Users/Maggie/OneDrive/UW - BHI/2018 Winter/BIOS 544/projectCommonalityCloud.png", units="in", width=1000, height=1000)
commonality.cloud(tdm, 
                  random.order=FALSE,
                  colors=brewer.pal(3, "Dark2"),
                  max.words=20)
# dev.off()



####################################################

#Quantify similarity between word frequecy sets
cor.test(data=frequencyDF[frequencyDF$category=="Controls",],
         ~ proportion + `Cases`)



####################################################
#The lasso part

glmProjectDF <- subsetDF %>% unnest_tokens(word, text) %>%
  mutate(word = str_extract(word, "[a-z]+"))# %>%  "[a-z']+"
#na.omit()

#Remove stopwords 
glmProjectDF <- glmProjectDF %>% anti_join(stop_words) %>% na.omit()

glmProjectCnt <- glmProjectDF %>%
  count(glmProjectDF$word) %>%
  filter(n>10) #for n>10, 16864 instead of ~60k --> only cuts down total by 100,000
#for n>100, 6875 --> cuts down total by 450k
names(glmProjectCnt)[1] <- "word"
glmProjectData <- join(glmProjectDF, glmProjectCnt, by="word", type="right", match="all")
glmProjectDF <- subset(glmProjectData, select=-c(n))


#Find most common words (td-idf)
glmProjectCount <- glmProjectDF %>% count(word, subject_id, case, sort=TRUE)
glmProjectTotal <- glmProjectCount %>% group_by(subject_id) %>% summarize(Abs_total = sum(n)) 
glmProjectCT <- left_join(glmProjectCount, glmProjectTotal)

projectSkinnyFeatures <- glmProjectCT %>% bind_tf_idf(word, subject_id, Abs_total) #1220047 --> 1015426 (& all went through)


#Converting to matrix form (dtm???)
projectMatrix <- projectSkinnyFeatures %>% cast_sparse(subject_id, word, tf_idf) #30,916,608 elements --> 3520000 (MUCH more reasonable)

projectOrd <- match(rownames(projectMatrix), subsetDF$subject_id)
projectOutcome <- ifelse(subsetDF$case[projectOrd] == 1, 1, 0) 


#Build sparse logistic model - lasso model
set.seed(23)
projectMatrixScale <- scale(projectMatrix)
projectMatrixScale[is.nan(projectMatrixScale)] = 0

#projectMatrixScale <- na.omit(projectMatrixScale)
projectCVFit <- cv.glmnet(projectMatrixScale, projectOutcome, family="binomial", type.measure="class")
plot(projectCVFit)

#deviance.glmnet(projectCVFit)


