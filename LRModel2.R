# Position 1800/2923
# Predictive Accuracy: 0.90372

NewsTrain = read.csv("NYTimesBlogTrain.csv", stringsAsFactors=FALSE)
NewsTest = read.csv("NYTimesBlogTest.csv", stringsAsFactors=FALSE)

# Extract out the DV (Popular) from the NewsTrain set
Pop <- as.factor(NewsTrain$Popular)
NewsTrain$Popular <- NULL

# Bind both datasets together
combined.df <- rbind(NewsTrain, NewsTest)

# Fill in missing value with Others
combined.df$NewsDesk[combined.df$NewsDesk == ""] <- "Others"
combined.df$SectionName[combined.df$SectionName == ""] <- "Others"
combined.df$SubsectionName[combined.df$SubsectionName == ""] <- "Others"
combined.df$NewsDesk[combined.df$NewsDesk == ""] <- "Others"
combined.df$SectionName[combined.df$SectionName == ""] <- "Others"
combined.df$SubsectionName[combined.df$SubsectionName == ""] <- "Others"

# Convert character variables into factors
combined.df$NewsDesk <- as.factor(combined.df$NewsDesk)
combined.df$SectionName <- as.factor(combined.df$SectionName)

# Convert WordCount to Log form to address skewnesss and add a 1 to prevent log(0) = -Inf
combined.df$WordCount <- log(combined.df$WordCount + 50)

# Extract the day and the hour from the PubDate variable
combined.df$PubDate <- strptime(combined.df$PubDate, "%Y-%m-%d %H:%M:%S")
combined.df$Weekday <- as.factor(combined.df$PubDate$wday)
combined.df$Hour <- as.factor(combined.df$PubDate$hour)

# Remove variables that will not be included in the model
combined.df$Headline <- NULL
combined.df$Snippet <- NULL
combined.df$Abstract <- NULL
combined.df$PubDate <- NULL
combined.df$UniqueID <- NULL

# Split them into training/testing set
Train = head(combined.df, nrow(NewsTrain))
Test = tail(combined.df, nrow(NewsTest))

# Attach DV(Popular) back into training set
Train$Popular <- Pop

# LR Model building
LogModel2 <- glm(Popular ~ ., data = Train, family = binomial)
PredTest = predict(LogModel2, newdata = Test, type = "response")
MySubmission = data.frame(UniqueID = NewsTest$UniqueID, Probability1 = PredTest)

write.csv(MySubmission, "LogModel2.csv", row.names=FALSE)