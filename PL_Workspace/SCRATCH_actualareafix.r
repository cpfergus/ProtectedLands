

> length(unique(cum_ArNLCD$estYr))
[1] 31



# Remove duplicate rows based on er_maj, nlcd class, est Yr

dat[duplicated(dat[,1:2]),]


cum_ArNLCD <- cum_ArNLCD[!duplicated(cum_ArNLCD[,c(1,4,6)]),]



# Create empty dataframe and fill in with vaues
#create an empty dataframe with what we want
df2 = data.frame(NAME = "", 
                 ID = 0, 
                 SURVEY_YEAR = min(df$SURVEY_YEAR):max(df$SURVEY_YEAR), 
                 REFERENCE_YEAR = min(df$SURVEY_YEAR):max(df$SURVEY_YEAR) - 1,
                 VALUE = NA, stringsAsFactors=FALSE)