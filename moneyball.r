library(tidyverse)

library(rvest)         # web scraping - ESPN salaries
library(stringr)       # data cleanup - ESPN salaries
library(scales)        # EDA visualization
library(ggridges)      # EDA visualization
library(viridis)       # EDA visualization

library(caret)         # Classification And REgression Training
library(elasticnet)    # lasso
library(ranger)        # ranger
library(earth)         # bagEarth
library(neuralnet)     # nnet
library(leaps)         # leapSeq
library(kernlab)       # svmPoly
library(Rborist)       # Rborist
library(kknn)          # kknn
library(blasso)        # blasso
library(party)         # cforest
library(evtree)        # evtree
library(xgboost)       # xgboost
library(gbm)

options(java.parameters = "-Xmx5g") # More memory



############################################################
####### Part 1: Scrape/Import/Clean/Combine Datasets #######
############################################################



### Scrape Player Salaries from ESPN for two recent pre-COVID seasons
### 2018 season (2017-18) and 2019 season (2018-19)


## Construct salary URLs
season_urls = sprintf('http://www.espn.com/nba/salaries/_/year/%s/', 2013:2022)
# season_urls = c(season_urls, 'http://www.espn.com/nba/salaries/_/') # add current year


## Compile salary data
datalist = list()
n = 1
for (i in 1:length(season_urls)) {
  Sys.sleep(.2)
  
  # Download 
  page = read_html(season_urls[i])
  
  # Determine number of sub-pages
  text = page %>% html_node('.page-numbers') %>% html_text() 
  num_pages = text %>% str_sub(., start= -2) %>% as.numeric()
  print(paste(i, " : ", text, " : ", num_pages, sep = ''))
  
  # Dynamically create list of sub-page urls
  temp_urls = paste(season_urls[i], 'page/', 2:num_pages, sep = '')
  temp_urls = c(temp_urls, season_urls[i]) # add first page back in
  
  # Loop through sub-pages
  for (j in 1:length(temp_urls)) {
    print(temp_urls[j])
    Sys.sleep(2)
    
    # Determine year
    year = str_sub(season_urls[i], -5, -2)
    page = read_html(temp_urls[j])
    table = page %>% html_table()
    datalist[[n]] = table[[1]] %>% mutate(year = year, page = j)
    n = n + 1
  }
}

# Combine salary data
raw = do.call('rbind', datalist)

salaries = raw %>%
  select(rank = X1, name = X2, team = X3, salary = X4, season = year) %>%
  filter(rank != 'RK') %>%
  mutate(salary = as.numeric(gsub('\\$|,', '', salary))) %>% 
  separate(name, into = c('name', 'position'), sep = ',', remove = T) %>%
  mutate(rank = as.numeric(rank)) %>% arrange(season, rank) %>%
  mutate(season = ifelse(season == 'es/_', 2019, season)) %>%
  select(-'rank', -'position', -'team') %>%
  rename(Player=name)



### Import Player Stats CSVs downloaded from Cleaning The Glass
### and 538's Modern RAPTOR by player
### https://cleaningtheglass.com/stats/players
### https://github.com/fivethirtyeight/data/blob/master/nba-raptor

# 538's Modern RAPTOR by player
players.RAPTOR <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/538 data/modern_RAPTOR_by_player.csv")

# Cleaning the Glass 2018 season data (442 rows each)
players.defense.rebounding.2018 <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/Cleaning the Glass data/2018 stats/players_defense_rebounding_2018.csv")
players.offensive.overview.2018 <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/Cleaning the Glass data/2018 stats/players_offensive_overview_2018.csv")
players.onoff.efficiency.2018 <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/Cleaning the Glass data/2018 stats/players_onoff_efficiency_2018.csv")
players.shooting.overall.2018 <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/Cleaning the Glass data/2018 stats/players_shooting_overall_2018.csv")

# Cleaning the Glass 2019 season data (444 rows each)
players.defense.rebounding.2019 <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/Cleaning the Glass data/2019 stats/players_defense_rebounding_2019.csv")
players.offensive.overview.2019 <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/Cleaning the Glass data/2019 stats/players_offensive_overview_2019.csv")
players.onoff.efficiency.2019 <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/Cleaning the Glass data/2019 stats/players_onoff_efficiency_2019.csv")
players.shooting.overall.2019 <- read_csv("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project/Cleaning the Glass data/2019 stats/players_shooting_overall_2019.csv")



### CLEAN: 538's Modern RAPTOR by player
players.RAPTOR = players.RAPTOR %>%
  rename(Player=player_name) %>%
  select(-'player_id', -'poss', -'mp')


### CLEAN: Cleaning the Glass 2018 season data

# If player listed on multiple teams in the same season, 
# keep only the row with the most playing minutes
players.defense.rebounding.2018 <- players.defense.rebounding.2018[order(
  players.defense.rebounding.2018[,'Player'],
  -players.defense.rebounding.2018[,'MIN']),]
players.defense.rebounding.2018 <- players.defense.rebounding.2018[!duplicated(
  players.defense.rebounding.2018$Player),]

players.offensive.overview.2018 <- players.offensive.overview.2018[order(
  players.offensive.overview.2018[,'Player'],
  -players.offensive.overview.2018[,'MIN']),]
players.offensive.overview.2018 <- players.offensive.overview.2018[!duplicated(
  players.offensive.overview.2018$Player),]

players.onoff.efficiency.2018 <- players.onoff.efficiency.2018[order(
  players.onoff.efficiency.2018[,'Player'],
  -players.onoff.efficiency.2018[,'MIN']),]
players.onoff.efficiency.2018 <- players.onoff.efficiency.2018[!duplicated(
  players.onoff.efficiency.2018$Player),]

players.shooting.overall.2018 <- players.shooting.overall.2018[order(
  players.shooting.overall.2018[,'Player'],
  -players.shooting.overall.2018[,'MIN']),]
players.shooting.overall.2018 <- players.shooting.overall.2018[!duplicated(
  players.shooting.overall.2018$Player),]

# Remove non-ranked stats
players.defense.rebounding.2018 <- select(players.defense.rebounding.2018, 
                                          -c('Team', 'BLK%', 'Pos', 'STL%',
                                             'FOUL%', 'fgOR%', 'fgDR%', 'ftOR%', 
                                             'ftOR% Rank', 'ftDR%', 
                                             'ftDR% Rank'))

players.offensive.overview.2018 <- select(players.offensive.overview.2018,
                                          -c('Team', 'Age', 'Pos', 'MIN', 
                                             'Usage', 'PSA', 'AST%', 'AST:Usg', 
                                             'TOV%'))

players.onoff.efficiency.2018 <- select(players.onoff.efficiency.2018,
                                        -c('Team', 'Age', 'Pos', 'MIN', 'Diff', 
                                           'Exp W', 'OFFENSE: Pts/Poss', 
                                           'OFFENSE: eFG%', 'OFFENSE: TOV%', 
                                           'OFFENSE: ORB%', 'OFFENSE: FT Rate', 
                                           'DEFENSE: Pts/Poss', 'DEFENSE: eFG%',
                                           'DEFENSE: TOV%', 'DEFENSE: ORB%',
                                           'DEFENSE: FT Rate'))

players.shooting.overall.2018 <- select(players.shooting.overall.2018,
                                        -c('Team', 'Age', 'Pos', 'MIN', 'eFG%',
                                           '2P%', '3P%', 'FT%', 'ASTD% All',
                                           'ASTD% Rim', 'ASTD% Mid', 
                                           'ASTD% Three'))



### CLEAN: Cleaning the Glass 2019 season data

# If player listed on multiple teams in the same season, 
# keep only the row with the most playing minutes
players.defense.rebounding.2019 <- players.defense.rebounding.2019[order(
  players.defense.rebounding.2019[,'Player'],
  -players.defense.rebounding.2019[,'MIN']),]
players.defense.rebounding.2019 <- players.defense.rebounding.2019[!duplicated(
  players.defense.rebounding.2019$Player),]

players.offensive.overview.2019 <- players.offensive.overview.2019[order(
  players.offensive.overview.2019[,'Player'],
  -players.offensive.overview.2019[,'MIN']),]
players.offensive.overview.2019 <- players.offensive.overview.2019[!duplicated(
  players.offensive.overview.2019$Player),]

players.onoff.efficiency.2019 <- players.onoff.efficiency.2019[order(
  players.onoff.efficiency.2019[,'Player'],
  -players.onoff.efficiency.2019[,'MIN']),]
players.onoff.efficiency.2019 <- players.onoff.efficiency.2019[!duplicated(
  players.onoff.efficiency.2019$Player),]

players.shooting.overall.2019 <- players.shooting.overall.2019[order(
  players.shooting.overall.2019[,'Player'],
  -players.shooting.overall.2019[,'MIN']),]
players.shooting.overall.2019 <- players.shooting.overall.2019[!duplicated(
  players.shooting.overall.2019$Player),]

# Remove non-ranked stats
players.defense.rebounding.2019 <- select(players.defense.rebounding.2019, 
                                          -c('Team', 'BLK%', 'Pos', 'STL%', 
                                             'FOUL%', 'fgOR%', 'fgDR%', 'ftOR%', 
                                             'ftOR% Rank', 'ftDR%', 
                                             'ftDR% Rank'))

players.offensive.overview.2019 <- select(players.offensive.overview.2019,
                                          -c('Team', 'Age', 'Pos', 'MIN', 
                                             'Usage', 'PSA', 'AST%', 'AST:Usg', 
                                             'TOV%'))

players.onoff.efficiency.2019 <- select(players.onoff.efficiency.2019,
                                        -c('Team', 'Age', 'Pos', 'MIN', 'Diff', 
                                           'Exp W', 'OFFENSE: Pts/Poss', 
                                           'OFFENSE: eFG%', 'OFFENSE: TOV%', 
                                           'OFFENSE: ORB%', 'OFFENSE: FT Rate', 
                                           'DEFENSE: Pts/Poss', 'DEFENSE: eFG%',
                                           'DEFENSE: TOV%', 'DEFENSE: ORB%',
                                           'DEFENSE: FT Rate'))

players.shooting.overall.2019 <- select(players.shooting.overall.2019,
                                        -c('Team', 'Age', 'Pos', 'MIN', 'eFG%',
                                           '2P%', '3P%', 'FT%', 'ASTD% All',
                                           'ASTD% Rim', 'ASTD% Mid', 
                                           'ASTD% Three'))



### COMBINE: Cleaning the four Glass season datasets,
### Player Salaries from ESPN, and 538's Modern RAPTOR by player

# 2018 season
stats.2018 <- players.defense.rebounding.2018 %>%
  left_join(players.offensive.overview.2018, by='Player') %>%
  left_join(players.onoff.efficiency.2018, by='Player') %>%
  left_join(players.shooting.overall.2018, by='Player') %>%
  left_join(salaries[salaries$season==2018,], by='Player') %>%
  left_join(players.RAPTOR[players.RAPTOR$season==2018,], by='Player')
dim(stats.2018)  # 442x51

# 2019 season
stats.2019 <- players.defense.rebounding.2019 %>%
  left_join(players.offensive.overview.2019, by='Player') %>%
  left_join(players.onoff.efficiency.2019, by='Player') %>%
  left_join(players.shooting.overall.2019, by='Player') %>%
  left_join(salaries[salaries$season==2019,], by='Player') %>%
  left_join(players.RAPTOR[players.RAPTOR$season==2019,], by='Player')
dim(stats.2019)  # 444x51



### CLEAN: stats.2018 and stats.2019

# Mutate salaries from $ to '% of salary cap'
# and remove rows with NA values
salary.cap.2018 <- 99093000
salary.cap.2019 <- 101869000
stats.2018 <- stats.2018 %>%
  mutate(salary = salary / salary.cap.2018) %>%
  drop_na
stats.2019 <- stats.2019 %>%
  mutate(salary = salary / salary.cap.2019) %>%
  drop_na
dim(stats.2018) # 351x51
dim(stats.2019) # 403x51  

# Remove columns: season.x, season.y, Player
# Move salary to first column

# 2018
stats.2018 <- stats.2018 %>% 
  select(-c('season.x', 'season.y', 'Player')) %>%
  relocate('salary', .after = 'pace_impact')
dim(stats.2018)  # 174x48
head(stats.2018)

# 2019
stats.2019 <- stats.2019 %>% 
  select(-c('season.x', 'season.y', 'Player')) %>%
  relocate('salary', .after = 'pace_impact')
dim(stats.2019)  # 200x48


setwd("/Users/matt/Library/Mobile Documents/com~apple~CloudDocs/Documents/OMSA/current courses/Fall 2022 - ISYE 7406/Project")
write_csv(stats.2019, 'stats 2019.csv')



#################################################
####### Part 2: Exploratory Data Analysis #######
#################################################

# Plot showing salary growth over the past decade (2013-2022 seasons)
plot.salaries = salaries %>%
  mutate(season = factor(season, levels = 2022:2013)) %>%
  ggplot(., aes(x = salary, y = season, fill = ..x..)) +
  geom_density_ridges_gradient(scale = 5, rel_min_height = 0.01) +
  scale_fill_viridis(option = 'C') + 
  scale_x_continuous(labels = unit_format(unit = "M", scale = 1e-6, sep = '')) + 
  labs(y = 'Season', x = 'Salary', 
       title = 'Distribution of NBA Salaries Over the Last Decade', subtitle = '2013-2022',
       caption = 'Source: ESPN') + 
  theme_minimal() +
  theme(legend.position = 'none', 
        text = element_text(size = 15))

plot.salaries



#########################################################
####### Part 3: Train Algorithms Using 2018 Stats #######
#########################################################

# Control, Pre-Processing, Tune Grid
control <- trainControl(method = "repeatedcv", 
                        number = 10, # number of folds
                        repeats = 5, # number of repeats
                        search = 'random',
                        verbose = FALSE)
prep <- c('scale', 'center')
tunegrid <- data.frame(num_trees=500,k=3,alpha=0.1,beta=0.1,nu=4)


# LASSO
set.seed(13)
lasso.model <- train(salary ~., 
                     data = stats.2018,
                     method = 'lasso',
                     trControl = control,
                     preProcess = prep)

# Ridge
set.seed(13)
ridge.model <- train(salary ~., 
                     data = stats.2018,
                     method = 'ridge',
                     trControl = control,
                     preProcess = prep)

# Random Forest (method = 'ranger')
set.seed(13)
rf.model <- train(salary ~., 
                  data = stats.2018,
                  method = 'ranger',
                  trControl = control,
                  preProcess = prep,
                  importance = "impurity")

# Bagged MARS (method = 'bagEarth')
set.seed(13)
baggedMARS.model <- train(salary ~.,
                          data = stats.2018,
                          method = 'bagEarth',
                          trControl = control,
                          preProcess = prep)

# Neural Network (method = 'nnet')
set.seed(13)
neuralNetwork.model <- train(salary ~.,
                             data = stats.2018,
                             method = 'nnet',
                             trControl = control,
                             preProcess = prep,
                             linout = TRUE)

# Support Vector Machines with Polynomial Kernel (method = 'svmPoly')
set.seed(13)
svmPoly.model <- train(salary ~.,
                       data = stats.2018,
                       method = 'svmPoly',
                       trControl = control,
                       preProcess = prep)

# Random Forest (method = 'Rborist')
set.seed(13)
Rborist.model <- train(salary ~.,
                       data = stats.2018,
                       method = 'Rborist',
                       trControl = control,
                       preProcess = prep)

# Stochastic Gradient Boosting (method = 'gbm')
set.seed(13)
gbm.model <- train(salary ~.,
                   data = stats.2018,
                   method = 'gbm',
                   trControl = control,
                   preProcess = prep)

# k-Nearest Neighbors (method = 'kknn')
set.seed(13)
kknn.model <- train(salary ~.,
                    data = stats.2018,
                    method = 'kknn',
                    trControl = control,
                    preProcess = prep)

# Gaussian Process with Polynomial Kernel (method = 'gaussprPoly')
set.seed(13)
gaussprPoly.model <- train(salary ~.,
                           data = stats.2018,
                           method = 'gaussprPoly',
                           trControl = control,
                           preProcess = prep)

# The Bayesian lasso (method = 'blasso')
set.seed(13)
blasso.model <- train(salary ~.,
                           data = stats.2018,
                           method = 'blasso',
                           trControl = control,
                           preProcess = prep)

# Conditional Inference Random Forest (method = 'cforest')
set.seed(13)
cforest.model <- train(salary ~.,
                       data = stats.2018,
                       method = 'cforest',
                       trControl = control,
                       preProcess = prep)

# Conditional Inference Random Forest (method = 'evtree')
set.seed(13)
evtree.model <- train(salary ~.,
                       data = stats.2018,
                       method = 'evtree',
                       trControl = control,
                       preProcess = prep)

# eXtreme Gradient Boosting (method = 'xgbTree')
set.seed(13)
xgbTree.model <- train(salary ~.,
                      data = stats.2018,
                      method = 'xgbTree',
                      trControl = control,
                      preProcess = prep)

# eXtreme Gradient Boosting (method = 'xgbDART')
set.seed(13)
xgbDART.model <- train(salary ~.,
                       data = stats.2018,
                       method = 'xgbDART',
                       trControl = control,
                       preProcess = prep)

# eXtreme Gradient Boosting (method = 'xgbLinear')
set.seed(13)
xgbLinear.model <- train(salary ~.,
                         data = stats.2018,
                         method = 'xgbLinear',
                         trControl = control,
                         preProcess = prep)

# RMSE ordered from smallest to largest
xgbLinear.model     # RMSE=0.02691536 # Accepts Case Weights
evtree.model        # RMSE=0.02713178 # Accepts Case Weights
cforest.model       # RMSE=0.02722145 # Bagging
rf.model            # RMSE=0.02753819 # Bagging
baggedMARS.model    # RMSE=0.02816532 # Accepts Case Weights
Rborist.model       # RMSE=0.02860917 # Bagging
blasso.model        # RMSE=0.02956862 # Bayesian Model
neuralNetwork.model # RMSE=0.02997196 # L2 Regularization
gaussprPoly.model   # RMSE=0.03002098 # Polynomial Model
xgbDART.model       # RMSE=0.03059197 # Supports Class Probabilities
xgbTree.model       # RMSE=0.03061082 # Supports Class Probabilities
kknn.model          # RMSE=0.03114384 # Supports Class Probabilities
svmPoly.model       # RMSE=0.03165134 # Kernel Method
lasso.model         # RMSE=0.03192150 # L1 Regularization
ridge.model         # RMSE=0.03210304 # L2 Regularization
gbm.model           # RMSE=0.03513144 # Supports Class Probabilities

varImp(xgbLinear.model)
# MIN                     100.000
# Age                      98.519
# war_reg_season           87.871
# `FOUL% Rank`             32.818
# `fgDR% Rank`             21.330
# war_total                18.366
# `Usage Rank`             15.724
# war_playoffs             14.612
# `fgOR% Rank`             11.825
# `OFFENSE: TOV% Rank`     10.668

varImp(cforest.model)
# varImpPlot(cforest.model)
# Age                     100.000
# war_total                59.865
# MIN                      46.727
# `FOUL% Rank`             25.972
# `Usage Rank`             22.858
# war_reg_season           20.849

varImp(rf.model)
# Age                      100.00
# MIN                       70.71
# `FOUL% Rank`              35.58
# `Usage Rank`              28.71
# `BLK% Rank`               22.08
# `DEFENSE: FT Rate Rank`   16.87
# `TOV% Rank`               16.41
# pace_impact               15.37
# `ASTD% Three Rank`        14.99
# `ASTD% All Rank`          14.24

varImp(gbm.model)
# Age                      100.00
# `OFFENSE: ORB% Rank`      38.49
# `STL% Rank`               27.41
# `fgDR% Rank`              26.10
# `AST:Usg Rank`            26.02



############################################################
####### Part 3: Measure Performance Using 2019 Stats #######
############################################################

xgbLinear.pred <- predict(xgbLinear.model, stats.2019)
evtree.pred <- predict(evtree.model, stats.2019)
cforest.pred <- predict(cforest.model, stats.2019)
rf.pred <- predict(rf.model, stats.2019)
baggedMARS.pred <- predict(baggedMARS.model, stats.2019)
Rborist.pred <- predict(Rborist.model, stats.2019)
blasso.pred <- predict(blasso.model, stats.2019)
neuralNetwork.pred <- predict(neuralNetwork.model, stats.2019)
gaussprPoly.pred <- predict(gaussprPoly.model, stats.2019)
xgbDART.pred <- predict(xgbDART.model, stats.2019)
xgbTree.pred <- predict(xgbTree.model, stats.2019)
kknn.pred <- predict(kknn.model, stats.2019)
lasso.pred <- predict(lasso.model, stats.2019)
svmPoly.pred <- predict(svmPoly.model, stats.2019)
ridge.pred <- predict(ridge.model, stats.2019)
gbm.pred <- predict(gbm.model, stats.2019)

postResample(pred = xgbLinear.pred, 
             obs = stats.2019$salary) # RMSE=0.03402403
postResample(pred = evtree.pred, 
             obs = stats.2019$salary) # RMSE=0.02899892
postResample(pred = cforest.pred, 
             obs = stats.2019$salary) # RMSE=0.02817672 # LOWEST
postResample(pred = rf.pred, 
             obs = stats.2019$salary) # RMSE=0.02850840 # 3rd LOWEST
postResample(pred = baggedMARS.pred, 
             obs = stats.2019$salary) # RMSE=0.02834600 # 2nd LOWEST
postResample(pred = Rborist.pred, 
             obs = stats.2019$salary) # RMSE=0.02916735
postResample(pred = blasso.pred, 
             obs = stats.2019$salary) # RMSE=0.03075874
postResample(pred = neuralNetwork.pred, 
             obs = stats.2019$salary) # RMSE=0.03118686
postResample(pred = gaussprPoly.pred, 
             obs = stats.2019$salary) # RMSE=0.03094781
postResample(pred = xgbDART.pred, 
             obs = stats.2019$salary) # RMSE=0.03262118
postResample(pred = xgbTree.pred, 
             obs = stats.2019$salary) # RMSE=0.03262704
postResample(pred = kknn.pred, 
             obs = stats.2019$salary) # RMSE=0.03172867
postResample(pred = lasso.pred, 
             obs = stats.2019$salary) # RMSE=0.03022447
postResample(pred = svmPoly.pred, 
             obs = stats.2019$salary) # RMSE=0.03339669
postResample(pred = ridge.pred, 
             obs = stats.2019$salary) # RMSE=0.03204033
postResample(pred = gbm.pred, 
             obs = stats.2019$salary) # RMSE=0.03285839
