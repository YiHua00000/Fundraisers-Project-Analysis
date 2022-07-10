library(tidyverse)
library(caret)
library(tree)
library(class)
library(glmnet)
library(ROCR) 
library(e1071)
library(randomForest)
library(gbm)
library(kernlab)
library(xgboost)
library(text2vec)
library(text2vec)
library(tm)
library(SnowballC)
library(vip)
library(ranger)
library(tidytext)
library(syuzhet)
library(xgboost)

small_train_X <- read.csv("ks_training_x.csv")
small_train_y <- read.csv("ks_training_y.csv")
test<-read.csv("ks_test_X.csv")

train <- small_train_X %>%
  left_join(small_train_y, by = "id") %>%
  mutate(success = as.factor(success))





train_pre <- train %>%
  select(id,reward_amounts)%>%
  separate(reward_amounts, into= c('reward_1', 'reward_2', 'reward_3', 'reward_4', 'reward_5', 'reward_6', 'reward_7'
                                   , 'reward_8', 'reward_9', 'reward_10', 'reward_11', 'reward_12', 'reward_13'
                                   , 'reward_14', 'reward_15', 'reward_16', 'reward_17', 'reward_18', 'reward_19', 'reward_20'
                                   , 'reward_21', 'reward_22', 'reward_23', 'reward_24', 'reward_25', 'reward_26'
                                   , 'reward_27', 'reward_28', 'reward_29', 'reward_30', 'reward_31', 'reward_32'
                                   , 'reward_33', 'reward_34', 'reward_35','reward_36','reward_37','reward_38'
                                   ,'reward_39','reward_40','reward_41','reward_42','reward_43','reward_44'
  ), sep=',')%>%
  select(id,'reward_1', 'reward_2', 'reward_3', 'reward_4', 'reward_5', 'reward_6', 'reward_7'
         , 'reward_8', 'reward_9', 'reward_10', 'reward_11', 'reward_12', 'reward_13'
         , 'reward_14', 'reward_15', 'reward_16', 'reward_17', 'reward_18', 'reward_19', 'reward_20'
         , 'reward_21', 'reward_22', 'reward_23', 'reward_24', 'reward_25', 'reward_26'
         , 'reward_27', 'reward_28', 'reward_29', 'reward_30', 'reward_31', 'reward_32'
         , 'reward_33', 'reward_34', 'reward_35','reward_36','reward_37','reward_38'
         ,'reward_39','reward_40','reward_41','reward_42','reward_43','reward_44')%>%
  mutate_if(is.character,as.numeric)%>%
  
  mutate(reward_1=ifelse(is.na(reward_1), 0, reward_1),
         reward_2=ifelse(is.na(reward_2), 0, reward_2),
         reward_3=ifelse(is.na(reward_3), 0, reward_3),
         reward_4=ifelse(is.na(reward_4), 0, reward_4),
         reward_5=ifelse(is.na(reward_5), 0, reward_5),
         reward_6=ifelse(is.na(reward_6), 0, reward_6),
         reward_7=ifelse(is.na(reward_7), 0, reward_7),
         reward_8=ifelse(is.na(reward_8), 0, reward_8),
         reward_9=ifelse(is.na(reward_9), 0, reward_9),
         reward_10=ifelse(is.na(reward_10), 0, reward_10),
         reward_11=ifelse(is.na(reward_11), 0, reward_11),
         reward_12=ifelse(is.na(reward_12), 0, reward_12),
         reward_13=ifelse(is.na(reward_13), 0, reward_13),
         reward_14=ifelse(is.na(reward_14), 0, reward_14),
         reward_15=ifelse(is.na(reward_15), 0, reward_15),
         reward_16=ifelse(is.na(reward_16), 0, reward_16),
         reward_17=ifelse(is.na(reward_17), 0, reward_17),
         reward_18=ifelse(is.na(reward_18), 0, reward_18),
         reward_19=ifelse(is.na(reward_19), 0, reward_19),
         reward_20=ifelse(is.na(reward_20), 0, reward_20),
         reward_21=ifelse(is.na(reward_21), 0, reward_21),
         reward_22=ifelse(is.na(reward_22), 0, reward_22),
         reward_23=ifelse(is.na(reward_23), 0, reward_23),
         reward_24=ifelse(is.na(reward_24), 0, reward_24),
         reward_25=ifelse(is.na(reward_25), 0, reward_25),
         reward_26=ifelse(is.na(reward_26), 0, reward_26),
         reward_27=ifelse(is.na(reward_27), 0, reward_27),
         reward_28=ifelse(is.na(reward_28), 0, reward_28),
         reward_29=ifelse(is.na(reward_29), 0, reward_29),
         reward_30=ifelse(is.na(reward_30), 0, reward_30),
         reward_31=ifelse(is.na(reward_31), 0, reward_31),
         reward_32=ifelse(is.na(reward_32), 0, reward_32),
         reward_33=ifelse(is.na(reward_33), 0, reward_33),
         reward_34=ifelse(is.na(reward_34), 0, reward_34),
         reward_35=ifelse(is.na(reward_35), 0, reward_35),
         reward_36=ifelse(is.na(reward_36), 0, reward_36),
         reward_37=ifelse(is.na(reward_37), 0, reward_37),
         reward_38=ifelse(is.na(reward_38), 0, reward_38),
         reward_39=ifelse(is.na(reward_39), 0, reward_39),
         reward_40=ifelse(is.na(reward_40), 0, reward_40),
         reward_41=ifelse(is.na(reward_41), 0, reward_41),
         reward_42=ifelse(is.na(reward_42), 0, reward_42),
         reward_43=ifelse(is.na(reward_43), 0, reward_43),
         reward_44=ifelse(is.na(reward_44), 0, reward_44))%>%
  mutate(
    
    total_reward=reward_1+reward_2+reward_3+reward_4+reward_5+reward_6+reward_7+
      reward_8+reward_9+reward_10+reward_11+reward_12+reward_13+
      reward_14+reward_15+reward_17+reward_18+reward_19+reward_20+
      reward_21+reward_22+reward_23+reward_24+reward_25+reward_26+
      reward_27+reward_28+reward_29+reward_30+reward_31+reward_32+
      reward_33+reward_34+reward_35+reward_36+reward_37+reward_38+
      reward_39+reward_40+reward_41+reward_42+reward_43+reward_44, 
      total_reward_rank = ntile(total_reward, 5),
      top_reward = as.factor(ifelse(total_reward_rank == 5, "YES", "NO")),

    max_reward= pmax(reward_1, reward_2, reward_3, reward_4, reward_5, reward_6, reward_7
                     , reward_8, reward_9, reward_10, reward_11, reward_12, reward_13
                     , reward_14, reward_15, reward_16, reward_17, reward_18, reward_19, reward_20
                     , reward_21, reward_22, reward_23, reward_24, reward_25, reward_26
                     , reward_27, reward_28, reward_29, reward_30, reward_31, reward_32
                     ,reward_33, reward_34, reward_35,reward_36,reward_37,reward_38
                     ,reward_39,reward_40,reward_41,reward_42,reward_43,reward_44),
    max_reward=(max_reward - min(max_reward)) / (max(max_reward) - min(max_reward)),
    min_reward=reward_1,
    min_reward=(min_reward - min(min_reward)) / (max(min_reward) - min(min_reward))
    
  )

train_pre<-train_pre%>%select(-c(reward_1, reward_2, reward_3, reward_4, reward_5, reward_6, reward_7
                                 , reward_8, reward_9, reward_10, reward_11, reward_12, reward_13
                                 , reward_14, reward_15, reward_16, reward_17, reward_18, reward_19, reward_20
                                 , reward_21, reward_22, reward_23, reward_24, reward_25, reward_26
                                 , reward_27, reward_28, reward_29, reward_30, reward_31, reward_32
                                 ,reward_33, reward_34, reward_35,reward_36,reward_37,reward_38
                                 ,reward_39,reward_40,reward_41,reward_42,reward_43,reward_44))




train_1 <- train %>%
  select(id,success,goal,deadline,launched_at,created_at,num_words,avg_wordlengths,sentence_counter,
         afinn_pos,afinn_neg,ADV,NOUN,ADP,PRT,DET,PRON,VERB,NUM,CONJ,ADJ,
         minage_project,maxage_project,color_foreground,
         male_project, female_project,category_parent,region,isTextPic,isLogoPic,isCalendarPic,
         isDiagramPic, isShapePic,  color_background, grade_level, contains_youtube,
         smiling_project, numfaces_project,numfaces_creator,avgsyls,avgsentencelength,
         male_creator, female_creator,maxage_creator,minage_creator
         ,numfaces_project, numfaces_creator,smiling_project, smiling_creator, creator_id,isbwImg1,tag_names,
         blurb, captions,category_name,name
         
  ) %>%
  mutate(blurb_length=lengths(strsplit(blurb, split=" ")),
         blurb_length=(blurb_length - min(blurb_length)) / (max(blurb_length) - min(blurb_length)))%>%
  mutate(captions_length=lengths(strsplit(captions, split=" ")),
         captions_length=(captions_length - min(captions_length)) / (max(captions_length) - min(captions_length))) %>%
  mutate(faces_creator=case_when(
    numfaces_creator >0 & smiling_creator>0.7~ 'smiling Faces'  ,
    numfaces_creator >0 & smiling_creator<0.7~ 'Faces'  ,
    numfaces_creator==0~'View'),
    
    faces_creator=as.factor(faces_creator))%>%
  mutate(faces_project=case_when(
    numfaces_project >0  ~ 'Faces'  ,
    
    numfaces_project==0~'View'),
    
    faces_project=as.factor(faces_project))%>%
  mutate(tag_names_length=lengths(strsplit(tag_names, "[|]")),
         tag_names_length=(tag_names_length - min(tag_names_length)) / (max(tag_names_length) - min(tag_names_length)))%>%
  mutate(
    color_foreground=ifelse(is.na(color_foreground), "None", color_foreground),
    color_foreground=case_when(
      color_foreground%in%c("Blue", "Orange","Pink","Purple", "Red")~ 'Light',
      color_foreground%in%c("Yellow", "White", "Green", "Brown","Teal")~ 'Clean',
      TRUE ~ 'None'
    ),
    color_foreground=as.factor(color_foreground))%>%

  mutate(
    color_background=ifelse(is.na(color_background), "None", color_background),
    color_background=case_when(
      color_background%in%c("Teal", "Purple")~ 'Clean',
      color_background%in%c("Red")~ 'Too light',
      
      TRUE ~ 'None'
    ),
    color_background=as.factor(color_background)
  )%>%
  mutate(isbwImg1=as.factor(ifelse(is.na(isbwImg1), "FALSE", isbwImg1)))%>%
  mutate(color=case_when(
    color_background%in%c('Clean')& isbwImg1%in%c('FALSE')~ '1',
    color_background%in%c('Clean')& isbwImg1%in%c('TRUE')~'2',
    color_background%in%c('Too light')& isbwImg1%in%c('FALSE')~ '3',
    color_background%in%c('Too light')& isbwImg1%in%c('TRUE')~ '4',
    color_background%in%c('None')& isbwImg1%in%c('FALSE')~ '5',
    color_background%in%c('None')& isbwImg1%in%c('TRUE')~ '6',
    TRUE ~ 'None' ),
    color=as.factor(color))%>%

  group_by(creator_id, category_parent) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  mutate(rank = ntile(total, 5),
         top_creator = as.factor(ifelse(rank == 5, "YES", "NO")))%>%

  mutate(male_or_female_project = case_when(
    male_project <female_project ~ 'F',
    male_project >female_project ~ 'M',
    TRUE ~ 'U'
  ),
  male_or_female_project = as.factor(male_or_female_project))%>%
  mutate(minage_creator = case_when(
    minage_creator <= 1 ~ 'Infant' ,
    minage_creator >1 & minage_creator <= 4 ~ 'Toddler'  ,
    minage_creator >4 & minage_creator <= 11 ~ 'Kids'   ,
    minage_creator >11 & minage_creator <= 18 ~ 'Adolscent'    ,
    minage_creator >18 & minage_creator <= 30 ~ 'Young_Adults'  ,
    minage_creator >30 & minage_creator <=45 ~ 'Mid_Adults',
    minage_creator >45 & minage_creator <=60 ~ 'Old_Adults',
    minage_creator > 60 ~ 'Senior_Citizens'),
    minage_creator=as.factor(minage_creator))%>%
  
  mutate(maxage_creator = case_when(
    maxage_creator <= 1 ~ 'Infant' ,
    maxage_creator >1 & maxage_creator <= 4 ~ 'Toddler'  ,
    maxage_creator >4 & maxage_creator <= 11 ~ 'Kids'   ,
    maxage_creator >11 & maxage_creator <= 18 ~ 'Adolscent'    ,
    maxage_creator >18 & maxage_creator <= 30 ~ 'Young_Adults'  ,
    maxage_creator >30 & maxage_creator <=45 ~ 'Mid_Adults',
    maxage_creator >45 & maxage_creator <=60 ~ 'Old_Adults',
    maxage_creator > 60 ~ 'Senior_Citizens'),
    maxage_creator=as.factor(maxage_creator)) %>%
  
  mutate(
    creator_age = case_when(
      minage_creator %in% c('Adolscent','Young_Adults') & maxage_creator %in% c('Adolscent','Young_Adults')~ 'More creative',
      TRUE~"Less creative"
    ),
    creator_age=as.factor(creator_age)
  )%>%
  mutate(male_or_female_creator = case_when(
    male_creator <female_creator ~ 'F',
    male_creator >female_creator ~ 'M',
    TRUE ~ 'U'
  ),
  male_or_female_creator = as.factor(male_or_female_creator))%>%
  mutate(avgsentencelength=(avgsentencelength - min(avgsentencelength)) / (max(avgsentencelength) - min(avgsentencelength)))%>%
  mutate(avgsyls=(avgsyls - min(avgsyls)) / (max(avgsyls) - min(avgsyls)))%>%

  mutate(contains_youtube=as.factor(contains_youtube)) %>%
  mutate(grade_level = case_when(
    grade_level <0 ~ 'Too basic',
    grade_level >=0 & grade_level <15 ~ 'Fair',
    grade_level >= 15 & grade_level <18 ~ 'Advanced',
    TRUE~"Skilled"),
    grade_level = as.factor(grade_level)) %>%
  
  mutate(profile_pict_type = case_when(
    isTextPic == 1 ~ 'Text',
    isLogoPic == 1 ~ 'Logo',
    isCalendarPic == 1 ~ 'Calendar',
    isDiagramPic == 1 ~ 'Diagram',
    isShapePic == 1 ~ 'Shape',
    TRUE ~  'None'
  ),
  profile_pict_type=case_when(
    profile_pict_type %in% c('Calendar', 'Diagram') ~ 'Great',
    TRUE ~  'Not good'
  ),
  profile_pict_type=as.factor(profile_pict_type)) %>%
  
  
  mutate(
    
    region=as.factor(region)
  )%>%
  
  
  left_join(train_pre, by = "id") %>%
  
  
  
  mutate(more_reward=(total_reward/goal),
         more_reward=(more_reward - min(more_reward)) / (max(more_reward) - min(more_reward)),
         total_reward=(total_reward - min(total_reward)) / (max(total_reward) - min(total_reward)))%>%
  mutate( success=as.factor(success)  )%>%
  mutate(goal_rank = ntile(goal, 5),
         top_goal = as.factor(ifelse(goal_rank == 5, "YES", "NO")))%>%
  mutate(goal=(goal - min(goal)) / (max(goal) - min(goal)) )%>%
  mutate(
    category_parent = as.factor(category_parent)
  ) %>%
  mutate(minage_project = case_when(
    minage_project <= 1 ~ 'Infant' ,
    minage_project >1 & minage_project <= 4 ~ 'Toddler'  ,
    minage_project >4 & minage_project <= 11 ~ 'Kids'   ,
    minage_project >11 & minage_project <= 18 ~ 'Adolscent'    ,
    minage_project >18 & minage_project <= 30 ~ 'Young_Adults'  ,
    minage_project >30 & minage_project <=45 ~ 'Mid_Adults',
    minage_project >45 & minage_project <=60 ~ 'Old_Adults',
    minage_project > 60 ~ 'Senior_Citizens'),
    minage_project=as.factor(minage_project))%>%
  
  mutate(maxage_project = case_when(
    maxage_project <= 1 ~ 'Infant' ,
    maxage_project >1 & maxage_project <= 4 ~ 'Toddler'  ,
    maxage_project >4 & maxage_project <= 11 ~ 'Kids'   ,
    maxage_project >11 & maxage_project <= 18 ~ 'Adolscent'    ,
    maxage_project >18 & maxage_project <= 30 ~ 'Young_Adults'  ,
    maxage_project >30 & maxage_project <=45 ~ 'Mid_Adults',
    maxage_project >45 & maxage_project <=60 ~ 'Old_Adults',
    maxage_project > 60 ~ 'Senior_Citizens'),
    maxage_project=as.factor(maxage_project)) %>%
  mutate(
    project_age = case_when(
      
      minage_project %in% c('Young_Adults', 'Mid_Adults', 'Old_Adults','Senior_Citizens') & maxage_project %in% c('Young_Adults', 'Mid_Adults', 'Old_Adults','Senior_Citizens')~ 'Adults',
      TRUE~"Other"
    ),
    project_age=as.factor(project_age)
  )%>%

  
  
  mutate(deadline =as.Date(deadline),
         launched_at= as.Date(launched_at), 
         created_at= as.Date(created_at),
         duration=as.numeric(difftime(deadline, launched_at, units = "days")),
         duration=(duration - min(duration)) / (max(duration) - min(duration)),
         prepare_time=as.numeric(difftime(launched_at, created_at, units = "days")),
         prepare_time=(prepare_time - min(prepare_time)) / (max(prepare_time) - min(prepare_time))
         
  )%>% mutate(
    afinn_pos_precentage=(afinn_pos/num_words),
    afinn_pos_precentage=ifelse(is.na(afinn_pos_precentage), 0, afinn_pos_precentage),
    afinn_neg_precentage=(afinn_neg/num_words),
    afinn_neg_precentage=ifelse(is.na(afinn_neg_precentage), 0, afinn_neg_precentage),
    attitude=ifelse(afinn_pos_precentage>=afinn_neg_precentage, "positive","negative"),
    attitude=as.factor(attitude)
  )%>%
  
  mutate(avg_wordlengths=(avg_wordlengths - min(avg_wordlengths)) / (max(avg_wordlengths) - min(avg_wordlengths)))%>%
  mutate(sentence_counter=(sentence_counter - min(sentence_counter)) / (max(sentence_counter) - min(sentence_counter)))%>%
  
  mutate(ADV=(ADV/num_words),
         ADV=ifelse(is.na(ADV), 0, ADV),
         NOUN=(NOUN/num_words),
         NOUN=ifelse(is.na(NOUN), 0, NOUN),
         ADP=(ADP/num_words),
         ADP=ifelse(is.na(ADP), 0, ADP),
         PRT=(PRT/num_words),
         PRT=ifelse(is.na(PRT), 0, PRT),
         DET=(DET/num_words),
         DET=ifelse(is.na(DET), 0, DET),
         PRON=(PRON/num_words),
         PRON=ifelse(is.na(PRON), 0, PRON),
         VERB=(VERB/num_words),
         VERB=ifelse(is.na(VERB), 0, VERB),
         NUM=(NUM/num_words),
         NUM=ifelse(is.na(NUM), 0, NUM),
         CONJ=(CONJ/num_words),
         CONJ=ifelse(is.na(CONJ), 0, CONJ),
         ADJ=(ADJ/num_words),
         ADJ=ifelse(is.na(ADJ), 0, ADJ),
  )%>%
  mutate(num_words=(num_words - min(num_words)) / (max(num_words) - min(num_words)))%>%
  separate(launched_at, into= c('launched_at_year', 'launched_at_month', 'launched_at_day'), sep='-')%>%
  group_by(launched_at_year)%>%
  mutate(launched_at_year_number=n())%>%
  ungroup()%>%
  mutate(launched_at_year_rank = ntile(launched_at_year_number, 5),
         top_launched_at_year = as.factor(ifelse(launched_at_year_rank == 5, "YES", "NO")))%>%
  group_by(launched_at_month)%>%
  mutate(launched_at_month_number=n())%>%
  ungroup()%>%
  mutate(launched_at_month_rank = ntile(launched_at_month_number, 5),
         top_launched_at_month = as.factor(ifelse(launched_at_month_rank == 5, "YES", "NO")))%>%
  separate(deadline, into= c('deadline_year', 'deadline_month', 'deadline_day'), sep='-')%>%
  group_by(deadline_year)%>%
  mutate(deadline_year_number=n())%>%
  ungroup()%>%
  mutate(deadline_year_rank = ntile(deadline_year_number, 5),
         top_deadline_year = as.factor(ifelse(deadline_year_rank == 5, "YES", "NO")))%>%
  group_by(launched_at_month)%>%
  mutate(deadline_month_number=n())%>%
  ungroup()%>%
  mutate(deadline_month_rank = ntile(deadline_month_number, 5),
         top_deadline_month = as.factor(ifelse(deadline_month_rank == 5, "YES", "NO")))%>%
  separate(created_at, into= c('created_at_year', 'created_at_month', 'created_at_day'), sep='-')%>%
  group_by(created_at_year)%>%
  mutate(created_at_year_number=n())%>%
  ungroup()%>%
  mutate(created_at_year_rank = ntile(created_at_year_number, 5),
         top_created_at_year = as.factor(ifelse(created_at_year_rank == 5, "YES", "NO")))%>%
  group_by(created_at_month)%>%
  mutate(created_at_month_number=n())%>%
  ungroup()%>%
  mutate(created_at_month_rank = ntile(created_at_month_number, 5),
         top_created_at_month = as.factor(ifelse(created_at_month_rank == 5, "YES", "NO")))%>%
  group_by(category_parent, category_name)%>%
  mutate(category_number=n())%>%
  ungroup()%>%
  mutate(category_rank = ntile(category_number, 5),
         top_category_rank = as.factor(ifelse(category_rank == 5, "YES", "NO")))%>%
  mutate(
    blurb_sentiment=get_sentiment(blurb, method="afinn"),
    blurb_sentiment=as.factor(ifelse(blurb_sentiment<=-6, "negative" ,ifelse(blurb_sentiment<=4, "neutral", "positive"))))%>%
  mutate(
    name_sentiment=get_sentiment(name, method="afinn"),
    name_sentiment=as.factor(ifelse(name_sentiment<=-1, "negative" ,ifelse(name_sentiment<=1, "neutral", "positive"))))%>%
  mutate(
    captions_sentiment=get_sentiment(captions, method="afinn"),
    captions_sentiment=as.factor(ifelse(captions_sentiment<=-2, "negative" , "positive")))



prep_fun = tolower
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% 
    removePunctuation %>% 
    removeWords(stopwords(kind="en")) %>% 
    stemDocument %>%
    word_tokenizer 
}
tok_fun = cleaning_tokenizer


it_train = itoken(train_1$blurb, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = train_1$id, 
                  progressbar = FALSE)


vocab = create_vocabulary(it_train)
vocab_small = prune_vocabulary(vocab, vocab_term_max = 15240)
vectorizer = vocab_vectorizer(vocab_small)
dtm_train = create_dtm(it_train, vectorizer)
dtm_train_bin <- dtm_train>0+0 

dtm_train_small <- dtm_train_bin[,1:10]

dense = as.matrix(dtm_train_small)+0

train_1 <- cbind(train_1, dense)



train_1<-train_1%>%
  mutate(topwrods=case_when(
    help == 1~ 'YES',
    new == 1~ 'YES'  ,
    film == 1~ 'YES'  ,
    will == 1~ 'YES'  ,
    music == 1~ 'YES'  ,
    make == 1~ 'YES'  ,
    stori == 1~ 'YES'  ,
    art == 1~ 'YES'  ,
    world == 1~ 'YES'  ,
    game == 1~ 'YES'  ,
    TRUE~"NO"
  ),
  topwrods=as.factor(topwrods))







model_data<-train_1%>%
  select(success, goal,duration,sentence_counter,prepare_time,total_reward, max_reward,more_reward,min_reward,
         ADV,ADP,PRT,DET,PRON,VERB,NUM,CONJ,ADJ,avgsyls,avgsentencelength,afinn_pos_precentage,afinn_neg_precentage,
         male_or_female_creator, male_or_female_project , color_foreground,project_age,category_parent, profile_pict_type
         , region,grade_level,color,contains_youtube,creator_age,faces_project,top_creator,tag_names_length,
         faces_creator, captions_length,blurb_length,launched_at_year_number, top_launched_at_year,top_launched_at_month,
         top_deadline_year, top_deadline_month,top_created_at_year,top_created_at_month,top_category_rank,top_goal,
         blurb_sentiment,topwrods, name_sentiment, captions_sentiment, top_goal,top_reward)



set.seed(1000)
train_rows <- sample(nrow(model_data),.7*nrow(model_data))
tr <- model_data[train_rows,]
va <- model_data[-train_rows,] 



rf.mod_data <- randomForest(success~.,
                       data=tr,
                       mtry=36, ntree=3000,
                       importance=TRUE)

rf_preds <- predict(rf.mod_data, newdata=va)
rf_acc <- mean(ifelse(rf_preds==va$success,1,0))
rf_acc




test_pre <- test %>%
  select(id,reward_amounts)%>%
  separate(reward_amounts, into= c('reward_1', 'reward_2', 'reward_3', 'reward_4', 'reward_5', 'reward_6', 'reward_7'
                                   , 'reward_8', 'reward_9', 'reward_10', 'reward_11', 'reward_12', 'reward_13'
                                   , 'reward_14', 'reward_15', 'reward_16', 'reward_17', 'reward_18', 'reward_19', 'reward_20'
                                   , 'reward_21', 'reward_22', 'reward_23', 'reward_24', 'reward_25', 'reward_26'
                                   , 'reward_27', 'reward_28', 'reward_29', 'reward_30', 'reward_31', 'reward_32'
                                   , 'reward_33', 'reward_34', 'reward_35','reward_36','reward_37','reward_38'
                                   ,'reward_39','reward_40','reward_41','reward_42','reward_43','reward_44'
  ), sep=',')%>%
  select(id,'reward_1', 'reward_2', 'reward_3', 'reward_4', 'reward_5', 'reward_6', 'reward_7'
         , 'reward_8', 'reward_9', 'reward_10', 'reward_11', 'reward_12', 'reward_13'
         , 'reward_14', 'reward_15', 'reward_16', 'reward_17', 'reward_18', 'reward_19', 'reward_20'
         , 'reward_21', 'reward_22', 'reward_23', 'reward_24', 'reward_25', 'reward_26'
         , 'reward_27', 'reward_28', 'reward_29', 'reward_30', 'reward_31', 'reward_32'
         , 'reward_33', 'reward_34', 'reward_35','reward_36','reward_37','reward_38'
         ,'reward_39','reward_40','reward_41','reward_42','reward_43','reward_44')%>%
  mutate_if(is.character,as.numeric)%>%
  
  mutate(reward_1=ifelse(is.na(reward_1), 0, reward_1),
         reward_2=ifelse(is.na(reward_2), 0, reward_2),
         reward_3=ifelse(is.na(reward_3), 0, reward_3),
         reward_4=ifelse(is.na(reward_4), 0, reward_4),
         reward_5=ifelse(is.na(reward_5), 0, reward_5),
         reward_6=ifelse(is.na(reward_6), 0, reward_6),
         reward_7=ifelse(is.na(reward_7), 0, reward_7),
         reward_8=ifelse(is.na(reward_8), 0, reward_8),
         reward_9=ifelse(is.na(reward_9), 0, reward_9),
         reward_10=ifelse(is.na(reward_10), 0, reward_10),
         reward_11=ifelse(is.na(reward_11), 0, reward_11),
         reward_12=ifelse(is.na(reward_12), 0, reward_12),
         reward_13=ifelse(is.na(reward_13), 0, reward_13),
         reward_14=ifelse(is.na(reward_14), 0, reward_14),
         reward_15=ifelse(is.na(reward_15), 0, reward_15),
         reward_16=ifelse(is.na(reward_16), 0, reward_16),
         reward_17=ifelse(is.na(reward_17), 0, reward_17),
         reward_18=ifelse(is.na(reward_18), 0, reward_18),
         reward_19=ifelse(is.na(reward_19), 0, reward_19),
         reward_20=ifelse(is.na(reward_20), 0, reward_20),
         reward_21=ifelse(is.na(reward_21), 0, reward_21),
         reward_22=ifelse(is.na(reward_22), 0, reward_22),
         reward_23=ifelse(is.na(reward_23), 0, reward_23),
         reward_24=ifelse(is.na(reward_24), 0, reward_24),
         reward_25=ifelse(is.na(reward_25), 0, reward_25),
         reward_26=ifelse(is.na(reward_26), 0, reward_26),
         reward_27=ifelse(is.na(reward_27), 0, reward_27),
         reward_28=ifelse(is.na(reward_28), 0, reward_28),
         reward_29=ifelse(is.na(reward_29), 0, reward_29),
         reward_30=ifelse(is.na(reward_30), 0, reward_30),
         reward_31=ifelse(is.na(reward_31), 0, reward_31),
         reward_32=ifelse(is.na(reward_32), 0, reward_32),
         reward_33=ifelse(is.na(reward_33), 0, reward_33),
         reward_34=ifelse(is.na(reward_34), 0, reward_34),
         reward_35=ifelse(is.na(reward_35), 0, reward_35),
         reward_36=ifelse(is.na(reward_36), 0, reward_36),
         reward_37=ifelse(is.na(reward_37), 0, reward_37),
         reward_38=ifelse(is.na(reward_38), 0, reward_38),
         reward_39=ifelse(is.na(reward_39), 0, reward_39),
         reward_40=ifelse(is.na(reward_40), 0, reward_40),
         reward_41=ifelse(is.na(reward_41), 0, reward_41),
         reward_42=ifelse(is.na(reward_42), 0, reward_42),
         reward_43=ifelse(is.na(reward_43), 0, reward_43),
         reward_44=ifelse(is.na(reward_44), 0, reward_44))%>%
  mutate(
    
    total_reward=reward_1+reward_2+reward_3+reward_4+reward_5+reward_6+reward_7+
      reward_8+reward_9+reward_10+reward_11+reward_12+reward_13+
      reward_14+reward_15+reward_17+reward_18+reward_19+reward_20+
      reward_21+reward_22+reward_23+reward_24+reward_25+reward_26+
      reward_27+reward_28+reward_29+reward_30+reward_31+reward_32+
      reward_33+reward_34+reward_35+reward_36+reward_37+reward_38+
      reward_39+reward_40+reward_41+reward_42+reward_43+reward_44, 
    total_reward_rank = ntile(total_reward, 5),
    top_reward = as.factor(ifelse(total_reward_rank == 5, "YES", "NO")),
    
    max_reward= pmax(reward_1, reward_2, reward_3, reward_4, reward_5, reward_6, reward_7
                     , reward_8, reward_9, reward_10, reward_11, reward_12, reward_13
                     , reward_14, reward_15, reward_16, reward_17, reward_18, reward_19, reward_20
                     , reward_21, reward_22, reward_23, reward_24, reward_25, reward_26
                     , reward_27, reward_28, reward_29, reward_30, reward_31, reward_32
                     ,reward_33, reward_34, reward_35,reward_36,reward_37,reward_38
                     ,reward_39,reward_40,reward_41,reward_42,reward_43,reward_44),
    max_reward=(max_reward - min(max_reward)) / (max(max_reward) - min(max_reward)),
    min_reward=reward_1,
    min_reward=(min_reward - min(min_reward)) / (max(min_reward) - min(min_reward))
    
  )

test_pre<-test_pre%>%select(-c(reward_1, reward_2, reward_3, reward_4, reward_5, reward_6, reward_7
                                 , reward_8, reward_9, reward_10, reward_11, reward_12, reward_13
                                 , reward_14, reward_15, reward_16, reward_17, reward_18, reward_19, reward_20
                                 , reward_21, reward_22, reward_23, reward_24, reward_25, reward_26
                                 , reward_27, reward_28, reward_29, reward_30, reward_31, reward_32
                                 ,reward_33, reward_34, reward_35,reward_36,reward_37,reward_38
                                 ,reward_39,reward_40,reward_41,reward_42,reward_43,reward_44))




test_1 <- test %>%
  select(id,goal,deadline,launched_at,created_at,num_words,avg_wordlengths,sentence_counter,
         afinn_pos,afinn_neg,ADV,NOUN,ADP,PRT,DET,PRON,VERB,NUM,CONJ,ADJ,
         minage_project,maxage_project,color_foreground,
         male_project, female_project,category_parent,region,isTextPic,isLogoPic,isCalendarPic,
         isDiagramPic, isShapePic,  color_background, grade_level, contains_youtube,
         smiling_project, numfaces_project,numfaces_creator,avgsyls,avgsentencelength,
         male_creator, female_creator,maxage_creator,minage_creator
         ,numfaces_project, numfaces_creator,smiling_project, smiling_creator, creator_id,isbwImg1,tag_names,
         blurb, captions,category_name,name
         
  ) %>%
  mutate(blurb_length=lengths(strsplit(blurb, split=" ")),
         blurb_length=(blurb_length - min(blurb_length)) / (max(blurb_length) - min(blurb_length)))%>%
  mutate(captions_length=lengths(strsplit(captions, split=" ")),
         captions_length=(captions_length - min(captions_length)) / (max(captions_length) - min(captions_length))) %>%
  mutate(faces_creator=case_when(
    numfaces_creator >0 & smiling_creator>0.7~ 'smiling Faces'  ,
    numfaces_creator >0 & smiling_creator<0.7~ 'Faces'  ,
    numfaces_creator==0~'View'),
    
    faces_creator=as.factor(faces_creator))%>%
  mutate(faces_project=case_when(
    numfaces_project >0  ~ 'Faces'  ,
    
    numfaces_project==0~'View'),
    
    faces_project=as.factor(faces_project))%>%
  mutate(tag_names_length=lengths(strsplit(tag_names, "[|]")),
         tag_names_length=(tag_names_length - min(tag_names_length)) / (max(tag_names_length) - min(tag_names_length)))%>%
  mutate(
    color_foreground=ifelse(is.na(color_foreground), "None", color_foreground),
    color_foreground=case_when(
      color_foreground%in%c("Blue", "Orange","Pink","Purple", "Red")~ 'Light',
      color_foreground%in%c("Yellow", "White", "Green", "Brown","Teal")~ 'Clean',
      TRUE ~ 'None'
    ),
    color_foreground=as.factor(color_foreground))%>%
  
  mutate(
    color_background=ifelse(is.na(color_background), "None", color_background),
    color_background=case_when(
      color_background%in%c("Teal", "Purple")~ 'Clean',
      color_background%in%c("Red")~ 'Too light',
      
      TRUE ~ 'None'
    ),
    color_background=as.factor(color_background)
  )%>%
  mutate(isbwImg1=as.factor(ifelse(is.na(isbwImg1), "FALSE", isbwImg1)))%>%
  mutate(color=case_when(
    color_background%in%c('Clean')& isbwImg1%in%c('FALSE')~ '1',
    color_background%in%c('Clean')& isbwImg1%in%c('TRUE')~'2',
    color_background%in%c('Too light')& isbwImg1%in%c('FALSE')~ '3',
    color_background%in%c('Too light')& isbwImg1%in%c('TRUE')~ '4',
    color_background%in%c('None')& isbwImg1%in%c('FALSE')~ '5',
    color_background%in%c('None')& isbwImg1%in%c('TRUE')~ '6',
    TRUE ~ 'None' ),
    color=as.factor(color))%>%
  
  group_by(creator_id, category_parent) %>%
  mutate(total = n()) %>%
  ungroup() %>%
  mutate(rank = ntile(total, 5),
         top_creator = as.factor(ifelse(rank == 5, "YES", "NO")))%>%
  
  mutate(male_or_female_project = case_when(
    male_project <female_project ~ 'F',
    male_project >female_project ~ 'M',
    TRUE ~ 'U'
  ),
  male_or_female_project = as.factor(male_or_female_project))%>%
  mutate(minage_creator = case_when(
    minage_creator <= 1 ~ 'Infant' ,
    minage_creator >1 & minage_creator <= 4 ~ 'Toddler'  ,
    minage_creator >4 & minage_creator <= 11 ~ 'Kids'   ,
    minage_creator >11 & minage_creator <= 18 ~ 'Adolscent'    ,
    minage_creator >18 & minage_creator <= 30 ~ 'Young_Adults'  ,
    minage_creator >30 & minage_creator <=45 ~ 'Mid_Adults',
    minage_creator >45 & minage_creator <=60 ~ 'Old_Adults',
    minage_creator > 60 ~ 'Senior_Citizens'),
    minage_creator=as.factor(minage_creator))%>%
  
  mutate(maxage_creator = case_when(
    maxage_creator <= 1 ~ 'Infant' ,
    maxage_creator >1 & maxage_creator <= 4 ~ 'Toddler'  ,
    maxage_creator >4 & maxage_creator <= 11 ~ 'Kids'   ,
    maxage_creator >11 & maxage_creator <= 18 ~ 'Adolscent'    ,
    maxage_creator >18 & maxage_creator <= 30 ~ 'Young_Adults'  ,
    maxage_creator >30 & maxage_creator <=45 ~ 'Mid_Adults',
    maxage_creator >45 & maxage_creator <=60 ~ 'Old_Adults',
    maxage_creator > 60 ~ 'Senior_Citizens'),
    maxage_creator=as.factor(maxage_creator)) %>%
  
  mutate(
    creator_age = case_when(
      minage_creator %in% c('Adolscent','Young_Adults') & maxage_creator %in% c('Adolscent','Young_Adults')~ 'More creative',
      TRUE~"Less creative"
    ),
    creator_age=as.factor(creator_age)
  )%>%
  mutate(male_or_female_creator = case_when(
    male_creator <female_creator ~ 'F',
    male_creator >female_creator ~ 'M',
    TRUE ~ 'U'
  ),
  male_or_female_creator = as.factor(male_or_female_creator))%>%
  mutate(avgsentencelength=(avgsentencelength - min(avgsentencelength)) / (max(avgsentencelength) - min(avgsentencelength)))%>%
  mutate(avgsyls=(avgsyls - min(avgsyls)) / (max(avgsyls) - min(avgsyls)))%>%
  
  mutate(contains_youtube=as.factor(contains_youtube)) %>%
  mutate(grade_level = case_when(
    grade_level <0 ~ 'Too basic',
    grade_level >=0 & grade_level <15 ~ 'Fair',
    grade_level >= 15 & grade_level <18 ~ 'Advanced',
    TRUE~"Skilled"),
    grade_level = as.factor(grade_level)) %>%
  
  mutate(profile_pict_type = case_when(
    isTextPic == 1 ~ 'Text',
    isLogoPic == 1 ~ 'Logo',
    isCalendarPic == 1 ~ 'Calendar',
    isDiagramPic == 1 ~ 'Diagram',
    isShapePic == 1 ~ 'Shape',
    TRUE ~  'None'
  ),
  profile_pict_type=case_when(
    profile_pict_type %in% c('Calendar', 'Diagram') ~ 'Great',
    TRUE ~  'Not good'
  ),
  profile_pict_type=as.factor(profile_pict_type)) %>%
  
  
  mutate(
    
    region=as.factor(region)
  )%>%
  
  
  left_join(test_pre, by = "id") %>%
  
  
  
  mutate(more_reward=(total_reward/goal),
         more_reward=(more_reward - min(more_reward)) / (max(more_reward) - min(more_reward)),
         total_reward=(total_reward - min(total_reward)) / (max(total_reward) - min(total_reward)))%>%
  mutate(goal_rank = ntile(goal, 5),
         top_goal = as.factor(ifelse(goal_rank == 5, "YES", "NO")))%>%
  mutate(goal=(goal - min(goal)) / (max(goal) - min(goal)) )%>%
  mutate(
    category_parent = as.factor(category_parent)
  ) %>%
  mutate(minage_project = case_when(
    minage_project <= 1 ~ 'Infant' ,
    minage_project >1 & minage_project <= 4 ~ 'Toddler'  ,
    minage_project >4 & minage_project <= 11 ~ 'Kids'   ,
    minage_project >11 & minage_project <= 18 ~ 'Adolscent'    ,
    minage_project >18 & minage_project <= 30 ~ 'Young_Adults'  ,
    minage_project >30 & minage_project <=45 ~ 'Mid_Adults',
    minage_project >45 & minage_project <=60 ~ 'Old_Adults',
    minage_project > 60 ~ 'Senior_Citizens'),
    minage_project=as.factor(minage_project))%>%
  
  mutate(maxage_project = case_when(
    maxage_project <= 1 ~ 'Infant' ,
    maxage_project >1 & maxage_project <= 4 ~ 'Toddler'  ,
    maxage_project >4 & maxage_project <= 11 ~ 'Kids'   ,
    maxage_project >11 & maxage_project <= 18 ~ 'Adolscent'    ,
    maxage_project >18 & maxage_project <= 30 ~ 'Young_Adults'  ,
    maxage_project >30 & maxage_project <=45 ~ 'Mid_Adults',
    maxage_project >45 & maxage_project <=60 ~ 'Old_Adults',
    maxage_project > 60 ~ 'Senior_Citizens'),
    maxage_project=as.factor(maxage_project)) %>%
  mutate(
    project_age = case_when(
      
      minage_project %in% c('Young_Adults', 'Mid_Adults', 'Old_Adults','Senior_Citizens') & maxage_project %in% c('Young_Adults', 'Mid_Adults', 'Old_Adults','Senior_Citizens')~ 'Adults',
      TRUE~"Other"
    ),
    project_age=as.factor(project_age)
  )%>%
  
  
  
  mutate(deadline =as.Date(deadline),
         launched_at= as.Date(launched_at), 
         created_at= as.Date(created_at),
         duration=as.numeric(difftime(deadline, launched_at, units = "days")),
         duration=(duration - min(duration)) / (max(duration) - min(duration)),
         prepare_time=as.numeric(difftime(launched_at, created_at, units = "days")),
         prepare_time=(prepare_time - min(prepare_time)) / (max(prepare_time) - min(prepare_time))
         
  )%>% mutate(
    afinn_pos_precentage=(afinn_pos/num_words),
    afinn_pos_precentage=ifelse(is.na(afinn_pos_precentage), 0, afinn_pos_precentage),
    afinn_neg_precentage=(afinn_neg/num_words),
    afinn_neg_precentage=ifelse(is.na(afinn_neg_precentage), 0, afinn_neg_precentage),
    attitude=ifelse(afinn_pos_precentage>=afinn_neg_precentage, "positive","negative"),
    attitude=as.factor(attitude)
  )%>%
  
  mutate(avg_wordlengths=(avg_wordlengths - min(avg_wordlengths)) / (max(avg_wordlengths) - min(avg_wordlengths)))%>%
  mutate(sentence_counter=(sentence_counter - min(sentence_counter)) / (max(sentence_counter) - min(sentence_counter)))%>%
  
  mutate(ADV=(ADV/num_words),
         ADV=ifelse(is.na(ADV), 0, ADV),
         NOUN=(NOUN/num_words),
         NOUN=ifelse(is.na(NOUN), 0, NOUN),
         ADP=(ADP/num_words),
         ADP=ifelse(is.na(ADP), 0, ADP),
         PRT=(PRT/num_words),
         PRT=ifelse(is.na(PRT), 0, PRT),
         DET=(DET/num_words),
         DET=ifelse(is.na(DET), 0, DET),
         PRON=(PRON/num_words),
         PRON=ifelse(is.na(PRON), 0, PRON),
         VERB=(VERB/num_words),
         VERB=ifelse(is.na(VERB), 0, VERB),
         NUM=(NUM/num_words),
         NUM=ifelse(is.na(NUM), 0, NUM),
         CONJ=(CONJ/num_words),
         CONJ=ifelse(is.na(CONJ), 0, CONJ),
         ADJ=(ADJ/num_words),
         ADJ=ifelse(is.na(ADJ), 0, ADJ),
  )%>%
  mutate(num_words=(num_words - min(num_words)) / (max(num_words) - min(num_words)))%>%
  separate(launched_at, into= c('launched_at_year', 'launched_at_month', 'launched_at_day'), sep='-')%>%
  group_by(launched_at_year)%>%
  mutate(launched_at_year_number=n())%>%
  ungroup()%>%
  mutate(launched_at_year_rank = ntile(launched_at_year_number, 5),
         top_launched_at_year = as.factor(ifelse(launched_at_year_rank == 5, "YES", "NO")))%>%
  group_by(launched_at_month)%>%
  mutate(launched_at_month_number=n())%>%
  ungroup()%>%
  mutate(launched_at_month_rank = ntile(launched_at_month_number, 5),
         top_launched_at_month = as.factor(ifelse(launched_at_month_rank == 5, "YES", "NO")))%>%
  separate(deadline, into= c('deadline_year', 'deadline_month', 'deadline_day'), sep='-')%>%
  group_by(deadline_year)%>%
  mutate(deadline_year_number=n())%>%
  ungroup()%>%
  mutate(deadline_year_rank = ntile(deadline_year_number, 5),
         top_deadline_year = as.factor(ifelse(deadline_year_rank == 5, "YES", "NO")))%>%
  group_by(launched_at_month)%>%
  mutate(deadline_month_number=n())%>%
  ungroup()%>%
  mutate(deadline_month_rank = ntile(deadline_month_number, 5),
         top_deadline_month = as.factor(ifelse(deadline_month_rank == 5, "YES", "NO")))%>%
  separate(created_at, into= c('created_at_year', 'created_at_month', 'created_at_day'), sep='-')%>%
  group_by(created_at_year)%>%
  mutate(created_at_year_number=n())%>%
  ungroup()%>%
  mutate(created_at_year_rank = ntile(created_at_year_number, 5),
         top_created_at_year = as.factor(ifelse(created_at_year_rank == 5, "YES", "NO")))%>%
  group_by(created_at_month)%>%
  mutate(created_at_month_number=n())%>%
  ungroup()%>%
  mutate(created_at_month_rank = ntile(created_at_month_number, 5),
         top_created_at_month = as.factor(ifelse(created_at_month_rank == 5, "YES", "NO")))%>%
  group_by(category_parent, category_name)%>%
  mutate(category_number=n())%>%
  ungroup()%>%
  mutate(category_rank = ntile(category_number, 5),
         top_category_rank = as.factor(ifelse(category_rank == 5, "YES", "NO")))%>%
  mutate(
    blurb_sentiment=get_sentiment(blurb, method="afinn"),
    blurb_sentiment=as.factor(ifelse(blurb_sentiment<=-6, "negative" ,ifelse(blurb_sentiment<=4, "neutral", "positive"))))%>%
  mutate(
    name_sentiment=get_sentiment(name, method="afinn"),
    name_sentiment=as.factor(ifelse(name_sentiment<=-1, "negative" ,ifelse(name_sentiment<=1, "neutral", "positive"))))%>%
  mutate(
    captions_sentiment=get_sentiment(captions, method="afinn"),
    captions_sentiment=as.factor(ifelse(captions_sentiment<=-2, "negative" , "positive")))



prep_fun = tolower
cleaning_tokenizer <- function(v) {
  v %>%
    removeNumbers %>% 
    removePunctuation %>% 
    removeWords(stopwords(kind="en")) %>% 
    stemDocument %>%
    word_tokenizer 
}
tok_fun = cleaning_tokenizer


it_train = itoken(test_1$blurb, 
                  preprocessor = prep_fun, 
                  tokenizer = tok_fun, 
                  ids = test_1$id, 
                  progressbar = FALSE)


vocab = create_vocabulary(it_train)
vocab_small = prune_vocabulary(vocab, vocab_term_max = 15240)
vectorizer = vocab_vectorizer(vocab_small)
dtm_train = create_dtm(it_train, vectorizer)
dtm_train_bin <- dtm_train>0+0 

dtm_train_small <- dtm_train_bin[,1:10]

dense = as.matrix(dtm_train_small)+0

test_1 <- cbind(test_1, dense)



test_1<-test_1%>%
  mutate(topwrods=case_when(
    help == 1~ 'YES',
    new == 1~ 'YES'  ,
    make == 1~ 'YES'  ,
    will == 1~ 'YES'  ,
    creat == 1~ 'YES'  ,
    need == 1~ 'YES'  ,
    game == 1~ 'YES'  ,
    want == 1~ 'YES'  ,
    world == 1~ 'YES'  ,
    can == 1~ 'YES'  ,
    TRUE~"NO"
  ),
  topwrods=as.factor(topwrods))







test_data<-test_1%>%
  select( goal,duration,sentence_counter,prepare_time,total_reward, max_reward,more_reward,min_reward,
         ADV,ADP,PRT,DET,PRON,VERB,NUM,CONJ,ADJ,avgsyls,avgsentencelength,afinn_pos_precentage,afinn_neg_precentage,
         male_or_female_creator, male_or_female_project , color_foreground,project_age,category_parent, profile_pict_type
         , region,grade_level,color,contains_youtube,creator_age,faces_project,top_creator,tag_names_length,
         faces_creator, captions_length,blurb_length,launched_at_year_number, top_launched_at_year,top_launched_at_month,
         top_deadline_year, top_deadline_month,top_created_at_year,top_created_at_month,top_category_rank,top_goal,
         blurb_sentiment,topwrods, name_sentiment, captions_sentiment, top_goal,top_reward)





classifications_success <- predict(rf.mod_data, newdata=test_data)
summary(classifications_success)

write.table(classifications_success, "success_group7.csv", row.names = FALSE)

