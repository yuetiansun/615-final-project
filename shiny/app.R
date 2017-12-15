library(shiny)
library(twitteR)
library(devtools)
library(reshape)
library(plyr)
library(ggplot2)
library(splitstackshape)
library(stringr)
library(maps)
library(ggmap)

#read the dataset for map
coordinates_tidy <-read.csv('tweets.map_tax.csv')

# Define UI logic required 
ui <- shinyUI(fluidPage(
   
   # Application title
   titlePanel("615 Final Project: Analysis of Twitter with \"Tax\""),
   h2('Introduction'),
   p('As we all know that President Trump has published a policy about reformation of tax these days. This
     policy has significant effect on thousands of people’s life for example graduate student may have much heavier
     burdern about their tuition fee. This project aims to extract useful information when people twittering about
     tax finding out what people atitudes are and what they are most concerned about. This project is devided
     into several parts. Firstly, to have first impression of data, we plot the density graph and points on map
     about which area’s people are more likely to twitter about tax. Then we conduct Emoji Analysis on these
     twitters trying to find people’s opinion by analyzing emojis. Next we take a further step in the text part of
     twitters. We conduct Topic Modeling Analysis, Sentiment Analysis and make a perdict of American people
     thought when talking about tax. As a result of these, we can make an assumption of people satisfaction this
     policy.'),
   h2('1 Exploratory Analysis of Data'),
   p('We randomly select 47474 of twitters with \"tax" from 11/24/2018 to 12/04/2018. Now let us take a first look
     of the data. We would like to figure out the general situation about twittering with \"tax\" across the US.
     Thus, we make a point plot and density plot on map to have a first impression. You can choose how many density levels you want to plot in the density plot'),
   # Sidebar with a slider input for number of bins 
   plotOutput("map1"),
   sidebarLayout(
     sidebarPanel(sliderInput("level",
                               "Density Levels:",
                               min = 2,  max = 10,  value = 3)),
     mainPanel(plotOutput("map2"))  ),
   p('As we can see the plot above, the point plot and the density show that people in the east coast and north
     middle part of America have more twitters about \"tax\", which means in some sense they are more concerned
     about tax issue. We can guess that may be the east coast has lots of schools and has large population. The
     reformation of tax will have significant effect on this area. And also people live in the north middle parts of
     America don’t have very high salary on average and the reduction of individual income tax can help them
     have better life. '),
   h2('2 Emoji Analysis'),
   p('Now we want to take a deep step on people attitude when twittering \"tax\". Let us take a looking of what
    emojis people would like to twitter about \"tax\". First of all, we need to clean the dataset and extract the
     emojis (emojis code) from the tweets we selected. Then by pairing the emojis code with emojis dictionary
     and emojis picture, we can get the original emojis. Then we calculate the frequency of these emojis and figue
     out which one people are more likely to use when twittering about \"tax\". You can choose the number of emojis in frequency table and bar plot'),
   sidebarLayout(
     sidebarPanel(sliderInput("num_emoji1",
                  "Rank of Emojis:",
                  min = 1,  max = 30, value = 10)),
     mainPanel(tableOutput("emoji")  )),
   sidebarLayout(
     sidebarPanel(sliderInput("num_emoji2",
                  "Rank of Emojis:",
                  min = 1,  max = 30, value = 10)),
     mainPanel(plotOutput("emoji1")  )),
   p('As we can see from the plot and the table above, the most frequently uesd emoji when talking about tax is
     money bag. This make sense because tax directly related to money and this should be the thing people most
     concerned about. The third emoji is chart with downwards trend. We can assume that this is a negative
     one and people using this to express their life going down or it may be a positive one used to express the
     tax reduction. Also the fourth to sixth shows people concern about hospital fee, tuition fee and many other
     fee related to tax policy. And the face with tears of joy can some how show the people atitude about tax
     reduction.'),
   p('The next step after visualizing the top emojis in the overall dataset is to compare emoji frequency between
two different subsets of the data. To find out more information about it, we can give several pairs of important
     words people may twitter with \"tax\" and comparing which emoji they would like to use. There are multiple 
     pairs of words you can choose for this table and graph.'),
   sidebarLayout(
     sidebarPanel(selectInput("pairword1", "Keywords",
                              choices = c("trump", "bill", "money", "vote", "reform", "cut", "vote", "gop", "senate", "republican","scam")),
                  selectInput("pairword2", "Keywords",
                               choices = c("cut", "bill", "money", "vote", "reform", "trump", "vote", "gop", "senate", "republican","scam"))),
     mainPanel(tableOutput("emoji2")  )),
     sidebarLayout(
       sidebarPanel(selectInput("pairword3", "Keywords",
                                choices = c("trump", "bill", "money", "vote", "reform", "cut", "vote", "gop", "senate", "republican","scam")),
                    selectInput("pairword4", "Keywords",
                                 choices = c("cut", "bill", "money", "vote", "reform", "trump", "vote", "gop", "senate", "republican","scam"))),
                    mainPanel(plotOutput("emoji3")  )), 
   p('Here let us interpret keywords \"trump\" and \"cut\" as an example. As we can see above, when
people mention \"trump\" in twitter with \"tax\", they tends to use face with tears of joy while people mention
     \"cut\" are more likely to use money bag. This result makes sense because cutting tax is directly related to
     money. Also people who mention \"cut\" have higher probability of using police cars revolving light emoji.
     This may use to call people’s attention about the reformation of tax. In additon, people who mention \"cut\"
     or \"trump\" are equal likely to use person with folded hands. May be they use it to hope for a good life. '),
   p('After exploring the emojis, we now focus on the text part of twitters.'),
   h2('3 Text Analysis'),
   p('Now let us take a look of what words people would likely to use when twittering with \"tax\". To make the
result intuitively, we make a table and a frequency plot of words showing up more than 1500 times. You can change the frequency by varying the slider'),
   sidebarLayout(
     sidebarPanel(sliderInput("word_freq1",
                              "Minimum of Word Frequency:",
                              min = 500,  max = 3000, value = 1400)),
     mainPanel(tableOutput("text") )),
   sidebarLayout(
     sidebarPanel(sliderInput("word_freq2",
                              "Minimum of Word Frequency:",
                              min = 500,  max = 3000, value = 1400)),
     mainPanel(plotOutput("text1") )),
   p('From the bar plot and the table above, We can find that people are concerned about cuting their bill,
republican and voting things about tax. These make sense because the reducation tax policy is passed in
     senate and is comed up by Trump, Repulican Party. And the aim of these policy is to cut the tax and
     promote American’s life.'),
   p('Now let us continue finding out what topics people would likely to talk about tax.'),
   h3('3.1 Topic Modeling'),
   p('To take a further step, we conduct Topic Modeling and analyze what topic people will talk when twitter
about tax.'),
   p('Firstly, we would like to introduce what is Topic Modeling and how it works on twitter. In machine learning
and natural language processing, a topic model is a type of statistical model for discovering the abstract
     \"topics\" that occur in a collection of documents. Topic modeling is a frequently used text-mining tool for
     discovery of hidden semantic structures in a text body. Intuitively, given that a document is about a particular
     topic, one would expect particular words to appear in the document more or less frequently: \"dog\" and
     \"bone\" will appear more often in documents about dogs, \"cat\" and \"meow\" will appear in documents about
     cats, and \"the\" and \"is\" will appear equally in both.'),
   p('Latent Dirichlet allocation (LDA) is a particularly popular method for fitting a topic model. It treats each
document as a mixture of topics, and each topic as a mixture of words. This allows documents to “overlap”
     each other in terms of content, rather than being separated into discrete groups, in a way that mirrors typical
     use of natural language.'),
   p('There are four methods you can use for Topic Modeling. You can choose to see the result by hitting the botton below. In this report, we take LDA method as an example. And we would like to choose Gibbs Sampling. We do not show how Topic Modeling works mathematically in here. If you are interested in it, please read the PDF report in 615 final project file.' ),
   sidebarLayout(
     sidebarPanel(radioButtons("method", "Topic Modeling Method",
                  choices = c("CTM", "VEM", "VEM_Fixed", "Gibbs"),
                  selected = "Gibbs")),
     mainPanel(tableOutput("text2")  )),
   p('Here is the example analysis result. As we can see from the above table, this is the 5 topics people would likely to talk about when twittering
\"tax\". Now let’s indicate the first three topics. we can find that topic 1 contain words like \"rich\", \"poor\",
     \"middle\" and \"class\". Thus we can make an assumption that people care about how tax affects on different
     classes. The second topic has \"trump\", \"reform\", \"thank\" and \"lie\" in it. Thus we can indicate that this
     may relate to topic about President Trump and reformation of tax. In the third part, we have words \"vote\",
     \"people\", \"support\" and \"follow\". Therefore, we can view the third topic as people supporting degree on tax
     reduction.'),
   h3('3.2 Sentiment Analysis'),
   p('Now let us take a look on what people attitude on tax by conducting sentiment analysis. To start with, let
us see what words people use to express their emotion. We can choose emotion like joy, angry, disgust.'),
   sidebarLayout(
     sidebarPanel(selectInput("emotion", "Emotional Words",
                              choices = c("joy", "disgust", "anger"))),
     mainPanel(tableOutput("text3")  )),
   p('We here use joyful word as an example to analysis. As we can see from the above table, the most frequently used joyful words are \"vote\", \"pay\", \"money\" and
\"good\". Thus, we can guess that people with positve opinion about \"tax\" are likely to talk about vote for the
     tax policy and the money things.'),
   p('Now let us analyze people atitude by comparing the positive and negative words. You can choose the number of twitter by verying the slider.'),
   sidebarLayout(
     sidebarPanel(sliderInput("everyword",
                              "Number of Positive- Negavtive Word in Every... Twitters:",
                              min = 1,  max = 300, value = 80)),
     mainPanel(plotOutput("text4")  )),
   p('As we can see from the positive - negative plot, the lines above horizontal line means the positve words are
more than negative words in every eighty twitter. This graph show that the case positive words are more than
     negative happens more than the other case. That is to say, the number of positive words is larger than the
     number of negative words. Thus we can indicate that the on the average people show positive atitude when
     twittering about tax. We are now interested in what positive and negative words people most frequently used. Again, you can change the number of words by varying the slider.'),
   sidebarLayout(
     sidebarPanel(sliderInput("pnword_num1",
                              "Rank of Positive and Negative Words:",
                              min = 1,  max = 20, value = 5)),
     mainPanel(tableOutput("text5")  )),
   sidebarLayout(
     sidebarPanel(sliderInput("pnword_num2",
                              "Rank of Positive and Negative Words:",
                              min = 1,  max = 20, value = 5)),
     mainPanel(plotOutput("text6")  )),
   p('As we can see from the plot and table above, the three most frequently used words are all positive. People
would likely to use \"reform\", \"trump\" and \"rich\". We can indicate that people tend to believe the reformation
    of tax published by Trump can help people become rich and wealthy. But as we can also find that some
     people use negative words like \"scam\", \"poor\" and \"break\". Thus, although lots of people support the policy,
     there are some people think it is a scam and will make people poor.'),
   p('To have a more intuitive way of the words and its frequency, we make word clouds of top 300 frequently used
words and top 200 positive and negative words. You can change the minimum frequency and the maximum number of words here.'),
   sidebarLayout(
     sidebarPanel(sliderInput("w1_freq",
                              "Minimum Frequency:",
                              min = 10,  max = 800, value = 10),
                  sliderInput("w1_max",
                              "Maximum Number of Words:",
                              min = 50,  max = 800,  value = 300)),
     mainPanel(plotOutput("wordcloud1"))),
   sidebarLayout(
     sidebarPanel(sliderInput("w2_max",
                              "Maximum Number of Words:",
                              min = 50,  max = 800,  value = 200)),
     mainPanel(plotOutput("wordcloud2"))),
   p('As we see from the two word clouds above, it is very clear tha people concern about the about bill and money staff most and then they also care about the policy itself like the voting things, senate, repiblican and Trump and so on. Besides, people with positive attitude would likely to think about the reform of tax will make them rich which people with negative attitude think the reduction of tax is a scam.'),
   h2('Conclusion'),
   p('In this project, we care about how people think about the tax through twitter. We find that people living
in east coast and north middle part show more concern about tax. Besides, by analyzing the emojis, we
     figue out that people are most concerned about money stuff. They care about hospital fee, tuition fee and
     many other fee related to tax policy. To narrow down, we use \"trump\" and \"cut\" as our second keywords.
     When people mention \"trump\" in twitter with \"tax\", they tends to use face with tears of joy while people
     mention \"cut\" are more likely to use money bag. In additon, people who mention \"cut\" or \"trump\" are equal
     likely to use person with folded hands. For the test part, from the bar plots, word clouds and tables, we
     can find that people are concerned about cuting their bill, republican and voting things about tax. And
     by conducting Topic Modeling, we find that people would likely to talk about how tax affects on different
     classes, President Trump and reformation of tax, and supporting degree on tax reduction. Additionaly, by
     conducting the Sentiment Analysis, we find that people show more postive attitude when twittering about
     tax and they tend to believe the reformation of tax can bring people good life.'),
    p('To sum up, when twittering about tax, people are most concerned about the reduction policy and how it
     affect their money and their life. In general, although some people think this policy is a scam, lots of people
     do believe this reformation will make them wealthy and they support Trump policy of cuting tax.'),
   h2('Reference'),
   p('[1] URL: https://deanattali.com/blog/building-shiny-apps-tutorial/#before-we-begin'),
   p('[2] URL: http://ethen8181.github.io/machine-learning/clustering_old/topic_model/LDA.html'),
   p('[3] URL: https://en.wikipedia.org/wiki/Latent_Dirichlet_allocation'))
   )

# Define server logic required 
server <- shinyServer(function(input, output, session) {
  terms<-reactive({
    #change when the "update" buttion is pressed
    input$update
    #but not for anything else
    isolate({
      withProgress({
        setProgress(message = "Processing corpus...")
        getTermMatrix(input$selection)
      })
    })
  })
  
  
# make the point plot on the map 
  output$map1 <- renderPlot({usmap1 = readRDS("usmap1.rds")
    usmap1 })
# make the density plot on the map  
  output$map2 <- renderPlot({
    coordinates_tidy <-read.csv('tweets.map_tax.csv')
    map=readRDS("usmap2.rds")
    ggmap(map)+ggtitle("Geolocaion density plot for keyword 'tax' from domestic twitter users")+stat_density2d(
      aes(x = long, y = lat, alpha = ..level..),
      size = 0.5, bins = input$level, data = coordinates_tidy,
      geom = "polygon",colour = "black")})
#make the emoji frequency table  
  output$emoji <- renderTable({
    emojis.count.p=readRDS("emojis_count_p.rds")
    ##### MAKE BAR CHART OF TOP EMOJIS IN NEW DATASET
    df.plot <- subset(emojis.count.p, rank <= input$num_emoji1)
    df.plot}, caption='Most frequently used emojis')
 #make the emoji frequency plot   
  output$emoji1 <- renderPlot({
    emojis.count.p=readRDS("emojis_count_p.rds")
    df.plot <- subset(emojis.count.p, rank <= input$num_emoji2)
    xlab <- 'Rank'
    ylab <- 'Overall Frequency (per 1,000 Tweets)'
    df.plot <- arrange(df.plot, name)
    imgs <- lapply( paste0('ios_9_3_emoji_files/', df.plot$name, '.png'), png::readPNG)
    g <- lapply(imgs, grid::rasterGrob)
    k <- 0.20 * (10/nrow(df.plot)) * max(df.plot$dens)
    df.plot$xsize <- k
    df.plot$ysize <- k
    df.plot <- arrange(df.plot, name);
    ggplot(data = df.plot, aes(x = rank, y = dens)) +
      geom_bar(stat = 'identity', fill = 'dodgerblue4') +
      xlab(xlab) + ylab(ylab) +
      mapply(function(x, y, i) {
        annotation_custom(g[[i]], xmin = x-0.5*df.plot$xsize[i], xmax = x+0.5*df.plot$xsize[i], ymin = y-0.5*df.plot$ysize[i], ymax = y+0.5*df.plot$ysize[i])},
        df.plot$rank, df.plot$dens, seq_len(nrow(df.plot))) +
      scale_x_continuous(expand = c(0, 0), breaks = seq(1, nrow(df.plot), 1), labels = seq(1, nrow(df.plot), 1)) +
      scale_y_continuous(expand = c(0, 0), limits = c(0, 1.10 * max(df.plot$dens))) +
      theme(panel.grid.minor.y = element_blank(),
            axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 14), 
            axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'))+ggtitle("Plot of emoji Frequency")
  })
  
#make the emoji comparation table
  output$emoji2 <- renderTable({
    ##### CREATE MASTER DATASET OF ORIGINAL TWEETS appended with array of emojis
    ## EMOJIS: create reduced tweets+emojis matrix
    tweets.emojis=readRDS("tweets_emojis.rds")
    df =readRDS("df.rds")
    #### MAKE TWO WAY PLOT FOR A SET OF MUTUALLY EXCLUSIVE SUBSETS OF THE DATA
    df.1 <- subset(tweets.emojis, grepl(paste(c(input$pairword1), collapse = '|'), tolower(tweets.emojis$text)));
    df.2 <- subset(tweets.emojis, grepl(paste(c(input$pairword2), collapse = '|'), tolower(tweets.emojis$text)));
    # dataset 1
    df.a <- subset(subset(df.1, emoji.names != ''), select = c(tweetid, emoji.names))
    df.a$emoji.names <- as.character(df.a$emoji.names)
    df.b <- data.frame(table(unlist(strsplit(df.a$emoji.names, ','))))
    names(df.b) <- c('var', 'freq')
    df.b$var <- trimws(df.b$var, 'both')
    df.b <- subset(df.b, var != '')
    df.c <- aggregate(freq ~ var, data = df.b, function(x) sum(x))
    df.c <- df.c[with(df.c, order(-freq)), ]
    row.names(df.c) <- NULL
    df.d <- subset(df.c, freq > 1)
    df.d$dens <- round(1000 * (df.d$freq / nrow(df)), 1)
    df.d$dens.sm <- (df.d$freq + 1) / (nrow(df) + 1)
    df.d$rank <- as.numeric(row.names(df.d))
    colnames(df.d)[1] <- "name"
    df.e <- subset(df.d, select = c(name, dens, dens.sm, freq, rank))
    df.e$ht <- as.character(arrange(data.frame(table(tolower(unlist(str_extract_all(df.1$text, '#\\w+'))))), -Freq)$Var1[1]);
    emojis.count.1 <- df.e
    # dataset 2
    df.a <- subset(subset(df.2, emoji.names != ''), select = c(tweetid, emoji.names)); df.a$emoji.names <- as.character(df.a$emoji.names);
    df.b <- data.frame(table(unlist(strsplit(df.a$emoji.names, ',')))); names(df.b) <- c('var', 'freq'); df.b$var <- trimws(df.b$var, 'both'); df.b <- subset(df.b, var != '');
    df.c <- aggregate(freq ~ var, data = df.b, function(x) sum(x)); df.c <- df.c[with(df.c, order(-freq)), ]; row.names(df.c) <- NULL;
    df.d <- subset(df.c, freq > 1)
    df.d$dens <- round(1000 * (df.d$freq / nrow(df)), 1)
    df.d$dens.sm <- (df.d$freq + 1) / (nrow(df) + 1)
    df.d$rank <- as.numeric(row.names(df.d))
    colnames(df.d)[1] <- "name"
    df.e <- subset(df.d, select = c(name, dens, dens.sm, freq, rank));
    df.e$ht <- as.character(arrange(data.frame(table(tolower(unlist(str_extract_all(df.2$text, '#\\w+'))))), -Freq)$Var1[1]);
    df.e[1:10, ]; emojis.count.2 <- df.e;
    # combine datasets and create final dataset
    names(emojis.count.1)[-1] <- paste0(names(emojis.count.1)[-1], '.1'); names(emojis.count.2)[-1] <- paste0(names(emojis.count.2)[-1], '.2'); 
    df.a <- merge(emojis.count.1, emojis.count.2, by = 'name', all.x = TRUE, all.y = TRUE);
    df.a[, c(2:4, 6:8)][is.na(df.a[, c(2:4, 6:8)])] <- 0; df.a <- df.a[with (df.a, order(-dens.1)), ];
    df.a$index <- ifelse(df.a$dens.1 > 0 & df.a$dens.2 > 0 & (df.a$dens.1 > df.a$dens.2), round(100 * ((df.a$dens.1 / df.a$dens.2) - 1), 0),
                         ifelse(df.a$dens.1 > 0 & df.a$dens.2 > 0 & (df.a$dens.2 > df.a$dens.1), -1 * round(100 * ((df.a$dens.2 / df.a$dens.1) - 1), 0), NA));
    df.a$logor <- log(df.a$dens.sm.1 / df.a$dens.sm.2);
    df.a$dens.mean <- 0.5 * (df.a$dens.1 + df.a$dens.2);
    k <- 50; df.b <- subset(df.a, (rank.1 <= k | rank.2 <= k) & 
                              (freq.1 >= 5 | freq.2 >= 5) & 
                              (freq.1 > 0 & freq.2 > 0) & dens.mean > 0);
    df.c <- subset(df.b, select = c(name, dens.1, dens.2, freq.1, freq.2, dens.mean, round(logor, 2)));
    df.c <- df.c[with(df.c, order(-logor)), ]; row.names(df.c) <- NULL
    emojis.comp.p <- df.c;
    df.c}, caption='Table of emoji comparison')
# make the emojis comparation plot 
  output$emoji3 <- renderPlot({
    ##### PLOT TOP EMOJIS SCATTERPLOT: FREQ VS VALENCE  
    ## read in custom emojis
    tweets.emojis=readRDS("tweets_emojis.rds")
    df =readRDS("df.rds")
    #### MAKE TWO WAY PLOT FOR A SET OF MUTUALLY EXCLUSIVE SUBSETS OF THE DATA
    df.1 <- subset(tweets.emojis, grepl(paste(c(input$pairword3), collapse = '|'), tolower(tweets.emojis$text)));
    df.2 <- subset(tweets.emojis, grepl(paste(c(input$pairword4), collapse = '|'), tolower(tweets.emojis$text)));
    # dataset 1
    df.a <- subset(subset(df.1, emoji.names != ''), select = c(tweetid, emoji.names)); df.a$emoji.names <- as.character(df.a$emoji.names);
    df.b <- data.frame(table(unlist(strsplit(df.a$emoji.names, ',')))); names(df.b) <- c('var', 'freq'); df.b$var <- trimws(df.b$var, 'both'); df.b <- subset(df.b, var != '');
    df.c <- aggregate(freq ~ var, data = df.b, function(x) sum(x)); df.c <- df.c[with(df.c, order(-freq)), ]; row.names(df.c) <- NULL;
    df.d <- subset(df.c, freq > 1)
    df.d$dens <- round(1000 * (df.d$freq / nrow(df)), 1)
    df.d$dens.sm <- (df.d$freq + 1) / (nrow(df) + 1)
    df.d$rank <- as.numeric(row.names(df.d))
    colnames(df.d)[1] <- "name"
    df.e <- subset(df.d, select = c(name, dens, dens.sm, freq, rank)); 
    df.e$ht <- as.character(arrange(data.frame(table(tolower(unlist(str_extract_all(df.1$text, '#\\w+'))))), -Freq)$Var1[1]);
    df.e[1:10, ]; emojis.count.1 <- df.e;
    # dataset 2
    df.a <- subset(subset(df.2, emoji.names != ''), select = c(tweetid, emoji.names)); df.a$emoji.names <- as.character(df.a$emoji.names);
    df.b <- data.frame(table(unlist(strsplit(df.a$emoji.names, ',')))); names(df.b) <- c('var', 'freq'); df.b$var <- trimws(df.b$var, 'both'); df.b <- subset(df.b, var != '');
    df.c <- aggregate(freq ~ var, data = df.b, function(x) sum(x)); df.c <- df.c[with(df.c, order(-freq)), ]; row.names(df.c) <- NULL;
    df.d <- subset(df.c, freq > 1)
    df.d$dens <- round(1000 * (df.d$freq / nrow(df)), 1)
    df.d$dens.sm <- (df.d$freq + 1) / (nrow(df) + 1)
    df.d$rank <- as.numeric(row.names(df.d))
    colnames(df.d)[1] <- "name"
    df.e <- subset(df.d, select = c(name, dens, dens.sm, freq, rank));
    df.e$ht <- as.character(arrange(data.frame(table(tolower(unlist(str_extract_all(df.2$text, '#\\w+'))))), -Freq)$Var1[1]);
    df.e[1:10, ]; emojis.count.2 <- df.e;
    # combine datasets and create final dataset
    names(emojis.count.1)[-1] <- paste0(names(emojis.count.1)[-1], '.1'); names(emojis.count.2)[-1] <- paste0(names(emojis.count.2)[-1], '.2'); 
    df.a <- merge(emojis.count.1, emojis.count.2, by = 'name', all.x = TRUE, all.y = TRUE);
    df.a[, c(2:4, 6:8)][is.na(df.a[, c(2:4, 6:8)])] <- 0; df.a <- df.a[with (df.a, order(-dens.1)), ];
    df.a$index <- ifelse(df.a$dens.1 > 0 & df.a$dens.2 > 0 & (df.a$dens.1 > df.a$dens.2), round(100 * ((df.a$dens.1 / df.a$dens.2) - 1), 0),
                         ifelse(df.a$dens.1 > 0 & df.a$dens.2 > 0 & (df.a$dens.2 > df.a$dens.1), -1 * round(100 * ((df.a$dens.2 / df.a$dens.1) - 1), 0), NA));
    df.a$logor <- log(df.a$dens.sm.1 / df.a$dens.sm.2);
    df.a$dens.mean <- 0.5 * (df.a$dens.1 + df.a$dens.2);
    k <- 50; df.b <- subset(df.a, (rank.1 <= k | rank.2 <= k) & 
                              (freq.1 >= 5 | freq.2 >= 5) & 
                              (freq.1 > 0 & freq.2 > 0) & dens.mean > 0);
    df.c <- subset(df.b, select = c(name, dens.1, dens.2, freq.1, freq.2, dens.mean, round(logor, 2)));
    df.c <- df.c[with(df.c, order(-logor)), ]; row.names(df.c) <- NULL
    emojis.comp.p <- df.c
    df.t <- arrange(emojis.comp.p, name);
    imgs <- lapply(paste0('ios_9_3_emoji_files/', df.t$name, '.png'), png::readPNG)
    g <- lapply(imgs, grid::rasterGrob);
    ## make plot  
    df.t <- arrange(emojis.comp.p, logor)
    xlab <- paste0('Emoji Valence: Log Odds Ratio (', paste0(unique(emojis.count.2$ht), ' <--> ', unique(emojis.count.1$ht), ')'));
    ylab <- 'Overall Frequency (Per 1,000 Tweets)'
    k <- 8 # size parameter for median element
    xsize <- (k/100) * (max(df.t$logor) - min(df.t$logor)); ysize <- (k/100) * (max(df.t$dens.mean) - min(df.t$dens.mean));
    df.t$xsize <- xsize; df.t$ysize <- ysize;
    df.t$dens.m <- ifelse(df.t$dens.mean > median(df.t$dens.mean), round(sqrt((df.t$dens.mean / min(df.t$dens.mean))), 2), 1);
    df.t$xsize <- df.t$dens.m * df.t$xsize; df.t$ysize <- df.t$dens.m * df.t$ysize;
    df.t <- arrange(df.t, name);
    ggplot(df.t, aes(jitter(logor), dens.mean)) +
      xlab(xlab) + ylab(ylab) +
      mapply(function(x, y, i) {
        annotation_custom(g[[i]], xmin = x-0.5*df.t$xsize[i], xmax = x+0.5*df.t$xsize[i], 
                          ymin = y-0.5*df.t$ysize[i], ymax = y+0.5*df.t$ysize[i])},
        jitter(df.t$logor), df.t$dens.mean, seq_len(nrow(df.t))) +
      scale_x_continuous(limits = c(1.15 * min(df.t$logor), 1.15 * max(df.t$logor))) +
      scale_y_continuous(limits = c(0, 1.20 * max(df.t$dens.mean))) +
      geom_vline(xintercept = 0) + geom_hline(yintercept = 0) +
      theme_bw() +
      theme(axis.title.x = element_text(size = 10), axis.title.y = element_text(size = 10), 
            axis.text.x  = element_text(size = 8, colour = 'black'), axis.text.y  = element_text(size = 8, colour = 'black'));
  })
  
  
  
  output$text <- renderTable({
  #text analysis
  library(tidytext)
  library(tidyverse)
  library(wordcloud)
  library(RColorBrewer)
  library(tokenizers)
  library(tm)
  library(NLP)
  library(topicmodels)
  library(SnowballC)
  library(reshape2)

  tidy_tw <- read.csv("tweets.texttidy_tax.csv")
  #get the table of word freq
  texttable1 <- tidy_tw %>%
          count(word, sort = TRUE) %>%
          filter(n > input$word_freq1)
  texttable1 }, width="500", caption='Table of words frequency')
  
  
  output$text1 <- renderPlot({
  #plot the bar plot of the freq of words
    tidy_tw <- read.csv("tweets.texttidy_tax.csv")
    tidy_tw %>%
    count(word, sort = TRUE) %>%
    filter(n > input$word_freq2) %>%
    mutate(word = reorder(word, n)) %>%
    ggplot(aes(word, n, fill=word)) +
    geom_col(alpha=0.8) +
    xlab(NULL) +
    coord_flip() +ggtitle("Bar plot of the top 10 words frequency")
  })
  
#output the topic modeling result 
  output$text2 <- renderTable({
    m = readRDS("topic.rds")
    d <- as.data.frame(m[input$method])
    d <- d[,1:5]
  }, caption="Topics for twitter tax")
  
#output the emotional words
  output$text3 <- renderTable({
    #sentiment analysis
    #take a look of the joy words in twitter with 'tax'
    tidy_tw <- read.csv("tweets.texttidy_tax.csv")
    nrcjoy <- get_sentiments("nrc") %>% 
      filter(sentiment == input$emotion)
    test_table2 <- tidy_tw %>%
            inner_join(nrcjoy) %>%
            count(word, sort = TRUE) %>%
            top_n(8)
    test_table2 <- t(test_table2)
    colnames(test_table2) <- c("Word1", "Word2", "Word3", "Word4", "Word5","Word6", "Word7", "Word8")
    test_table2
  }, caption='Table of top 8 emotional words')

 
  output$text4 <- renderPlot({
    #plot the num of positive -negative words
    tidy_tw <- read.csv("tweets.texttidy_tax.csv")
    twsentiment <- tidy_tw %>%
      inner_join(get_sentiments("bing")) %>%
      count(index = line %/% input$everyword, sentiment) %>%
      spread(sentiment, n, fill = 0) %>%
      mutate(sentiment = positive - negative)
    
    ggplot(twsentiment, aes(index, sentiment, fill=index)) +
      geom_col(show.legend = FALSE)+ggtitle("Positive - negative words in every...twitter")+ labs(y = "number of positve words minus negative", x = 'every ... twitter')
  })
  
  
  output$text5 <- renderTable({
    #compare positive-negative
    tidy_tw <- read.csv("tweets.texttidy_tax.csv")
    bing_word_counts <- tidy_tw %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    #get the table of positive and negative words
    bing_word_counts %>%
            group_by(sentiment) %>%
            top_n(input$pnword_num1)}, width="500", caption='Table of positive and negative words')
  
  #plot the freq of top 10 positive and negative words  
  output$text6 <- renderPlot({
    tidy_tw <- read.csv("tweets.texttidy_tax.csv")
    bing_word_counts <- tidy_tw %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      ungroup()
    bing_word_counts %>%
      group_by(sentiment) %>%
      top_n(input$pnword_num2) %>%
      ungroup() %>%
      mutate(word = reorder(word, n)) %>%
      ggplot(aes(word, n, fill = sentiment)) +
      geom_col(show.legend = FALSE) +
      facet_wrap(~sentiment, scales = "free_y") +
      labs(y = "Contribution to sentiment",
           x = NULL) +
      coord_flip()+ggtitle("Frequency of top 10 positive and negative words")
  })

  #Word Clouds
  output$wordcloud1 <- renderPlot({
    pal2 <- brewer.pal(8,"Dark2")
    tidy_tw <- read.csv("tweets.texttidy_tax.csv")
    tidy_tw %>%
      count(word) %>%
      with(wordcloud(word, n, scale=c(8,.2),min.freq=input$w1_freq,
                     max.words=input$w1_max, random.order=FALSE,rot.per=.3, colors = pal2))})
  
  
  output$wordcloud2 <- renderPlot({
    #make the world clouds of positive vs. negative words 
    tidy_tw <- read.csv("tweets.texttidy_tax.csv")
    tidy_tw %>%
      inner_join(get_sentiments("bing")) %>%
      count(word, sentiment, sort = TRUE) %>%
      acast(word ~ sentiment, value.var = "n", fill = 0) %>%
      comparison.cloud(colors = c("#F8766D", "#00BFC4"), random.order=FALSE,rot.per=.3, max.words = input$w2_max)})
})
# Run the application 
shinyApp(ui = ui, server = server)

