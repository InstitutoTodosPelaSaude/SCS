library(tidytext)
library(tidyr)
library(dplyr)
library(tidytable)
library(lubridate)
library(stringr)
library(wordcloud2)
library(stringi)
library(ggplot2)

#Lista de stop words em PT-Br
stopwords <- stopwords <- read.delim(
  file = "http://www.labape.com.br/rprimi/ds/stopwords.txt", 
  header = FALSE,
  col.names = "palavras") 
#Remove espaços em branco das stop words
stopwords <- stopwords %>% 
  mutate.(palavras = str_replace_all(string = palavras, pattern = " ", repl = ""))

#Cria o dataset para a nuvem de palavras associadas aos casos com menção à febre
Nuvem <- SCS %>% 
  filter.(year(dt_atendimento)==2022) %>% 
  filter.(!is.na(queixa)) %>% 
  filter.(febre == 1) %>% 
  select.(queixa) %>% 
  #Remove caracteres especiais de palavras
  mutate.(queixa = stri_trans_general(queixa, "Latin-ASCII")) %>%
  #Remove espaços em branco antes e depois das palavras
  mutate.(queixa = trimws(queixa)) %>%
  #Tokeniza as palavras
  unnest_tokens("Palavras", "queixa") %>% 
  #Remove stop words das palavras
  anti_join.(stopwords, by = c("Palavras" = "palavras")) %>% 
  #Substitui por missing as stopwords
  mutate.(Palavras = str_replace_all(Palavras, '[[:digit:]]', '')) %>% 
  #Substitui por missing as pontuações
  mutate.(Palavras = str_replace_all(Palavras, '[[:punct:]]', '')) %>% 
  #Substitui por missing o termo paciente e assim por diante
  mutate.(Palavras = str_replace_all(Palavras, 'paciente', '')) %>% 
  mutate.(Palavras = str_replace_all(Palavras, 'relata', '')) %>% 
  mutate.(Palavras = str_replace_all(Palavras, 'refere', '')) %>% 
  mutate.(Palavras = str_replace_all(Palavras, 'sintomas', '')) %>% 
  mutate.(Palavras = str_replace_all(Palavras, 'ha', '')) %>% 
  #Substitui por missing as palavras dia ou dias
  mutate.(Palavras = str_replace_all(Palavras, '^dias?$', '')) %>% 
  mutate.(Palavras = str_replace_all(Palavras, 'fez|uso', '')) %>% 
  mutate.(Palavras = str_replace_all(Palavras, 'ontem|hoje|inicio', '')) %>% 
  ##Substitui por missing palavras com dois caracteres apenas
  mutate.(Palavras = str_replace_all(Palavras, '^[a-z]$', '')) %>% 
  #mutate.(Palavras = str_replace_all(Palavras, '^[a-z]$', '')) %>% 
  mutate.(Palavras = str_replace_all(Palavras, 'febre', '')) %>% 
  #Remove todos os registros em branco
  filter.(Palavras != "") %>% 
  #Conta as palavras
  count(Palavras, sort = TRUE) %>% 
  #Cria contagens únicas das palavras por palavras únicas
  mutate.(Palavras = factor(Palavras, levels = rev(unique(Palavras)))) %>% 
  ungroup()
#Plota a nuvem de palavras
PlotNuvem <- wordcloud2(data = Nuvem)

TesteMes <- SCS %>% 
  filter(month(dt_atendimento) == 1)

table(month(SCS$dt_atendimento))


#Recently I found myself with some free time on my hands so I decided to learn a new skill, or at least start learning. So I thought to myself, what would be a good skill that would help me as a data analyst or would have helped me in the past? It had to be something that took a lot of time and that could be automated.

Introduction
Recently I found myself with some free time on my hands so I decided to learn a new skill, or at least start learning. So I thought to myself, what would be a good skill that would help me as a data analyst or would have helped me in the past? It had to be something that took a lot of time and that could be automated.

And then it struck me, TEXT ANALYSIS!
  
  I always hated the annoying task of having to analyse text, be it in the form of comments on the internet, transcripts from focus groups or something else, does not matter. As you might know, if you’ve done this sort of analysis, this can be a really boring and tedious work and can take a large amount of time. The way I used to do it was to read the comments, categorize each one then do some basic analysis based on those categories.

You can avoid spending the huge amount of time on this situation if you learn how to analyse text using a programming language and here is why. You can be required to analyse text either as a recurring report or as a one time analysis. In both cases text analysis can be very beneficial.

In the first case is kind of self-explanatory. You need to spend the time to set up the analysis, graphs and report, but this needs to be done just once and you can use it every time you want to refresh the report. The other solution is to analyse the data manually every time.

Now, what about the second case, when you have a one time report? Wouldn’t it take just as much time to set up the report as it would to analyse it manually? Well, no and you will see in the article below how easy it can be to analyse data using R.

So, in order to see how to analyse text using R I have started reading Text Mining with R by Julia Silge and David Robinson. I highly recommend this book as their approach is to transform the text into a tidy format that allows you to easily analyse and visualize the results using graphs.

Disclaimers and the structure
I would like to shortly discuss the structure of the article and make some disclaimers about it, so we are on the same page.

This article is intended just as an introductory example into what text analysis can do and how it can be used by data analysts, although I encourage you to study further if you think these methods can be useful. It is not intended as a comprehensive course on Natural Language Processing (NLP), as that is a complex topic that cannot be dealt with in just one article. Here, I will just show you three methods that can cover a great deal of analytical needs in a company.

I would also like to point out that I will show some basic sentiment analysis methods, however they do not cover all the possibilities and are just the tip of the iceberg. That being said, with some tweaking, they can reliably be used as a starting point in the endeavours to automate this process, with more complex methods being added at a latter time.

So, with the disclaimers out of the way we will discuss:
  
  Word Frequencies
Comparisons Between Texts
Sentiment Analysis
Wordclouds
The Data
As a dataset, I though that a series of phone reviews would be a good starting point.

As an analyst, it might be required of you to spend some time analysing reviews for different products your company makes and get insight from said reviews. As a dummy dataset I have chosen a series of reviews for the OnePlus phone models. You can check the reviews here.

I could have chosen any other brand of phone, or any other product for that matter, however I own a OnePlus. I want to check what is the general opinion about them and see if my decision was right or was it just bias.

See, these text analysis skills can be used for selfish reasons as well, it doesn’t always have to be something “useful” or “productive”. You can learn them just so you can allow yourself to be too lazy to read a book or a review front to back.

Now let’s download the data.

# We will need a URL from where to download the data
# In this case we will do it from my GitHub repository
# You can download the data using this link
url <- "https://raw.github.com/VladAluas/Text_Analysis/master/Datasets/Text_review.csv"
# I prefer vroom to ingest csv, but you can use readr::read_csv() if you fancy it more
reviews <- vroom::vroom(url)
#> Rows: 433
#> Columns: 3
#> Delimiter: ","
#> chr [3]: Model, Segment, Text
#> 
#> Use `spec()` to retrieve the guessed column specification
#> Pass a specification to the `col_types` argument to quiet this message


The structure of the data can be seen below:
  
  head(reviews)
#> # A tibble: 6 x 3
#>   Model    Segment                Text                                          
#>   <chr>    <chr>                  <chr>                                         
#> 1 OnePlus~ Introduction           "The days of the $600 smartphone aren't over ~
#> 2 OnePlus~ Design, Features, and~ "The OnePlus One doesn't feel like a sub-$400~
#> 3 OnePlus~ Design, Features, and~ "Our white test unit features a so-called sil~
#> 4 OnePlus~ Design, Features, and~ "The 5.5-inch, 1080p IPS display is on par wi~
#> 5 OnePlus~ Design, Features, and~ "There are two speaker grilles flanking the m~
#> 6 OnePlus~ Design, Features, and~ "With GSM (850/900/1800/1900MHz), UMTS (Bands~


As you can see, the data is structured in three columns: the model number, the segment of the review and the text from the segment.

I have chosen to keep each paragraph from each review as a separate text because it’s easier to work with, and it’s more realistic. This is most likely how you might analyse the data when you read and compare the reviews section by section.

Now that we have the data, I want to discuss the text analysis principles that I will use in this article.

Text Analysis
As you can see, it is quite hard to work with the data at the moment. You can’t count words or quantify them in any way, so we will need to transform the last column into a more analysis friendly format.

As mentioned in the introduction, we will use some methods developed by Julia Silge and David Robinson in the tidytext package. The function that we will “abuse” in this article is unnest_tokens().

This function allows us to transform the text column into a tidy format (see here).

Let’s see it in action.

# We need to activate some additional libraries
# If you do not have the libraries installed you can install them using: install.packages(c("tidyverse", "tidytext"))
library(tidytext)
library(tidyverse)
#> Registered S3 methods overwritten by 'readr':
#>   method           from 
#>   format.col_spec  vroom
#>   print.col_spec   vroom
#>   print.collector  vroom
#>   print.date_names vroom
#>   print.locale     vroom
#>   str.col_spec     vroom
#> -- Attaching packages --------------------------------------- tidyverse 1.3.0 --
#> v ggplot2 3.3.3     v purrr   0.3.4
#> v tibble  3.0.6     v dplyr   1.0.4
#> v tidyr   1.1.2     v stringr 1.4.0
#> v readr   1.4.0     v forcats 0.5.1
#> -- Conflicts ------------------------------------------ tidyverse_conflicts() --
#> x dplyr::filter() masks stats::filter()
#> x dplyr::lag()    masks stats::lag()
reviews %>%
  # We need to specify the name of the column to be created (Word) and the source column (Text)
  unnest_tokens("Word", "Text") 
#> # A tibble: 30,067 x 3
#>    Model     Segment      Word      
#>    <chr>     <chr>        <chr>     
#>  1 OnePlus 1 Introduction the       
#>  2 OnePlus 1 Introduction days      
#>  3 OnePlus 1 Introduction of        
#>  4 OnePlus 1 Introduction the       
#>  5 OnePlus 1 Introduction 600       
#>  6 OnePlus 1 Introduction smartphone
#>  7 OnePlus 1 Introduction aren't    
#>  8 OnePlus 1 Introduction over      
#>  9 OnePlus 1 Introduction quite     
#> 10 OnePlus 1 Introduction yet       
#> # ... with 30,057 more rows
As you can see, the function took all the sentences from the Text column and broke them down into a format that has one word per row and way more rows than before. So, our new data structure is one step away from a tidy format, all we need to do is count each word to see how many times it appears in the text, and then we will have a tidy format.

I would also like to point out, as you have already probably noticed, that the function has transformed all the words to lower case and removed all the special symbols (e.g. the $ from the price described in the introduction of the OnePlus 1). This is important because it can save us a lot of headaches when cleaning the data.

Now we will transform the data in a proper tidy format. To do so, we will unnest the sentences, we will count each word, and then we can display the frequencies on a graph.

Because we want to use the graph later, we will create a function, word_frequency() that contains all the steps we want to apply to the graph. We will also replace some characters, so we will not double or under count some words.

reviews_tidy <- reviews %>%
  unnest_tokens("Word", "Text") %>%
  # We also want to prevent the analysis in showing 6t and 6t's as two separate words
  mutate(Word = str_replace(Word, "'s", ""))
# We want to display graphically a word frequency plot
# We will create a function that will store all the operations we will repeat several times
word_frequency <- function(x, top = 10){
  
  x %>%
    
    # We need a word count
    count(Word, sort = TRUE) %>%
    
    # We want to create a factor from the word column with the levels showing the most frequent words as top level
    # This is just for aestethic reasons, however, it helps make the point
    mutate(Word = factor(Word, levels = rev(unique(Word)))) %>% 
    # We use the "top" variable defined in the function so we can decide how many words we want to use 
    top_n(top) %>%
    
    # This will be useful later if we want to use a grouping variable and will do nothing if we don't  
    ungroup() %>%
    
    # The graph itself
    ggplot(mapping = aes(x = Word, y = n)) +
    geom_col(show.legend = FALSE) +
    coord_flip() +
    labs(x = NULL)
}
reviews_tidy %>%
  word_frequency(15)
#> Selecting by n

There, frequency analysis done. Now, what does the word the say about the OnePlus brand of phones?
  
  Nothing!
  
  As you can see and might have expected, determiners and conjunctions (e.g. the, and, a, to) are the most frequently used words in any language and do not tell us much about the message of a sentence, not by themselves at least. These are called stop words, and we will eliminate them, so we can focus on the words that can give us a better picture of the text.

Fortunately for us, the tidytext package provides a dataset called stop_words (what else) that contains a list of all the determiners and conjunctions, adverbs and adjectives that we can eliminate from a text, so we can analyse it properly.

This dataset contains only stop words from English. If you analyse a different language, you would have to use a different dataset or create one for yourself.

With that in mind, I think we can recreate the previous graph after we eliminate the stop words and see what it tells us about the OnePlus phones overall.

# Same dataset as before with an extra code line
reviews_tidy <- reviews %>%
  unnest_tokens("Word", "Text") %>%
  anti_join(stop_words, by = c("Word" = "word")) %>% # anti_join just keeps the rows common to both data sets
  mutate(Word = str_replace(Word, "'s", ""))
# The graph is the same as before, we just changed the dataset
reviews_tidy %>%
  word_frequency(15)
#> Selecting by n

Slightly better, don’t you think?
  
  So, as an overall idea, we can see that the brand name (OnePlus) is the most used, as we would expect. Then, we can see phone, which is to be expected since we are talking about a product that is a phone.

We can also see that galaxy is mentioned quite a lot, just as much as camera which is again expected. OnePlus promoted themselves as a brand with high performance models at a cheaper price than a flagship from Samsung or other makers, therefore it would be only natural to see the comparison between the two.

However, if you reverse the analysis, you might not see OnePlus in a Samsung review because Samsung is the gold standard against which everyone is compared, while OnePlus is not.

Another pairing we see is low and light which is the part in the reviews where they are comparing camera performance in low light.

Also you might have spotted that 7 and 8 are there as well. This is most likely because the 7 from all the OnePlus 7 series is mentioned quite a lot, the same goes for the 8. This can be avoided, but it requires an extra step.

You will need to replace the spaces in the model name (e.g. mutate(Word = str_replace(Word, "OnePlus 7", "OnePlus_7))), and do this for all the models not just OnePlus 7

I will not do this, but you are welcome to try and let me know how the analysis changed.

Now, the graph we used earlier shows us the most frequently used words across all texts in the corpus. This is useful because it gives us some good insight on what are the words most associated with OnePlus as a brand overall.

But, I would also like to have the top 5 words associated with each model. We can do so by adding two lines of code to the previous chunk. It’s as simple as:

reviews_tidy %>%
  group_by(Model) %>% 
  word_frequency(5) +
  facet_wrap(~ Model, scales = "free_y") # This is just to split the graph into multiple graphs for each model
#> Selecting by n

Cool, right?

We have a matrix of graphics that shows us which terms are most frequently associated with a model and that is very useful from a business perspective. Although, keep in mind that these terms might require some cleaning. You might have words that are not useful for your analysis.

There might be some cases in which you have a known fault in your product and it can be so frequent that it overshadows every other feedback. In this particular case an example can be the word oneplus. This word has no relevance to the analysis, we know the name of the brand we are interested in, and having this word in the graphs might obscure the presence of a more relevant word.

It can be anything else, let’s say a battery problem, you name it, I’m sure you have your own example in mind.

Should that be the case, there is a simple solution for that. Add the undesired words to the stop_words list and start the analysis all over again and it will give you the most frequent words that are of interest.

I would suggest using this very sparsely, as you might overlook some crucial information that at some point seemed unimportant and now you forgot that is forcefully removed.

Comparison between models
That is all well and good, but in the end, we still have to analyse the graphs using our instincts and determine what is the conclusion for each model, right? This cannot be automated!

Yes, it can!!!

For our purpose we will use a method called Term frequency – inverse document frequency (tf_idf for short and the fact that it rolls off the tongue easier). You can read more about this here, but I will also give you a TL;DR here.

As you could see in the first graph, the most frequent terms in the review are the ones with no analytical value whatsoever, the, and, a, etc. Words that have a high analytical value (e.g. performance) will appear less often.

Kepping that in mind, the tf_idf method works based on this principle something like this. The word oneplus is in all reviews so that does not tell us anything about a particular document. The model number on the other hand, is specific for each review and therefore way more important in helping us distinguish between the document. The same can go for the top tier rival phone models to OnePlus at the time of the review (e.g. Galaxy S20).

This can be used to our advantage with a bit of a twist. We can check for the words that are frequent in one review and not the others to see what distinguishes one document from another.

This comparison can be done with a simple formula bind_tf_idf() that assigns weights to words using the principles below:

words with high frequency in all the documents: low weight
words with high frequency in just one of the documents and not the other: high weight
words with low frequency across the board: low weight
Let’s see this in practice:

review_tf_idf <- 
  reviews_tidy %>%
    count(Model, Word, sort = TRUE) %>%
    bind_tf_idf(Word, Model, n)
review_tf_idf %>%
  arrange(desc(tf_idf))
#> # A tibble: 8,213 x 6
#>    Model              Word        n     tf   idf tf_idf
#>    <chr>              <chr>   <int>  <dbl> <dbl>  <dbl>
#>  1 OnePlus 6T         6t         31 0.0293 2.08  0.0609
#>  2 OnePlus 5T         5t         21 0.0195 2.77  0.0540
#>  3 OnePlus 6          s9         18 0.0183 2.77  0.0508
#>  4 OnePlus 7T McLaren mclaren    20 0.0292 1.67  0.0489
#>  5 OnePlus 2          s6         11 0.0164 2.77  0.0456
#>  6 OnePlus 7 Pro 5G   5g         40 0.0620 0.693 0.0430
#>  7 OnePlus 7T         7t         22 0.0306 1.39  0.0425
#>  8 OnePlus 8 Pro      s20        25 0.0245 1.67  0.0410
#>  9 OnePlus 8          s20        25 0.0223 1.67  0.0373
#> 10 OnePlus 3T         3t         21 0.0222 1.67  0.0372
#> # ... with 8,203 more rows
Now we can display this using plots.

review_tf_idf %>%
  
# We need to sort the data in descending order so we can create the factors for each term
  arrange(desc(tf_idf)) %>%
# We create the factors as we did previously
  mutate(Word = factor(Word, levels = rev(unique(Word)))) %>%
# Select just the top 5 words for each model
  group_by(Model) %>%
  top_n(5) %>%
  ungroup() %>%
# Our Plot
  ggplot(mapping = aes(x = Word, y = tf_idf, fill = Model)) +
  geom_col(show.legend = FALSE) +
  labs(x = NULL, y = NULL) +
  coord_flip() +
  facet_wrap(~ Model, scales = "free_y")
#> Selecting by tf_idf

As we can see, these are the main items that separate one review from the other. Amongst them we can see the main flagship of Samsung, especially for latter reviews, they seem to compare the brands quite a lot.

We can also single out that for One Plus 7 Pro 5G there is a problem with overheating and the OnePlus 6 is described as elegant.

Of course, this can be tweaked quite a bit depending on your needs. You can eliminate words, you can replace some of them, or you can add a different grouping to the analysis. I’m pretty sure you have an idea on what you would change when you apply it to your analytic needs.

Sentiment Analysis
Now, finally the good part.

In this segment, I would like to discuss some basic principles of sentiment analysis and how they can be used in data analysis to quickly get an idea about your product. So, how do we achieve that?

Well, the most basic method, and the one that we will cover today, is to simply associate each word in the review to a sentiment. Then it becomes a simple matter of counting how many words are associated with positive or negative sentiments to get the overall affect of the text.

This is quite straight forward and the tidyverse package comes to our aid with some libraries that already have these associations made and are also validated against multiple sources. The libraries are:

AFFIN from Finn Årup Nielsen
bing from Bing Liu and collaborators
nrc from Saif Mohammad and Peter Turney
Each of these libraries is helpful in its own way and approaches sentiment analysis differently. Let’s check them:

get_sentiments("afinn")
#> # A tibble: 2,477 x 2
#>    word       value
#>    <chr>      <dbl>
#>  1 abandon       -2
#>  2 abandoned     -2
#>  3 abandons      -2
#>  4 abducted      -2
#>  5 abduction     -2
#>  6 abductions    -2
#>  7 abhor         -3
#>  8 abhorred      -3
#>  9 abhorrent     -3
#> 10 abhors        -3
#> # ... with 2,467 more rows
get_sentiments("bing")
#> # A tibble: 6,786 x 2
#>    word        sentiment
#>    <chr>       <chr>    
#>  1 2-faces     negative 
#>  2 abnormal    negative 
#>  3 abolish     negative 
#>  4 abominable  negative 
#>  5 abominably  negative 
#>  6 abominate   negative 
#>  7 abomination negative 
#>  8 abort       negative 
#>  9 aborted     negative 
#> 10 aborts      negative 
#> # ... with 6,776 more rows
get_sentiments("nrc")
#> # A tibble: 13,901 x 2
#>    word        sentiment
#>    <chr>       <chr>    
#>  1 abacus      trust    
#>  2 abandon     fear     
#>  3 abandon     negative 
#>  4 abandon     sadness  
#>  5 abandoned   anger    
#>  6 abandoned   fear     
#>  7 abandoned   negative 
#>  8 abandoned   sadness  
#>  9 abandonment anger    
#> 10 abandonment fear     
#> # ... with 13,891 more rows
I want to discuss these libraries just a bit.

The AFINN library gives a score between -5 and +5 to each word. Once this is done, the sentiment can be inferred by summing up the scores.

The bing library simply associates a word with a negative or positive valence. At the end we can count how many words are positive or negative.

The nrc library is interesting because it gives you a list of words that can be classified in multiple ways. As you can see, the second element, can be classified either as fear, negative or sadness. This is useful if you want to check for a specific sentiment, or a list of specific sentiments in a text (e.g. just how many terms are associated with fear)

Let’s proceed by using the AFINN library to check the sentiment for each model and see how they perform. We will use just the conclusion for each review as that should be the most relevant in transmitting the overall sentiment for the whole review.

However, we have to keep in mind that these being technical reviews, they might contain a terminology different from the one used in natural language, and the analysis might not be as accurate as an analysis on Facebook posts, for example.

conclusion_afinn <- reviews %>%
  filter(str_detect(Segment, "Conclusion")) %>%
  unnest_tokens("Word", "Text") %>%
  anti_join(stop_words, by = c("Word" = "word")) %>%
# We will get the sentiments with a inner_join since the words that don't have a match, don't have a score value
  inner_join(get_sentiments("afinn"), by = c("Word" = "word"))
conclusion_afinn
#> # A tibble: 122 x 4
#>    Model     Segment                 Word     value
#>    <chr>     <chr>                   <chr>    <dbl>
#>  1 OnePlus 1 Cameras and Conclusions cut         -1
#>  2 OnePlus 1 Cameras and Conclusions true         2
#>  3 OnePlus 1 Cameras and Conclusions alive        1
#>  4 OnePlus 1 Cameras and Conclusions true         2
#>  5 OnePlus 1 Cameras and Conclusions miss        -2
#>  6 OnePlus 1 Cameras and Conclusions straight     1
#>  7 OnePlus 1 Cameras and Conclusions capable      1
#>  8 OnePlus 1 Cameras and Conclusions free         1
#>  9 OnePlus 1 Cameras and Conclusions demand      -1
#> 10 OnePlus 1 Cameras and Conclusions impress      3
#> # ... with 112 more rows
As you can see, each token has been unnested, and assigned a sentiment value.

Now, in order to check the sentiments for each review, all we need to do is add the scores and plot them.

conclusion_afinn %>%
  group_by(Model) %>%
  summarise(Score = sum(value)) %>%
  arrange(desc(Score)) %>%
  mutate(Model = factor(Model, levels = rev(unique(Model)))) %>%
  ggplot(mapping = aes(x = Model, y = Score)) +
  geom_col() +
  coord_flip() +
  labs(x = NULL)

The scores are in and overall the Oneplus 2 has the best reviews.

However, what if we want to see a report on which model has the most positive and negative reviews? For that we would use the bing library.

Let’s see how:

conclusion_bing <- reviews %>%
  filter(str_detect(Segment, "Conclusion")) %>%
  unnest_tokens("Word", "Text") %>%
  anti_join(stop_words, by = c("Word" = "word")) %>%
  inner_join(get_sentiments("bing"), by = c("Word" = "word"))
conclusion_bing
#> # A tibble: 189 x 4
#>    Model     Segment                 Word       sentiment
#>    <chr>     <chr>                   <chr>      <chr>    
#>  1 OnePlus 1 Cameras and Conclusions led        positive 
#>  2 OnePlus 1 Cameras and Conclusions distortion negative 
#>  3 OnePlus 1 Cameras and Conclusions miss       negative 
#>  4 OnePlus 1 Cameras and Conclusions dynamic    positive 
#>  5 OnePlus 1 Cameras and Conclusions distortion negative 
#>  6 OnePlus 1 Cameras and Conclusions warped     negative 
#>  7 OnePlus 1 Cameras and Conclusions unnatural  negative 
#>  8 OnePlus 1 Cameras and Conclusions admirable  positive 
#>  9 OnePlus 1 Cameras and Conclusions soft       positive 
#> 10 OnePlus 1 Cameras and Conclusions prefer     positive 
#> # ... with 179 more rows
Now we can proceed with the same steps, just add the sentiment to the grouping.

conclusion_bing %>%
  group_by(Model, sentiment) %>%
  count() %>%
  ungroup() %>%
  mutate(Model = reorder(Model, n)) %>%
  ggplot(mapping = aes(x = Model, y = n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  labs(x = NULL, y = "Negative vs positive sentiment / Model") +
  facet_wrap(~ sentiment, ncol = 2)

Here we can see a more nuanced approach.

For example, the OnePlus 6T and the OnePlus 7 (for China) have no negative reviews, but they also have only a few positive things said about them. This seems to be reflected in their placement in the previous graph as well.

A curious case can be for the OnePlus 3 which seems to have more positive than negative reviews, however as an overall score it is dead last on a positivity ranking. This indicates that the review did not regard this model with high praise or the really negative descriptions were very negative. Most likely a combination of both.

Both these approaches have their advantages and disadvantages and in practice you will most likely use a combination of both, not just one. It is really useful to view a problem from multiple angles. You never know which method is helpful for the decision makers in your company.

With that in mind, I would like to discuss one more method of presenting the data that might help in some situations more than graphs.

WordClouds
In this section I would like to show you a different approach in presenting the data, WordClouds.

I personally find them very useful when you are trying to communicate the prevalence of a word in a text or speech. They basically have the same role as a pie chart, but they’re way better because they display data in a more user-friendly way. Using a wordcloud will allow you to look at it and see how frequent a word is without having to check and re-check a legend for dozens of times.

With that said, let’s check our wordcloud. It should show the same data as the first graph, just in a different display style, so I will use the same data set reviews_tidy.

For this I will use the wordcloud package. If you do not have it installed, you can install it by using install.packages("wordcloud").

library(wordcloud)
#> Loading required package: RColorBrewer
reviews_tidy %>%
  count(Word) %>%
  with(wordcloud(Word, n, max.words = 100))

As you can see, the results are similar to the first analysis, the more frequent a word, the larger the font. However, with this type of graph we can include a lot more items. In this we have included 100 words, as opposed to 15 in the first graph.

Now, this clearly shows that the most prevalent word is oneplus followed by phone then pro and so on, exactly the same information we had before, the only difference is that we have a bigger picture using this method.

As mentioned, it’s a very useful way to show the prevalence of multiple words in a text.

Now, I would like to show you how you can use a wordcloud for sentiment analysis.

library(reshape2)
#> 
#> Attaching package: 'reshape2'
#> The following object is masked from 'package:tidyr':
#> 
#>     smiths
reviews_tidy %>%
  inner_join(get_sentiments("bing"), by = c("Word" = "word")) %>%
  count(Word, sentiment, sort = TRUE) %>%
  acast(Word ~ sentiment, value.var = "n", fill = 0) %>%
  comparison.cloud(colors = c("#202121", "#797C80"),
                                                                                      max.words = 50)
                                                                   
                                                                   This is a very quick and useful way to show which elements influence the sentiment for your product the most and make decisions based on it.
                                                                   
                                                                   We can clearly see that the words that influence the most the negative scores are noise, expensive and loud while the ones that influence the positive reviews are excellent, fast and smooth.
                                                                   
                                                                   Sentiment Analysis wrap up
                                                                   There is more to be said about sentiment analysis, however in this article I just wanted to give you a short introduction and show some basic principles for it.
                                                                   
                                                                   I’m sure you have noticed some of the quirks yourself. For example the lexicons we have used here are intended for just one word, and that can miss the sentiment of a phase (e.g. not good is a negative term, however the lexicon will see not as neutral and good as positive, therefore overall it will see it as positive). In order to avoid situations like this we can use pairing of words and check for these types of situation.
                                                                   
                                                                   I plan to go into more detail in a series of articles on the subject, so if you find something interesting or have a topic to discuss, please let me know.
                                                                   
                                                                   Conclusion
                                                                   When I read the book, I realised that this type of analysis is very scalable, if done with a programming language like R. By this I mean that a computer will have no problem in analysing 10 or 30.000 words in about the same time. For a human the difference is huge.
                                                                   
                                                                   Besides being scalable, this analysis can be done multiple times without having to change the code or spend the time setting everything up.
                                                                   
                                                                   The last advantage of this is that this method is not prone to human error or fatigue. Let’s be honest, we as humans get tired after some time spent in the same task, and we can get errors. Why not avoid this if possible?
                                                                     
                                                                     If you have to do these types of analyses, please let me know if you think this article could be useful in your day to day work, and if you have applied them, please let me know how it influenced your time spent on the analysis.
                                                                   
                                                                   Also, check Julia and David’s book and if it helped you and you can afford it, buy it to show your support of their great work.