
<!-- README.md is generated from README.Rmd. Please edit that file -->

# rTermCount

rTermCount is an R package designed to help users count occurrences of
specific terms in a given text corpus. It provides functions to prepare
a list of terms to search for, tokenize the text corpus, and retrieve
the positions of the terms in the tokenized text.

## Installation

You can install the development version of rTermCount from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("MatthiasUckert/rTermCount")
```

## Usage

``` r
library(rTermCount)
#> Loading required package: data.table
## basic example code
```

Before counting the occurrences of specific terms in a text corpus, we
need to prepare the term list and the corpus itself. Fortunately,
rTermCount includes an example term list and an example text corpus that
can be used for testing purposes.

The example term list is a two-column data frame containing the terms to
search for and their corresponding IDs. To access the example term list,
simply call **`table_terms`** in R.

``` r
# View the example term list
head(table_terms)
#>   tid              term origin
#> 1   1           Twitter   GPT3
#> 2   2         in London   GPT3
#> 3   3            London   GPT3
#> 4   4      exact number   GPT3
#> 5   5            number   GPT3
#> 6   6 Government Office   GPT3
```

The example text corpus is a data frame containing text data that can be
used for testing. To access the example text corpus, simply call
**`table_doc`** in R.

``` r
# View the example text corpus
head(table_doc)
#>                               doc_id
#> 1 the_importance_of_being_on_twitter
#>                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                                              text
#> 1 The importance of being on twitter \n\nby Jerome K. Jerome \nLondon, Summer 1897  \n\nIt is a curious fact that the last remaining form of social life in which the people of London are still interested is Twitter. I was struck with this curious fact when I went on one of my periodical holidays to the sea-side, and found the whole place twittering like a starling-cage. I called it an anomaly, and it is. I spoke to the sexton, whose cottage, like all sexton's cottages, is full of antiquities and interesting relics of former centuries. I said to him, "My dear sexton, what does all this twittering mean?" And he replied, "Why, sir, of course it means Twitter." "Ah!" I said, "I know about that. But what is Twitter?" "It is a system of short and pithy sentences strung together in groups, for the purpose of conveying useful information to the initiated, and entertainment and the exercise of wits to the initiated, and entertainment and the exercise of wits to the rest of us." "Very interesting," I said. "Has it a name?" "It has," he said; "it is called Twitter. "Yes," I said, "I know that, but what is it?" "It is a system of information," he said. "Oh, yes," I replied; "but what is it?" "Why, sir," he said, "you can go up to any of the gentlemen you see twittering in the street, and say to him, 'You are a fool,' or 'Your wife is an adulteress,' or 'You have stolen that hat,' and if he is a member of the initiated he will answer you in the same form and tell you that you are a liar, or that your eyes resemble the eyes of a duck, or that you have stepped out of your part in the last charade you acted in, or that you were for a short time a statistician in a Government Office, and he will go on to tell you the whole story of your life, in language so exceedingly small and pointed that even you will be glad you can't understand it. There are more than seven thousand gentlemen in London who have taken a life-lease of Twitter, and are allowed to twitter freely and without let or hindrance on any subject they like, and nobody is allowed to contradict them." I said, "That is a very attractive form of amusement."     \n\n\n\f"It is," he said; "at the end of a dozen years a man is the most entertaining company in London." "But how do the other people amuse themselves?" I asked. "They twitter," he replied. "There are thousands and thousands of gentlemen in London who are not allowed to twitter freely. They can only twitter when they are asked questions, and the effect is very like the effect of those cross-examinations that used to take place before magistrates in the days when our judges were not so refined as they are now. The magistrate would say, 'So you stole that watch,' and the criminal, who had not stolen the watch at all, but only had a tender conscience and had just been running away from his friends to avoid answering some awkward question about a girl, would say, 'No, I didn't steal it.' The magistrate would say, 'Don't lie; I have evidence to prove that you did.' Then the criminal would say, 'Oh, well, if you put it that way, I did steal it, but it's a very good watch, and you ought to be glad I stole it.' Then the magistrate would say, 'Oh, you did steal it. You know you did steal it. You admit that you stole it, so we won't waste any more time over the matter.' That would be a sample of Twitter." Then I said to the sexton, "The people of London twitter every day on more or less useful subjects, do they not?" "They do," he said; "you may meet with any amount of twittering in London on any subject you can think of from politics to the so-called higher mathematics." "Do they not find out," I said, "that a twitter on mathematics would be as useful as a twitter on Shakespeare?" "No," he said, "they are two distinct subjects. I will explain the difference by an analogy." "Do," I said. "Well," he said, "suppose I had a ring with a note inside it that you were to open in the event of my death. In that case I could make the note of any length I liked." "That is just the point," I said. "I could have a long note that you would only open after many years, and you would find it full of the most interesting information about that foreign country you are always telling me about. Or I could have a very short note, quite a twitter, about going out to walk in the country with you to-morrow. The short note would   \n\n\fconvey just as much information as the long note, if you got it at once, but if you waited for the long note you would get information about foreign countries for nothing." "Oh, yes," I said. "Now, in the same way," he said, "mathematics are the long notes and their application to daily life is the short notes." "I understand that perfectly," I said. "Do you, sir?" he said. "Then suppose I tell you that some men have short notes on mathematics, and others have long notes on mathematics. Now the short-note men twitter all the week, and the long-note men twitter only on Sundays." "That is quite true," I said. "Now, suppose I was to tell you that I had taken out a lease of Twitter and could twitter whenever I liked, could I not?" "Yes," I said, "I suppose you could." "Suppose I twitter," he said, "that it is better to spend money freely in youth than in old age. Is that useful?" "No," I said. "Very well," he said. "Suppose I say that marriage is the firmest foundation for happiness, is that true?" "I will let you know that when I am married," I said. "Suppose," he said, "I say that a nice hot cup of tea is the best thing in the world for a hungry man in the morning. Is that useful?" "Not as useful," I said, "as a cold bath." "And suppose I say that cherry brandy is better than champagne for a convalescent."  \n\n\f"That would depend," I said, "on the convalescence." "Then I am as useful as anybody else." "Just so," I said. Then he went on, "All the other Twitter gentlemen have to wait for questions to twitter upon. I am the only one who can twitter what I please." "It is a grand position," I said. "It is," he said. "I am in a position to say that the ends of life are love and hate, music and silence. I have tweeted as much hundreds of times, and everybody has laughed and gone away, and afterwards, when they have been too busy or too tired to say anything else, they have remembered the old sexton's saying, 'The ends of life are love and hate, music and silence,' and have been very much pleased with themselves for remembering it. If I were to write a book, it would be called 'Two Thousand One Hundred and Eighty-nine.'" "It sounds a very suitable title," I said. "I am the only person," he said, "who can remember the Flood as well as the present year. I am the only person who has tweeted the exact number of bricks that were used in the Tower of Babel, and the exact number of tiles that were used to cover the house of the evil-doer in the city of the plain. When a man has tweeted that much he is in a position to twitter anything he likes." "I congratulate you," I said, "on the field you have won." "Thank you," he said. "The object of the exercise is to make people talk; and that is what they have been doing all day and all night for hundreds of years. Don't you think that it is a wonderful thing that a man who has never opened a book should be able to keep a whole society in talk by the twitter of a summer-house?" "Yes," I said, "it seems to me a remarkable achievement." "As I said before," he replied, "I am a peculiarly fortunate man. I am one of the seven thousand."  \n\n\fThen I said to him, "What is the object of the exercise?" "It is to keep people talking," he said. "I will tell you what happened. "Twelve thousand years ago, just as people were beginning to get tired of doing all their talking with their tongues, which have such limited capacities, a certain number of persons discovered that it was possible to do it with their fingers, which have such unlimited capacities. The discovery was not made simultaneously by the whole of the twelve thousand. Some people began, as usual, and others followed, as usual; and it was those who followed who decided that it was the twitter that mattered. The twitter was not taken very seriously at first. For a long time there was considerable doubt as to whether it was or was not an advantage to the world. People went on doing their talking with their tongues just the same as if there were no such thing as a twitter. That was because they were sceptical about the value of the invention. There are many inventions in the world which were invented by persons of whose ability we have no knowledge, and are therefore sceptical of the value of their inventions. Twitter is an example of that. I could quote you a hundred other cases. Twitter had its first successes in England; and the chief of them was the conversion of the Duke of Portland. It is a curious fact that the old duke was one of the first people to invent a twitter. But the duke invented one which was not like that which is popular at present. The duke's twitter was put together in such a way that it could be carried about with him in his pocket. You pulled a string, and it made a noise, but that was not a twitter. That was a sort of a speciality twitter. A man with his brain in his pocket might walk about the streets of London shouting at people; but he could not twitter. The duke was one of the earliest English twittering-drones, and one of the first to see the superiority of the twitter over the tongue. He knew that the twitter could be carried about, like the cowpox, in the pocket. He knew that any man with the right instrument could go about among his fellow-men twittering, and so keep them talking without intermission, and keep them from doing anything else except twittering back to him. The people would talk more and more, and after a time they would find they had nothing to talk about except themselves, and then they would have to invent ways of inventing things to talk about. Twitter, like the cowpox, seems to have that effect on mankind. The duke, however, was not satisfied with inventing a twitter that would twitter. He felt that the true function of a twitter was to keep people talking. And to do  \n\n\fthat, it would be necessary to do what the inventor of the pump is supposed to have done when he found out how to make a pump work. I mean that he would have to go about showing his invention to every one he met, and explaining how it worked, and giving a demonstration of it. After a while the twitter would spread by itself, and everybody in England would have one; and then it would spread all over the world, and then people would begin to find that they had to talk to each other on a subject of some sort, and the invention of conversation would begin. People who were too lazy to make conversation would find they had to invent it. Soon after that the twitter would be improved and made portable, so that it would no longer be necessary to carry about a large gilt chamber-pot in one's pocket. Then it would no longer be necessary to pull a string to twitter. Anybody who chose could twitter by simply pressing a button. By that time, however, the twitter would have spread to other parts of the world; and in the tropical forests the monkeys and the men who live in the trees would twitter in their own particular way, while the elephants in India would twitter differently from the elephants in Africa. Then it would spread to the stars, and the twitters would be in every language; and people would twitter to the twitters all the time, and get no answer, because there would be no one to twitter to them. Then it would spread to the sun, and the twitters would all burst with the heat; and all twittering would be over the world. There would be nothing left for any one to do but to sit on the sea-shore with his legs in the sea, and twitter to the twitters who were twittering to the twitters who were twittering, and be happy, because there would be nothing else to do. "You have a vivid imagination, sexton," I said. "But what do you twitter?" "Everything," he replied. "One goes on until one knows that one's tweets are universally known. Then one goes on for a little longer, and then one stops." "Why do you stop?" I said. "For two reasons," he said. "The first is that it is no longer necessary. The twitter is now worldwide. No one can go anywhere without hearing it in a concentrated form, to fit the intellect of the traveler. The second is that you have not your mind in your pocket, and cannot twitter yourself. That is why I continue to twitter. In order to keep myself in the twittering mood."
```

Both the example term list and text corpus can be used as inputs to the
**`prep_termlist()`** and **`prep_document()`** functions, respectively,
to prepare them for searching. For both inputs it is important to
standardize the text before we start the searching.

The standardization function used in rTermCount is **`std_str()`**. This
function takes a string as input and standardizes it by performing the
following operations:

- Replaces all non-word characters with spaces

- Removes all punctuation characters

- Removes excess whitespace and replaces it with a single space

- Converts all characters to lowercase

- Translates all non-ASCII characters to their ASCII equivalents

The **`std_str()`** function can also take an optional **`.op`**
argument, which allows the user to choose which operations to perform.
The available options are “space” (to remove excess whitespace), “punct”
(to remove punctuation characters), “case” (to convert all characters to
lowercase), and “ascii” (to translate non-ASCII characters to their
ASCII equivalents).

By standardizing the term list and corpus with the **`std_str()`**
function, users can ensure that the terms and corpus are consistent and
compatible, reducing the likelihood of missing or false positives in the
search results.

### **Preparing the Term List**

The term list is a two-column data frame containing the terms to search
for and their corresponding IDs.

``` r
head(table_terms)
#>   tid              term origin
#> 1   1           Twitter   GPT3
#> 2   2         in London   GPT3
#> 3   3            London   GPT3
#> 4   4      exact number   GPT3
#> 5   5            number   GPT3
#> 6   6 Government Office   GPT3
```

To prepare the term list for searching, we use the **`prep_termlist()`**
function. This function takes two mandatory inputs:

- **`.tab`**: The term list data frame, which should contain two
  columns: **`tid`** (the term ID) and **`term`** (the term to search
  for).

- **`.fun_std`**: The standardization function to standardize the terms
  before searching. In most cases, we can use the built-in
  **`std_str()`** function.

After running **`prep_termlist()`**, we obtain a new data frame with
three columns: **`tid`** (the term ID), **`ngram`** (the length of the
term), **`term`** (the standardized term), and **`term_orig`** (the
original term). The standardized terms are used for searching, while the
original terms are retained for reference.

``` r
termlist <- prep_termlist(
  .tab = table_terms,
  .fun_std = std_str
)

head(termlist)
#> # A tibble: 6 x 5
#>     tid ngram term              term_orig         origin
#>   <int> <int> <chr>             <chr>             <chr> 
#> 1     1     1 twitter           Twitter           GPT3  
#> 2     2     2 in london         in London         GPT3  
#> 3     3     1 london            London            GPT3  
#> 4     4     2 exact number      exact number      GPT3  
#> 5     5     1 number            number            GPT3  
#> 6     6     2 government office Government Office GPT3
```

### Preparing the Document

Before counting the occurrences of terms in a text corpus, we need to
tokenize the document into a data frame where each row represents one
token (usually a word). To prepare the document for searching, we can
use the prep_document() function, which takes three mandatory inputs:

.tab: The text data frame to tokenize. The data frame should contain a
column with the text to tokenize and, optionally, an ID column for each
document.

.fun_std: The standardization function to standardize the tokens before
searching. In most cases, we can use the built-in std_str() function.

.until: A character indicating the level of tokenization. The available
options are “tok” (token level), “sen” (sentence level), “par”
(paragraph level), and “pag” (page level). By default, .until is set to
“tok”, meaning that the text is tokenized at the word level.

After running prep_document(), we obtain a new data frame with one row
per token, and additional columns for the document ID (if provided), the
sentence ID (if .until is set to “sen”), and the paragraph ID (if .until
is set to “par”). The standardized tokens are used for searching, while
the original tokens are retained for reference.

Here is an example of how to use prep_document() to tokenize a text
corpus:

``` r
document <- rTermCount::prep_document(
  .tab = table_doc,
  .fun_std = std_str,
  .until = "tok"
)
```

### Counting Terms

Once we have prepared the term list and the text corpus, we can count
the occurrences of the terms in the corpus using the
**`position_count()`** function. This function takes three mandatory
inputs:

- **`.tls`**: The term list prepared with **`prep_termlist()`**.

- **`.doc`**: The document prepared with **`prep_document()`**.

- **`...`**: Any additional arguments to be passed to the function.

The **`...`** argument in the **`position_count()`** function is used to
pass any additional arguments to the function. One important additional
argument is the separator column (**`sep_col`**). The separator column
is a column in the **`table_doc`** data frame that contains a unique
identifier for each unit of text that should not be matched by the term
list.

In order to find the positions of the terms in the text,
**`position_count()`** matches the terms in the term list to the
standardized tokens in the document. It does this by using the
**`dplyr`** package to perform a left join operation on the term list
and the tokenized document data frames.

Here is an example of how to use **`position_count()`** to count the
occurrences of terms in a text corpus:

``` r
output <- rTermCount::position_count(
  .tls = termlist,
  .doc = document,
  sen_id
)

head(output)
#> # A tibble: 6 x 7
#>   doc_id                               tid ngram term    start  stop dup  
#>   <chr>                              <int> <int> <chr>   <int> <dbl> <lgl>
#> 1 the_importance_of_being_on_twitter     1     1 twitter     6     6 FALSE
#> 2 the_importance_of_being_on_twitter     3     1 london     11    11 FALSE
#> 3 the_importance_of_being_on_twitter     3     1 london     32    32 FALSE
#> 4 the_importance_of_being_on_twitter     1     1 twitter    37    37 FALSE
#> 5 the_importance_of_being_on_twitter     1     1 twitter   120   120 FALSE
#> 6 the_importance_of_being_on_twitter     1     1 twitter   131   131 FALSE
```

The output of **`position_count()`** is a data frame with one row for
each occurrence of each term in the document. The output contains the
following columns:

- **`doc_id`**: The document ID.

- **`tid`**: The term ID.

- **`term`**: The original term.

- **`start`**: The starting position of the term in the document.

- **`stop`**: The ending position of the term in the document.

- **`dup`**: A logical flag indicating whether the term is part of a
  higher n-gram.
