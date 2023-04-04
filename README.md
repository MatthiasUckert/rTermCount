
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
#>   tid                                  term source
#> 1   1                        Animal welfare  GPT 4
#> 2   2                          Biodiversity  GPT 4
#> 3   3    Biodiversity conservation programs  GPT 4
#> 4   4                      Carbon footprint  GPT 4
#> 5   5 Carbon footprint reduction strategies  GPT 4
#> 6   6         Carbon neutrality and offsets  GPT 4
```

The example text corpus is a data frame containing text data that can be
used for testing. To access the example text corpus, simply call
**`table_doc`** in R.

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
#>   tid                                  term source
#> 1   1                        Animal welfare  GPT 4
#> 2   2                          Biodiversity  GPT 4
#> 3   3    Biodiversity conservation programs  GPT 4
#> 4   4                      Carbon footprint  GPT 4
#> 5   5 Carbon footprint reduction strategies  GPT 4
#> 6   6         Carbon neutrality and offsets  GPT 4
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
#>     tid ngram term                                  term_orig             source
#>   <dbl> <int> <chr>                                 <chr>                 <chr> 
#> 1     1     2 animal welfare                        Animal welfare        GPT 4 
#> 2     2     1 biodiversity                          Biodiversity          GPT 4 
#> 3     3     3 biodiversity conservation programs    Biodiversity conserv~ GPT 4 
#> 4     4     2 carbon footprint                      Carbon footprint      GPT 4 
#> 5     5     4 carbon footprint reduction strategies Carbon footprint red~ GPT 4 
#> 6     6     4 carbon neutrality and offsets         Carbon neutrality an~ GPT 4
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
document <- prep_document(
  .tab = table_doc,
  .fun_std = std_str,
  .until = "tok"
)

head(document)
#> # A tibble: 6 x 6
#>   doc_id   pag_id par_id sen_id tok_id token     
#>   <chr>     <int>  <int>  <int>  <int> <chr>     
#> 1 BASF ESG      1      1      1      1 basf      
#> 2 BASF ESG      1      1      1      2 report    
#> 3 BASF ESG      1      1      1      3 2021      
#> 4 BASF ESG      1      1      1      4 management
#> 5 BASF ESG      1      1      1      5 s         
#> 6 BASF ESG      1      1      1      6 report
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
output_pos <- position_count(
  .tls = termlist,
  .doc = document,
  sen_id
)

head(output_pos)
#> # A tibble: 6 x 7
#>   doc_id     tid ngram term                               start  stop dup  
#>   <chr>    <dbl> <int> <chr>                              <int> <dbl> <lgl>
#> 1 BASF ESG   127     1 sustainability                         7     7 FALSE
#> 2 BASF ESG   127     1 sustainability                        13    13 FALSE
#> 3 BASF ESG   127     1 sustainability                        40    40 FALSE
#> 4 BASF ESG   136     4 sustainable development goals sdgs   106   109 FALSE
#> 5 BASF ESG    89     2 renewable energy                     169   170 FALSE
#> 6 BASF ESG    73     2 human rights                         248   249 FALSE
```

The output of **`position_count()`** is a data frame with one row for
each occurrence of each term in the document. The output contains the
following columns:

- **`doc_id`**: The document ID.

- **`tid`**: The term ID.

- **`ngram`**: The ngram of the term.

- **`term`**: The original term.

- **`start`**: The starting position of the term in the document.

- **`stop`**: The ending position of the term in the document.

- **`dup`**: A logical flag indicating whether the term is part of a
  higher n-gram.

### Summarizing the Term Count

Last, we use the function summarize_count() to retrieve the summary
statistics of the position_count() function in the last step. This
function takes the followng inputs:

- **`.tab`**: The position count dataframe prepared by
  **`position_count()`**.

``` r
output_sum <- summarize_count(output_pos)
head(output_sum)
#> # A tibble: 6 x 6
#>   doc_id     tid ngram term                  n_dup n_uni
#>   <chr>    <dbl> <int> <chr>                 <int> <int>
#> 1 BASF ESG     2     1 biodiversity             53    53
#> 2 BASF ESG     4     2 carbon footprint         19    19
#> 3 BASF ESG     9     2 circular economy         18    18
#> 4 BASF ESG    15     2 climate change            9     9
#> 5 BASF ESG    21     2 community development     1     1
#> 6 BASF ESG    25     2 conflict minerals         3     3
```

The output of summarize_count**`()`** is a data frame that summarizes
the count of the terms on a document level either **excluding** terms
that are part of a higher N-gram (column: n_uni) or **including** terms
that are part of a higher N-gram (column: n_dup). The output contains
the following columns:

- **`doc_id`**: The document ID.

- **`tid`**: The term ID.

- **`ngram`**: The ngram of the term.

- **`term`**: The original term.

- **`n_uni`**: Count of the term, excluding N-grams that are part of a
  higher N-gram

- **`n_dup`**: Count of the term, including N-grams that are part of a
  higher N-gram

# Why this Package?

Counting terms in documents is perse an easy task. Nonetheless, using
simple look-ups or Regular Expressions have their drawbacks. For example
using regular expressions can take a very long time and we don’t
retrieve the exact location of a term. So the purpose of this package is
to:

1.  Provide a standardized input and output of term search

2.  Have a optimized pipeline for term list with thousands of terms

3.  Retrieve the exact location of a term within the document

4.  Deduplicate for terms that are part of a higher ngram

Having the exact location also enables us to retrieve the context of a
term

``` r
context <- get_context(
  .pos = output_pos,
  .doc = document, 
  .context = "word",
  .n = 4
) %>%
  dplyr::select(pre, term, post)

head(context)
#> # A tibble: 6 x 3
#>   pre                          term                               post          
#>   <chr>                        <chr>                              <chr>         
#> 1 2021 management s report     sustainability                     along the val~
#> 2 the value chain 96 ---       sustainability                     along the val~
#> 3 the three pillars of         sustainability                     are firmly an~
#> 4 them with respect nations    sustainable development goals sdgs in many ways ~
#> 5 increase the capabilities of renewable energy                   alongside the~
#> 6 a potential risk of          human rights                       violations --~
```
