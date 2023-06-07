
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

### Preparing the Term List

The term list is a two-column data frame containing the terms to search
for and their corresponding IDs (An additional indicator column
`part_of_ngram` is included that indicates whether a term is part of a
higher n-gram.

<table class=" lightable-paper table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto; font-size: 10px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
tid
</th>
<th style="text-align:left;">
term
</th>
<th style="text-align:left;">
part_of_ngram
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
Animal welfare
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
Biodiversity
</td>
<td style="text-align:left;">
3
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
Biodiversity conservation programs
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
Carbon footprint
</td>
<td style="text-align:left;">
5
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
Carbon footprint reduction strategies
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
Carbon neutrality and offsets
</td>
<td style="text-align:left;">
</td>
</tr>
</tbody>
</table>

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
```

<table class=" lightable-paper table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto; font-size: 10px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:right;">
tid
</th>
<th style="text-align:right;">
ngram
</th>
<th style="text-align:left;">
term
</th>
<th style="text-align:left;">
term_orig
</th>
<th style="text-align:left;">
part_of_ngram
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
animal welfare
</td>
<td style="text-align:left;">
Animal welfare
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
biodiversity
</td>
<td style="text-align:left;">
Biodiversity
</td>
<td style="text-align:left;">
3
</td>
</tr>
<tr>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
biodiversity conservation programs
</td>
<td style="text-align:left;">
Biodiversity conservation programs
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
carbon footprint
</td>
<td style="text-align:left;">
Carbon footprint
</td>
<td style="text-align:left;">
5
</td>
</tr>
<tr>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
carbon footprint reduction strategies
</td>
<td style="text-align:left;">
Carbon footprint reduction strategies
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
carbon neutrality and offsets
</td>
<td style="text-align:left;">
Carbon neutrality and offsets
</td>
<td style="text-align:left;">
</td>
</tr>
</tbody>
</table>

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
```

<table class=" lightable-paper table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto; font-size: 10px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
doc_id
</th>
<th style="text-align:right;">
pag_id
</th>
<th style="text-align:right;">
par_id
</th>
<th style="text-align:right;">
sen_id
</th>
<th style="text-align:right;">
tok_id
</th>
<th style="text-align:left;">
token
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
basf
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
report
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
2021
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
management
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
s
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
report
</td>
</tr>
</tbody>
</table>

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
```

<table class=" lightable-paper table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto; font-size: 10px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
doc_id
</th>
<th style="text-align:right;">
tid
</th>
<th style="text-align:right;">
ngram
</th>
<th style="text-align:left;">
term
</th>
<th style="text-align:right;">
start
</th>
<th style="text-align:right;">
stop
</th>
<th style="text-align:left;">
dup
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
sustainability
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
sustainability
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
sustainability
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
136
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
sustainable development goals sdgs
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
109
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
175
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate
</td>
<td style="text-align:right;">
145
</td>
<td style="text-align:right;">
145
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
renewable energy
</td>
<td style="text-align:right;">
169
</td>
<td style="text-align:right;">
170
</td>
<td style="text-align:left;">
FALSE
</td>
</tr>
</tbody>
</table>

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
```

<table class=" lightable-paper table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto; font-size: 10px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
doc_id
</th>
<th style="text-align:right;">
tid
</th>
<th style="text-align:right;">
ngram
</th>
<th style="text-align:left;">
term
</th>
<th style="text-align:right;">
n_dup
</th>
<th style="text-align:right;">
n_uni
</th>
<th style="text-align:left;">
part_of_ngram
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
sustainability
</td>
<td style="text-align:right;">
87
</td>
<td style="text-align:right;">
87
</td>
<td style="text-align:left;">
125, 128
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
174
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
carbon
</td>
<td style="text-align:right;">
64
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:left;">
4, 5, 6, 7, 8, 82, 83, 84
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
175
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
climate
</td>
<td style="text-align:right;">
57
</td>
<td style="text-align:right;">
48
</td>
<td style="text-align:left;">
14, 15, 16, 17, 18, 19, 20, 62
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
biodiversity
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:right;">
53
</td>
<td style="text-align:left;">
3
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
73
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
human rights
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
43
</td>
<td style="text-align:left;">
74
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
176
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
greenhouse gas
</td>
<td style="text-align:right;">
45
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
69, 70
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
69
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
greenhouse gas emissions
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:right;">
32
</td>
<td style="text-align:left;">
70
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
carbon footprint
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
5
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
46
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
environmental protection
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:right;">
19
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
circular economy
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:right;">
18
</td>
<td style="text-align:left;">
10
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
171
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
water management
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:right;">
12
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
166
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
transparency
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:right;">
11
</td>
<td style="text-align:left;">
126
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
renewable energy
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:right;">
10
</td>
<td style="text-align:left;">
24, 90, 91, 92
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
15
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
climate change
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:right;">
9
</td>
<td style="text-align:left;">
16, 17, 62
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
34
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
energy efficiency
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:right;">
6
</td>
<td style="text-align:left;">
35
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
93
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
resource efficiency
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
94
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
124
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
supply chain management
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:right;">
5
</td>
<td style="text-align:left;">
68
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
25
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
conflict minerals
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
74
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
human rights due diligence
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
115
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
social responsibility
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
28, 29
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
136
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
sustainable development goals sdgs
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
173
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
water stewardship
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:right;">
3
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
31
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
eco efficiency
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
80
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
labor standards
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
95
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
resource scarcity
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
123
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
stakeholder engagement
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
150
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
sustainable procurement
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
152
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
sustainable production
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
167
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
waste management
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
163
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
21
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
community development
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
112
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
social inclusion
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
130
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
sustainable agriculture
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
131
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
139
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
sustainable finance
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
140
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
172
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
water scarcity
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
</td>
</tr>
</tbody>
</table>

The output of `summarize_count()` is a data frame that summarizes the
count of the terms on a document level either **excluding** terms that
are part of a higher N-gram (column: `n_uni`) or **including** terms
that are part of a higher N-gram (column: `n_dup`). The output contains
the following columns (the column `part_of_ngram` is added to the output
in order to show case the deduplicated count):

- **`doc_id`**: The document ID.

- **`tid`**: The term ID.

- **`ngram`**: The ngram of the term.

- **`term`**: The original term.

- **`n_uni`**: Count of the term, excluding N-grams that are part of a
  higher N-gram

- **`n_dup`**: Count of the term, including N-grams that are part of a
  higher N-gram

Looking at the example above, we can see that the term **carbon (tid:
174)** is part of several other terms within the termlist (see column:
`part_of_ngram` which shows the term identifiers (tid) that are higher
ngrams including this term). Specifically, carbon is part of the term
**carbon footprint (tid: 5).** Both terms appear in the text corpus, so
when we count both terms individually we get “raw” count (column:
`n_dup`) of **64** for **carbon** and **19** for **carbon footprint.**
Not adjusting for the fact that the term **carbon** is completely
included in the term **carbon footprint**, would lead to a double
counting of **carbon.** Therefore the column `n_uni` adjust the “raw”
count to a “unique” count, by subtracting the occurrences of **carbon
footprint** from the occurrences of **carbon.** This leads to a unique
count of **carbon** of **45 (64 - 19).**

(Note: In the example above only **carbon footprint** as a higher ngram
was found in the corpus. In case more higher ngram were contained in the
corpus, the function would have automatically adjust for all
occurrences)

# Why this Package?

Counting terms in documents is generally not a hard task. Nonetheless,
using simple look-ups or Regular Expressions have their drawbacks. For
example using regular expressions can take a very long time and we don’t
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
) 
```

<table class=" lightable-paper table" style="font-family: &quot;Arial Narrow&quot;, arial, helvetica, sans-serif; margin-left: auto; margin-right: auto; font-size: 10px; margin-left: auto; margin-right: auto;">
<thead>
<tr>
<th style="text-align:left;">
doc_id
</th>
<th style="text-align:right;">
tid
</th>
<th style="text-align:right;">
ngram
</th>
<th style="text-align:left;">
dup
</th>
<th style="text-align:right;">
start
</th>
<th style="text-align:right;">
stop
</th>
<th style="text-align:left;">
pre
</th>
<th style="text-align:left;">
term
</th>
<th style="text-align:left;">
post
</th>
<th style="text-align:left;">
context
</th>
<th style="text-align:right;">
n
</th>
</tr>
</thead>
<tbody>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:right;">
7
</td>
<td style="text-align:left;">
2021 management s report
</td>
<td style="text-align:left;">
sustainability
</td>
<td style="text-align:left;">
along the value chain
</td>
<td style="text-align:left;">
word
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:right;">
13
</td>
<td style="text-align:left;">
the value chain 96 —
</td>
<td style="text-align:left;">
sustainability
</td>
<td style="text-align:left;">
along the value chain
</td>
<td style="text-align:left;">
word
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
127
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:right;">
40
</td>
<td style="text-align:left;">
the three pillars of
</td>
<td style="text-align:left;">
sustainability
</td>
<td style="text-align:left;">
are firmly anchored in
</td>
<td style="text-align:left;">
word
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
136
</td>
<td style="text-align:right;">
4
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
106
</td>
<td style="text-align:right;">
109
</td>
<td style="text-align:left;">
them with respect nations
</td>
<td style="text-align:left;">
sustainable development goals sdgs
</td>
<td style="text-align:left;">
in many ways see
</td>
<td style="text-align:left;">
word
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
175
</td>
<td style="text-align:right;">
1
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
145
</td>
<td style="text-align:right;">
145
</td>
<td style="text-align:left;">
demand for food enable
</td>
<td style="text-align:left;">
climate
</td>
<td style="text-align:left;">
smart mobility reduce emissions
</td>
<td style="text-align:left;">
word
</td>
<td style="text-align:right;">
4
</td>
</tr>
<tr>
<td style="text-align:left;">
BASF ESG
</td>
<td style="text-align:right;">
89
</td>
<td style="text-align:right;">
2
</td>
<td style="text-align:left;">
FALSE
</td>
<td style="text-align:right;">
169
</td>
<td style="text-align:right;">
170
</td>
<td style="text-align:left;">
increase the capabilities of
</td>
<td style="text-align:left;">
renewable energy
</td>
<td style="text-align:left;">
alongside these positive contributions
</td>
<td style="text-align:left;">
word
</td>
<td style="text-align:right;">
4
</td>
</tr>
</tbody>
</table>
