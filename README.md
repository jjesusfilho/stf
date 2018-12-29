
[![Travis build status](https://travis-ci.org/jjesusfilho/stf.svg?branch=master)](https://travis-ci.org/jjesusfilho/stf) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jjesusfilho/stf?branch=master&svg=true)](https://ci.appveyor.com/project/jjesusfilho/stf) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

stf
===

The goal of stf is to retrieve and manipulate data from the Brazilian Supreme Court decisions. The package makes an effort to clean and tidy the data so you might get it almost ready for your analysis.

Installation
------------

You can install stf from github with:

``` r
# install.packages("devtools")
devtools::install_github("courtsbr/stf")
```

You also have to make sure the packages [tesseract](https://github.com/ropensci/tesseract) and [pdftools](https://github.com/ropensci/pdftools) are installed as well as their dependencies.

You also have to download the `tesseract` trained data for Portuguese. You can find directions for Linux, Mac-OS and Windows [here](https://github.com/tesseract-ocr/tesseract/wiki)

Usage for STF
-------------

### Read metadata

Suppose you want to download the metadata from the Brazilian Supreme Court panel opinions with the expression "excesso de prazo". You can run this function:

``` r
df<-stf_opinion_metadata(open_search="excesso de prazo")
```

Or simply:

``` r
df<-stf_opinion_metadata("excesso adj2 prazo")
```

By using "adj2" you are telling the search engine that "prazo" is one word apart from "excesso".

If you want to search for monocratic decisions, you can use another functio:

``` r
df<-stf__mono_metadata("excesso adj2 prazo")
```

In order to find all the options, use the help function:

``` r
?stf_opinion_metadata()
```

Suppose now that you want to read all cases where "Telefônica" is a party. You can add the suffix ".PART." to the search:

``` r
telefonicaDF<-stf_opinion_metadata("telefonica.PART.")
```

If you want to see all the possible suffixes, the function `stf_help_view()` will load the help page on the Rstudio viewer pane:

``` r
stf_help_view()
```

### Download whole opinion text (inteiro teor):

Once you have imported the metadata, you can use the same data frame to import the whole decision. Beware that decisions published before 2011 and even some of that year are in pdf image, not text. Those decisions are converted to `png` and submmited to OCR in order to be read. The limitation is that it might take a long time to read all opinions.

### Vocabulary correspondence

The table below shows a rough translation of the Brazilian Supreme Court's opinion's elements to US English:

| Portuguese           | English               |
|----------------------|-----------------------|
| Acórdão              | Opinion               |
| Ementa               | Syllabus              |
| Relator              | Reporter              |
| Ministro             | Justice               |
| Órgão julgador       | Judicial panel        |
| Decisão              | Decision              |
| Processo             | Docket number         |
| Parte                | Party                 |
| Acompanhamento       | Docket sheet          |
| Classe               | Petition type         |
| Prover/conceder      | Reverse               |
| Desprover/denegar    | Affirm                |
| Anular decisão       | Remand                |
| Origem               | Original jurisdiction |
| Data da distribuição | Argued date           |
| Data do julgamento   | Decision's date       |

### Read the full opinion text (inteiro teor):

Once you have imported the metadata, you can use the same data frame to import the full opinion's text. Beware that opinions published before 2011 and even some of that year are in pdf image, not in text. Those opinions are downloaded, converted to `png`, and subsequently submmited to OCR in order to be read.

The limitation is that it takes a considerable amount of time to read the opinion's text. Without parallelization, one opinion can take up to 4 minutes to be read. As an example, 2000 opinions might take over five days to be read.

``` r
decisionTelefonica<-stf_opinion(telefonicaDF[1,]). 
# Downloads just the first decision from the dataset imported above.
```
