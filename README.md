
[![Travis build status](https://travis-ci.org/jjesusfilho/stf.svg?branch=master)](https://travis-ci.org/jjesusfilho/stf) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jjesusfilho/stf?branch=master&svg=true)](https://ci.appveyor.com/project/jjesusfilho/stf) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

stf
===

The goal of stf is to retrieve and manipulate data from the Brazilian Supreme Court decisions. The package makes an effort to clean and tidy the data so you might get it almost ready for your analysis.

Functions are grouped in four categories:

1.  Download functions: They start with the verb download and create file or directories.
2.  Read functions: They read data from files downloaded by download functions.
3.  Get functions: They create R objects, by retrieving and parsing the data from the url connection. In other words, they skip the downloading step so you you are going to miss the files. But on the other hand, these functions are faster.
4.  View functions: They don't return any object or file, they just browse the help pages from STF gateway.
5.  Tidy functions. I am still building these functions. They will help you to clean and preprocess data for posterior analysis. I will only add fuctions that are stf specific, other tidy functions are available in the [JurisMiner](github.com/courtsbr/JurisMiner) or [abjutils](github.com/courtsbr/abjutils) packages.

Installation
------------

You can install stf from github with:

``` r
# install.packages("devtools")
devtools::install_github("jjesusfilho/stf")
```

You also have to make sure the packages [tesseract](https://github.com/ropensci/tesseract) and [pdftools](https://github.com/ropensci/pdftools) are installed as well as their dependencies.

You also have to download the `tesseract` trained data for Portuguese. You can find directions for Linux, Mac-OS and Windows [here](https://github.com/tesseract-ocr/tesseract/wiki)

Usage for STF
-------------

### Read metadata

Suppose you want to get the metadata from the Brazilian Supreme Court panel opinions with the expression "excesso de prazo". You can apply this function:

``` r
df<-get_stf_precedent_metadata(open_search="excesso de prazo")
```

Or simply:

``` r
df<-get_stf_precedent_metadata("excesso adj2 prazo")
```

By using "adj2" you are telling the search engine that "prazo" is one word apart from "excesso".

If you want to search for monocratic decisions, you can use another function:

``` r
df<-get_stf_mono_metadata("excesso adj2 prazo")
```

In order to find all the options, use the help function:

``` r
?get_stf_precedent_metadata()
```

Suppose now that you want to read all cases where "Telefônica" is a party. You can add the suffix ".PART." to the search:

``` r
telefonicaDF<-get_stf_precedent_metadata("telefonica.PART.")
```

If you want to see all the possible suffixes, the function `stf_help_view()` will load the help page on R viewer pane:

``` r
view_stf_help()
```

### Read the full opinion text (inteiro teor):

Once you have imported the metadata, you can use the same data frame to import the full opinion's text. Beware that opinions published before 2011 and even some of that year are in pdf image, not in text. Those opinions are downloaded, converted to `png`, and subsequently submmited to OCR in order to be read.

The limitation is that it takes a considerable amount of time to read the opinion's text. Without parallelization, one opinion can take up to 4 minutes to be read. As an example, 2000 opinions might take over five days to be read.

``` r
decisionTelefonica<-download_stf_dockets(telefonicaDF[1,]). 
# Downloads just the first decision from the dataset imported above.
```

When you have the docket number
-------------------------------

If you have the docket number, you can use it to download all information about that lawsuit:

``` r
download_stf_dockets(action="HC",docket_number = "4040")
```

From that, you have a bunch of functions to read all html files downloaded in nine folders corresponding to each tab of the docket page. Check the reference page to see all of them.

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
