
-   [STF](#stf)
    -   [Overview](#overview)
    -   [Installation](#installation)
    -   [Usage for STF](#usage-for-stf)
        -   [Read metadata](#read-metadata)
        -   [Read the full opinion text (inteiro teor):](#read-the-full-opinion-text-inteiro-teor)
        -   [Use the docket number](#use-the-docket-number)
        -   [Work with the annual list of rulings](#work-with-the-annual-list-of-rulings)
        -   [Vocabulary correspondence](#vocabulary-correspondence)
    -   [Caveats](#caveats)

[![Travis build status](https://travis-ci.org/jjesusfilho/stf.svg?branch=master)](https://travis-ci.org/jjesusfilho/stf) [![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/jjesusfilho/stf?branch=master&svg=true)](https://ci.appveyor.com/project/jjesusfilho/stf) [![lifecycle](https://img.shields.io/badge/lifecycle-maturing-blue.svg)](https://www.tidyverse.org/lifecycle/#maturing)

STF
===

Overview
--------

The goal of stf is to retrieve and manipulate data from the Brazilian Supreme Court decisions. The package makes an effort to clean and tidy the data so you might get it almost ready for your analysis.

Functions are grouped in five categories:

1.  Download functions: They start with the verb download and create file or directories.
2.  Read functions: They read data from files downloaded by download functions.
3.  Get functions: They create R objects, by retrieving and parsing the data from the url connection. In other words, they just subsequently apply the corresponding download and read functions without storing any object to your disk.
4.  View functions: They don't return any object or file, they just browse the help pages from STF gateway.
5.  Tidy functions. I am still building these functions. They will help you cleaning and preprocessing data for posterior analysis. I will only add fuctions that are stf specific, other tidy functions are available in the [JurisMiner](github.com/courtsbr/JurisMiner) and the [abjutils](github.com/courtsbr/abjutils) packages.

Installation
------------

You can install stf from github with:

``` r
# install.packages("devtools")
devtools::install_github("jjesusfilho/stf")
```

You have to make sure the packages [tesseract](https://github.com/ropensci/tesseract) and [pdftools](https://github.com/ropensci/pdftools) are installed as well as their dependencies.

You also have to download the `tesseract` trained data for Portuguese. You can find directions for Linux, Mac-OS and Windows [here](https://github.com/tesseract-ocr/tesseract/wiki)

Usage for STF
-------------

### Read metadata

Suppose you want to get the metadata from the Brazilian Supreme Court panel opinions with the expression "excesso de prazo". You can execute the following function:

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

Suppose now that you want to read all cases where "Telefônica" is a party. You can add the suffix ".PART." to the search sentence:

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

### Use the docket number

If you have the docket number, you can use it to download all information about that lawsuit:

``` r
download_stf_dockets(action="HC",docket_number = "4040")
```

### Work with the annual list of rulings

Another way to work with decisions ruled by the Supreme Court is accessing their spreadsheet of annual rulings. The spreadsheets are supposed to have all cases ruled in a year and give valuable information about the cases. You can check all functions at the reference tab, but pretty soon I will add an vignette showing how to go through a case.

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
| Andamento            | Docket sheet          |
| Classe               | Petition type         |
| Prover/conceder      | Reverse               |
| Desprover/denegar    | Affirm                |
| Anular decisão       | Remand                |
| Origem               | Original jurisdiction |
| Data da distribuição | Argued date           |
| Data do julgamento   | Decision's date       |

Caveats
-------

The STF precedents' (jurisprudence) search service is flawed by poor design and inconsistent data. To number a few:

1.  Brazilian Supreme Court offers no Web API, which leads us to creating webscrapers to download public data that should be available to researchers through open data protocols and endpoints;

2.  The classes of the parties in a lawsuit are very messy. The way justices, or their advisors, write the parties' information is transfered to the database without any normalization. To give an example, the reference to the attorney can be written in many forms: advogada, adv, advda, advdo, proc etc. The authors of this package have put some effort to fix that, but the creativity of the the justices and their advisors goes beyond our capacity to figure everythig out.

3.  Contrary to other courts, such as TJSP and STJ, the search for decisions does not return all cases. If you don't have the docket number of every lawsuit about the matter or precedural class, you simply can't access everything that was decided.

4.  Initially we thought that the spreadsheets listing all ruled cases of the past years were accurate, but after downloading them in different times, we have noticed that the number of cases can increase or decrease depending on the moment we downloaded. As the STF IT people don't make any clarification about these changes, there is no way to guess how many cases were actually ruled during a certain year.

5.  Inconsistencies in the database are rife. Decisions without the full opinion's text, dates of publication with no reference to the decisions' dates and signatures' dates. Decisons of one justice are assigned to another justice etc.
