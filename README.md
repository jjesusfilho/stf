
<!-- badges: start -->

[![R-CMD-check](https://github.com/jjesusfilho/stf/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/jjesusfilho/stf/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# STF

## Objetivo

Inicialmente, as funções e o manual estavam em inglês. No entanto,
decidi passar o manual para português. As funções permanecem em inglês, por
ora. Este pacote contêm funções tanto para baixar a jurisprudência por
termos, quanto possibilita baixar toda a base do STF.

## Instalação

Para instalar o pacote stf, primeiramente instale o pacote remotes.
Depois use o remotes para instalar o pacote stf:

``` r
install.packages("remotes")
remotes::install_github("jjesusfilho/stf")
```

## Como usar

Para baixar acórdãos e decisões monocráticas com termos de busca, use a
função `stf_download_cjsg`. Se, por exemplo, você quiser baixar decisões
com os termos “mulher, violência e gênero”, proceda da seguinte forma:

``` r
library(stf)

dir.create("juris_stf")

stf_download_cjsg("mulher AND violência AND gênero", 
                  base = "acordaos",
                  size = 20, ## baixará 20 acórdãos por vez. 
                  dir = "juris_stf")
```

Eu usei size = 20, mas você pode aumentar o número de decisões por
arquivo para até 200.

Depois disso, você pode ler esses acórdãos com a função `stf_read_cjsg`:

``` r
df <- stf_read_cjsg(dir = "juris_stf")
```
