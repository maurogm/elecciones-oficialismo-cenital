#' ---
#' title: "Profit posterior"
#' author: maurogm
#' date: "`r Sys.Date()`"
#' output:
#'   github_document:
#'     toc: true
#' ---


#+ r knitr-global-options, include = FALSE
knitr::opts_chunk$set(warning = FALSE, message = FALSE, verbose = TRUE)
knitr::opts_knit$set(root.dir = normalizePath("../"))

# rmarkdown::render("src/exploracion_inicial.R", output_dir = "rendered_files", intermediates_dir = "rendered_files/intermediates")

#' # Setup

set.seed(999)
library(tidyverse)
library(data.table)
library(rethinking)

#' Carga de datos:
df <- readxl::read_xlsx("data/Base Elecciones Covid-19_2020-2021.xlsx",
    skip = 1,
    col_names = c(
        "pais",
        "fecha",
        "region",
        "victoria_oficialismo",
        "tipo_eleccion",
        "vdem_elect_index",
        "vdem_lib_index",
        "vdem_lib_comp",
        "idh",
        "fuente",
        "comentario"
    )
) %>%
    select(-c(comentario, fuente)) %>%
    mutate(victoria_oficialismo = victoria_oficialismo == "Ganó") %>%
    setDT()


#' # Exploración
df %>%
    ggplot() +
    aes(vdem_elect_index, vdem_lib_index, color = victoria_oficialismo) +
    geom_text(aes(label = pais), size = 3) +
    # facet_wrap(~region) +
    labs(
        title = "Victorias oficialistas según V-Dem indexes",
        x = "V-Dem Elect Dem Index",
        y = "V-Dem Lib Dem Index"
    ) +
    theme(legend.position = "bottom")

#' Ya que estamos, V-Dem indexes por continente:
df %>%
    ggplot() +
    aes(vdem_elect_index, vdem_lib_index, color = region) +
    geom_text(aes(label = pais), size = 3) +
    # facet_wrap(~region) +
    labs(
        title = "V-Dem indexes por continente",
        x = "V-Dem Elect Dem Index",
        y = "V-Dem Lib Dem Index"
    ) +
    theme(legend.position = "bottom")

#' # Modelado
#'
#' Entreno un modelo logístico de la probabilidad de victoria en función del
#' V-Dem Elect Dem Index:
modelo <- quap(
    alist(
        victoria_oficialismo ~ dbinom(1, p),
        logit(p) <- a + b * vdem_elect_index,
        a ~ dnorm(0, 1.5),
        b ~ dnorm(0, 10)
        # sigma ~ dunif(0, 50)
    ),
    data = df[!is.na(vdem_elect_index)]
)
#' La evidencia del efecto negativo del índice electoral sobre las victorias oficialistas
#' es bastante fuerte (al menos 94.5% de confianza en que `b` es negativo):
precis(modelo)

#' Densidad posterior de los coeficientes:
post <- extract.samples(modelo)
post %>%
    ggplot(aes(a, b)) +
    geom_density2d()

#' Sampleo algunos coeficientes de la distribución posterior, y grafico las curvas implicadas:
elect_index_seq <- seq(0, 1, 0.01)

logistic <- function(x) 1 / (1 + exp(-x))

a_mean <- precis(modelo)$mean[1]
b_mean <- precis(modelo)$mean[2]

n_samples <- 200
sample(1:nrow(post), size = n_samples) %>%
    post[., ] %>%
    mutate(sample_id = 1:n_samples) %>%
    crossing(data.table(vdem_elect_index = elect_index_seq)) %>%
    mutate(p_estimation = logistic(a + b * vdem_elect_index)) %>%
    ggplot() +
    geom_line(aes(vdem_elect_index, p_estimation, group = sample_id), alpha = 0.3) +
    geom_function(
        fun = function(x) logistic(a_mean + b_mean * x),
        color = "blue", size = 1.5
    ) +
    labs(
        title = "Posibles curvas muestreadas de la distribución posterior",
        subtitle = "En azul se resalta la curva más verosímil",
        x = "V-Dem Elect Dem Index",
        y = "Probabilidad de victoria del oficialismo"
    )
#' Se ve que la tendencia decreciente es clara, aunque todavía hay bastante incertidumbre
#' respecto a la forma de la curva.