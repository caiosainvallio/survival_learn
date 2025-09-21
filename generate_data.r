# carregar pacotes
library(dplyr)
library(lubridate)


set.seed(1234)

# número de observações
n <- 200

# gerando datas de início entre 1-jan-2015 e 31-dez-2019
dt_inicio <- sample(
    seq(as.Date("2018-01-01"), as.Date("2019-12-31"), by = "day"),
    size = n,
    replace = TRUE
)

# gera tempos até evento ou censura
# suponha distribuição de Weibull ou exponencial para tempo de evento
# primeiro gera tempo do evento
lambda <- 0.002 # taxa base para exponencial
tempo_evento <- rexp(n, rate = lambda)

# datas de seguimento máximo / fim do estudo
# suponha censura administrativa no dia 31-dez-2021
dt_fim_estudo <- as.Date("2020-01-31")

dt_evento <- dt_inicio + round(tempo_evento)

# monta status: evento só se dt_evento <= dt_fim_estudo, caso contrário censura
status <- ifelse(dt_evento <= dt_fim_estudo, 1, 0)

status |> table()

# ajusta dt_evento para NA onde censura
dt_evento2 <- ifelse(status == 1, dt_evento, NA)
dt_evento2 <- as.Date(dt_evento2, origin = "1970-01-01")

# gera mais covariáveis sintéticas
idade <- round(rnorm(n, mean = 60, sd = 10))
sexo <- sample(c("M", "F"), size = n, replace = TRUE)
tratamento <- sample(c("A", "B"), size = n, replace = TRUE)

# monta data frame
df_sint <- data.frame(
    id = 1:n,
    dt_inicio = dt_inicio,
    dt_evento = dt_evento2,
    status = status,
    idade = idade,
    sexo = sexo,
    tratamento = tratamento,
    stringsAsFactors = FALSE
) %>%
    mutate(
        # tempo de seguimento (dias)
        tempo = ifelse(
            status == 1,
            as.numeric(dt_evento - dt_inicio),
            as.numeric(dt_fim_estudo - dt_inicio)
        )
    )

# examinar
summary(df_sint$tempo)
table(df_sint$status)
head(df_sint)
write.csv(df_sint, "data/df_sint.csv", row.names = FALSE)
