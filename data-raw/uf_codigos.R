# data-raw/uf_codigos.R
# Gera data/uf_codigos.rda
# Execute: source("data-raw/uf_codigos.R")  (de dentro do diretório do pacote)

uf_codigos <- data.frame(
  sigla  = c("RO","AC","AM","RR","PA","AP","TO",
             "MA","PI","CE","RN","PB","PE","AL","SE","BA",
             "MG","ES","RJ","SP","PR","SC","RS","MS","MT","GO","DF"),
  codigo = c(11L,12L,13L,14L,15L,16L,17L,
             21L,22L,23L,24L,25L,26L,27L,28L,29L,
             31L,32L,33L,35L,41L,42L,43L,50L,51L,52L,53L),
  regiao = c(rep("Norte",7), rep("Nordeste",9),
             rep("Sudeste",4), rep("Sul",3), rep("Centro-Oeste",4)),
  stringsAsFactors = FALSE
)

# Salva diretamente sem depender de usethis
dir.create("data", showWarnings = FALSE)
save(uf_codigos, file = "data/uf_codigos.rda", compress = "bzip2")
message("✓ data/uf_codigos.rda gerado com ", nrow(uf_codigos), " UFs.")
