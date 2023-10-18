# Projeto Mais Fortaleza

<img src="./www/presentation.gif" alt="My project">

Descrição do projeto em um parágrafo, das principais funcionalidades.

O projeto pode ser visto em https://observatoriodefortaleza.fortaleza.ce.gov.br/dados/painel/obsr/


## Sobre

- Descrição mais detalhada (com bullets das principais features)
- O porque da existência do projeto.
- Pode incluir aqui também uma lista de features ainda não implementadas / próximos passos.


## Tecnologias

Algumas das principais tecnologias e frameworks utilizados no projeto.<br/><br/>

<div>
    <img src="https://img.shields.io/badge/R-276DC3?style=for-the-badge&logo=r&logoColor=white"
    height="22px" />
    <img src="https://img.shields.io/badge/RStudio-75AADB?style=for-the-badge&logo=RStudio&logoColor=white" height="22px" />
    <img src="https://img.shields.io/badge/Shiny-shinyapps.io-blue?style=flat&labelColor=white&logo=RStudio&logoColor=blue" height="22px" />
    <img src="https://img.shields.io/badge/Bootstrap-563D7C?style=for-the-badge&logo=bootstrap&logoColor=white" height="22px" />
    <img src="https://img.shields.io/badge/Leaflet-199900?style=for-the-badge&logo=Leaflet&logoColor=white" height="22px" />
    <img src="https://img.shields.io/badge/PostgreSQL-316192?style=for-the-badge&logo=postgresql&logoColor=white" height="22px" />
    <img src="https://img.shields.io/badge/Docker-2CA5E0?style=for-the-badge&logo=docker&logoColor=white" height="22px" />
    <img src="https://img.shields.io/badge/Figma-F24E1E?style=for-the-badge&logo=figma&logoColor=white" height="22px" />
</div>

## Como rodar o projeto

### Banco de dados

1. Crie um novo banco de dados no postgres com o nome obsr

```bash
CREATE DATABASE obsr
```

2. No terminal, faça o dump do banco de dados que se encontra em [`\data`](https://gitlab.com/DIOBS/obsr/dashboard/-/tree/main/data)

```bash
psql obsr < obsr.sql
```

### Projeto 

1. Crie um arquivo `.Renviron` com as credenciais do banco de dados usando o modelo do arquivo `.Renviron.example`

2. Abra o arquivo app.R no RStudio

3. No console, instale o pacote renv

```bash
install.packages("renv")
```

4. Inicialize o projeto

```bash
renv::init()
```

5. Instale todas as dependências

```bash
renv::install()
```

5. Crie um arquivo lockfile com o estado atual do projeto

```bash
renv::snapshot()
```

6. Execute o projeto

