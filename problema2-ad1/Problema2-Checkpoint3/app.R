#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(dplyr)
library(ggplot2)
library(RCurl)

dados = read.csv("ano-atual.csv", encoding = "UTF-8")

#Helper methods and datasets

dados_compactos = dados %>%
  select(nuDeputadoId,
         txNomeParlamentar,
         sgPartido,
         sgUF,
         txtDescricao,
         txtNumero,
         txtDescricaoEspecificacao,
         indTipoDocumento,
         vlrLiquido,
         numMes)

dados_compactos$txtDescricao = as.factor(dados_compactos$txtDescricao)
dados_compactos$txtDescricaoEspecificacao = as.factor(dados_compactos$txtDescricaoEspecificacao)

# deputados_por_estado = dados_compactos %>%
#   group_by(sgUF) %>%
#   na.omit() %>%
#   summarise(total = n()) %>%
#   arrange(total)

gastos_medios_estado = dados_compactos %>%
  filter(!is.na(sgUF)) %>%
  group_by(txNomeParlamentar, txtDescricao) %>%
  summarise(total = sum(vlrLiquido)) %>%
  left_join(dados_compactos, by=c("txNomeParlamentar", "txtDescricao")) %>%
  ungroup() %>%
  group_by(sgUF, txtDescricao) %>%
  summarise(mediana = median(total))

# gastos_medios_estado = gastos_despesas_estados %>%
#   rename(total_custo = total) %>%
#   left_join(deputados_por_estado, by = "sgUF") %>%
#   rename(total_deputados = total) %>%
#   filter(sgUF %in% c("PB", "RN", "PE", "CE")) %>%
#   mutate(gastos_medios = total_custo / total_deputados) %>%
#   select(-c(total_custo, total_deputados))

plot_dados = gastos_medios_estado %>%
  filter(sgUF %in% c("PB", "RN", "PE", "CE")) %>%
  ggplot(aes(x=txtDescricao, y=mediana, color=sgUF)) +
  geom_point(stat="identity") +
  geom_line(aes(group=sgUF))+
  labs(title="Comparando gastos de diferentes estados", x="Categoria", y="Mediana dos gastos") + 
  scale_color_discrete(name="Estado")
  

# Define UI for application that draws a histogram
ui <- shinyUI(fluidPage(
  fluidRow(
    column(10, offset="1",
    h1("Como gastam os deputados da Paraíba?", align="center"),
    h2("Comparando os gastos de nossos deputados com os gastos dos deputados de estados vizinhos", align="center"),
    h4(align="right", "Por Júlio Cesar Neves"),
    br(),
    p("A cota parlamentar é um repasse dado aos parlamentares brasileiros para 
      que eles possam custear seus gastos com atividades parlamentares. Instituída em 2009, o valor da cota disponível para
      cada parlamentar é proporcional à distância em que a capital do estado dele encontra-se da capital federal, e pode ser utilizado,
      entre outras coisas, para pagamentos de diárias em hotéis, divulgação de atividades parlamentares, fretamento de veículos e despesas
      com combustíveis", align="justify"),
    p("Visando analisar melhor a aplicação da cota pelos deputados paraibanos, dados relativos a eles foram comparados a dados
      dos estados vizinhos de Ceará, Pernambuco e Rio Grande do Norte. Para isso, foi calculada a mediana dos gastos destes deputados
      em cada uma das categorias de despesas. O valor da mediana indica que 50% dos deputados gastaram menos que o valor dado e
      outros 50% gastaram mais que o valor e foi escolhido como medida de comparação por ser menos suscetível a valores extremos", align="justify"),
    p("No gráfico interativo abaixo, é possível comparar os valores gastos por nossos deputados. Selecione uma área para restringir
      as atividades que devem ser comparadas ou selecione os estados que devem ser comparados para obter uma comparação mais especializada", align="justify")
  )),
  
  fluidRow(
    column(
      width=6, class="well", offset = 1,
      plotOutput("plot", height = 300,
                 brush = brushOpts(
                   id = "plot_brush",
                   resetOnNew = TRUE
                ))
    ),
    
    column(
      width=3, class="well", offset = 1,
      checkboxGroupInput("estados",
        label = "Selecione os estados que deseja ver",
        choices = list("PB" = "PB", "PE" = "PE", "CE" = "CE", "RN" = "RN"),
        selected = c("PB", "PE", "CE", "RN"))
    )), 
  
  br(), br(), br(),
    
  fluidRow(
    column(
        width=10, class="well", offset = 1,
        span(
          plotOutput("plot2", height = 300)
        )
      )
    ),
  
  fluidRow(
    column(
      10, offset = "1",
      h3("Sobre holofotes e aviões..."),
      p(align="justify", "Comparados aos parlamentares de estados vizinhos, os deputados paraibanos são os que
        que mais gastam dinheiro em atividades de divulgação, gastando quase 50% a mais que os 
        deputados do estado do Ceará, por exemplo. Os deputados paraibanos parecem amar os holofotes."),
      p(align="justify", "Outro ponto que chama a atenção no estudo é o fato de que, no estado da Paraíba, não foi apontada nenhuma
        utilização da cota parlamentar para a locação ou fretamento de aeronaves durante
        o período analisado. Isso é muito peculiar e ao meu ver pode indicar uma questão digna de
        análise mais aprofundada: Seria essa observação decorrente de um erro na coleta dos dados ou os deputados
        paraibanos realmente não costumam fretar aeronaves, como fazem os representantes dos demais estados?
        Estaria acontecendo algo estranho no vasto céu da Paraíba?"),
      br(), br()
    )
  )
  )
)

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  # Linked plots (middle and right)
  ranges <- reactiveValues(x = NULL, y = NULL)
  gastos = reactiveValues(estados = NULL)
  
  output$plot <- renderPlot({plot_dados + coord_flip()})
  
  output$plot2 <- renderPlot({
    plot_dados + coord_flip(ylim = ranges$x, xlim=ranges$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot_brush
    if (!is.null(brush)) {
      ranges$x <- c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  observe({
    estados = input$estados
    
    gastos_medios_estado = gastos_medios_estado %>% filter(sgUF %in% estados)
    
    
    plot_dados = gastos_medios_estado %>%
      ggplot(aes(x=txtDescricao, y=mediana, color=sgUF)) +
      geom_point(stat="identity") +
      geom_line(aes(group=sgUF)) +
      labs(title="Comparando gastos de diferentes estados", x="Categoria", y="Mediana dos gastos") + 
      scale_color_discrete(name="Estado")
    
    output$plot2 <- renderPlot({
      plot_dados + coord_flip(xlim=ranges$y)
    })
  })

})

# Run the application 
shinyApp(ui = ui, server = server)

