library(shiny)
library(tidyverse)

## calculate d1 for the price now, t=0:
calculate_d1 = function(S, E, r, bigT, sigma) {
  d1 = (log(S/E)+(r+(sigma^2)/2)*bigT)/(sigma*sqrt(bigT))
  return(d1)
}

## calculate d2 for the price now, t=0:
calculate_d2 = function(S, E, r, bigT, sigma) {
  d2 = (log(S/E)+(r-(sigma^2)/2)*bigT)/(sigma*sqrt(bigT))
  return(d2)
}

## calculate Call Option price now, t=0:
call_price = function(S, E, r, bigT, sigma) {
  C_E = (S*pnorm(calculate_d1(S, E, r, bigT, sigma))) - 
    (E*exp(-r*bigT)*pnorm(calculate_d2(S, E, r, bigT, sigma)))
  return(C_E)
}

## calculate Put Option price now, t=0:
put_price = function(S, E, r, bigT, sigma) {
  P_E = (E*exp(-r*bigT)*pnorm(-1*calculate_d2(S, E, r, bigT, sigma))) - 
    (S*pnorm(-1*calculate_d1(S, E, r, bigT, sigma)))
  return(P_E)
}

## Calculate the Greek Hedges:

## Delta - Change in Options as S changes
## for calls:
call_delta_hedge = function(S, E, r, bigT, sigma) {
  call_delta = pnorm(calculate_d1(S, E, r, bigT, sigma))
  return(call_delta)
}
## for puts:
put_delta_hedge = function(S, E, r, bigT, sigma) {
  put_delta = -1*pnorm(-1*calculate_d1(S, E, r, bigT, sigma))
  return(put_delta)
}

## Gamma - how does the delta hedge change as S changes? - same for calls and puts
gamma_hedge = function(S, E, r, bigT, sigma) {
  gamma_hedge = (1/(S*sigma*sqrt(2*pi*bigT)))*(exp(-1*(calculate_d1(S, E, r, bigT, sigma)^2)/2))
  return(gamma_hedge)
}

## Theta - Change in Options as TIME changes
## for calls:
call_theta_hedge = function(S, E, r, bigT, sigma) {
  call_theta = -1*((-1*(S*sigma*exp(-1*(calculate_d1(S, E, r, bigT, sigma)^2)/2))/(2*sqrt(2*pi)*sqrt(bigT))) - 
                     (r*E*exp(-1*r*bigT)*pnorm(calculate_d2(S, E, r, bigT, sigma))))
  return(call_theta)
}
## for puts:
put_theta_hedge = function(S, E, r, bigT, sigma) {
  put_theta = -1*((-1*(S*sigma*exp(-1*(calculate_d1(S, E, r, bigT, sigma)^2)/2))/(2*sqrt(2*pi)*sqrt(bigT))) + 
                    (r*E*exp(-1*r*bigT)*pnorm(-1*calculate_d2(S, E, r, bigT, sigma))))
  return(put_theta)
}

## Vega - Change in Options as VOLATILITY changes - same for calls and puts
vega_hedge = function(S, E, r, bigT, sigma) {
  vega_hedge = S*sqrt(bigT/2/pi)*exp((-1*(calculate_d1(S, E, r, bigT, sigma)^2)/2))
  return(vega_hedge)
}

## Rho - Change in Options as INTEREST RATE changes
## for calls:
call_rho_hedge = function(S, E, r, bigT, sigma) {
  call_rho = bigT*E*exp(-1*r*bigT)*pnorm(calculate_d2(S, E, r, bigT, sigma))
  return(call_rho)
}
## for puts:
put_rho_hedge = function(S, E, r, bigT, sigma) {
  put_rho = -1*bigT*E*exp(-1*r*bigT)*pnorm(-1*calculate_d2(S, E, r, bigT, sigma))
  return(put_rho)
}

ui <- fluidPage(
  tabsetPanel(
    tabPanel("European Option Pricing and Hedging", fluid = TRUE,
             titlePanel("European Option Pricing and Hedging using the Black-Scholes Formula"),
             sidebarLayout(
               sidebarPanel(numericInput(inputId = "s_sel",
                                         label = "Price of the Underlying Asset:",
                                         value = 60),
                            numericInput(inputId = "e_sel",
                                         label = "Exercise Price:",
                                         value = 60),
                            sliderInput(inputId = "r_sel",
                                        label = "Interest Rate (%):",
                                        min = 0,
                                        max = 30,
                                        value = 8,
                                        step = 0.1),
                            numericInput(inputId = "t_sel",
                                         label = "Days Until Expiration:",
                                         value = 90),
                            sliderInput(inputId = "sigma_sel",
                                        label = "Volatility (%):",
                                        min = 0,
                                        max = 50,
                                        value = 30,
                                        step = 0.5)),
               mainPanel(tableOutput(outputId = "pricing_table"))
             )
    )
  )
)

server <- function(input, output, session) {
  
  table_react = reactive({
    callprice = call_price(S = input$s_sel, 
                           E = input$e_sel, 
                           r = (input$r_sel)/100, 
                           bigT = (input$t_sel)/365, 
                           sigma = (input$sigma_sel)/100)
    putprice = put_price(S = input$s_sel, 
                         E = input$e_sel, 
                         r = (input$r_sel)/100, 
                         bigT = (input$t_sel)/365, 
                         sigma = (input$sigma_sel)/100)
    
    calldelta = call_delta_hedge(S = input$s_sel, 
                                 E = input$e_sel, 
                                 r = (input$r_sel)/100, 
                                 bigT = (input$t_sel)/365, 
                                 sigma = (input$sigma_sel)/100)
    putdelta = put_delta_hedge(S = input$s_sel, 
                               E = input$e_sel, 
                               r = (input$r_sel)/100, 
                               bigT = (input$t_sel)/365, 
                               sigma = (input$sigma_sel)/100)
    
    gamma = gamma_hedge(S = input$s_sel, 
                        E = input$e_sel, 
                        r = (input$r_sel)/100, 
                        bigT = (input$t_sel)/365, 
                        sigma = (input$sigma_sel)/100)
    
    calltheta = call_theta_hedge(S = input$s_sel, 
                                 E = input$e_sel, 
                                 r = (input$r_sel)/100, 
                                 bigT = (input$t_sel)/365, 
                                 sigma = (input$sigma_sel)/100)
    puttheta = put_theta_hedge(S = input$s_sel, 
                               E = input$e_sel, 
                               r = (input$r_sel)/100, 
                               bigT = (input$t_sel)/365, 
                               sigma = (input$sigma_sel)/100)
    
    vega = vega_hedge(S = input$s_sel, 
                      E = input$e_sel, 
                      r = (input$r_sel)/100, 
                      bigT = (input$t_sel)/365, 
                      sigma = (input$sigma_sel)/100)
    
    callrho = call_rho_hedge(S = input$s_sel, 
                             E = input$e_sel, 
                             r = (input$r_sel)/100, 
                             bigT = (input$t_sel)/365, 
                             sigma = (input$sigma_sel)/100)
    putrho = put_rho_hedge(S = input$s_sel, 
                           E = input$e_sel, 
                           r = (input$r_sel)/100, 
                           bigT = (input$t_sel)/365, 
                           sigma = (input$sigma_sel)/100)
    
    table = data.frame(
      Option_Type = c("European Call Option", "European Put Option"),
      Option_Price = c(callprice, putprice),
      Delta_Hedge = c(calldelta, putdelta),
      Gamma_Hedge = c(gamma, gamma),
      Theta_Hedge = c(calltheta, puttheta),
      Vega_Hedge = c(vega, vega),
      Rho_Hedge = c(callrho, putrho)
    )
    
    table |>
      mutate(Delta_Hedge = round(Delta_Hedge, digits = 5))
  })
  
  output$pricing_table = renderTable({
    table_react()
  })
  
}
shinyApp(ui, server)