
library(shiny)
library(corrplot)
library(gridExtra)
library(ggplot2)

#Import the dataset
temperature_data <- read.csv("E:\\R-Lectures\\Assignment2\\Assignment 2.csv")
View(temperature_data)

#Combined wind direction is a categorical variable, so encoding it into a numerical variable.
temperature_data$cbwd <- as.numeric(temperature_data$cbwd)-1

#Getting the names of the variables which contains the missing values
colnames(temperature_data)[colSums(is.na(temperature_data))>0]

#checking the number of mssing values in the above found variable
sum(is.na(temperature_data$pm2.5))

#Imputing the missing values in the pm2.5 with median data of that variable
temperature_data$pm2.5[which(is.na(temperature_data$pm2.5))] <- median(na.omit(temperature_data$pm2.5))

#1.  Linear Regression Model building with combination of variables

corrplot(cor(temperature_data))

#Including all independent variables for linear regression model building except the first one which is the Number column
model1 <- lm(TEMP~.-No,data=temperature_data)

#Multiple R-Squared value is 0.8325 with all the variables taken into the model pretty significant
summary(model1)

model1

#Get the model 1 coefficients 
model1_coef <- summary(model1)$coef

#Including only the year,DEWP and PRES variables for fitting linear model
model2 <- lm(TEMP~ DEWP+PRES,data=temperature_data)

#Multiple R-Squared value is 0.7667 with all the variables taken into the model pretty significant
summary(model2)

#Get the model2 coefficients
model2_coef <- summary(model2)$coef

#Including only the year,month,day,hour,DEWP and PRES variables for fitting linear model
model3 <- lm(TEMP~ year+month+day+hour+DEWP+PRES,data=temperature_data)

#Multiple R-Squared value is 0.789 with 'day' being insignificant
summary(model3)

#Get the model 3 coefficients
model3_coef <- summary(model3)$coef

#So, Removing day and the intercept from model3 and again fit the linear model and checking for R-squared value
model4 <- lm(TEMP~ year+month+hour+DEWP+PRES,data=temperature_data)

#Multiple R-Squared value is 0.789, same as the previous model.
summary(model4)

#Get the coefficients for model4
model4_coef <- summary(model4)$coef

#Checking AIC (Akaike's Information Criterion) Scores for various linear models built above 
AIC(model1)  #265333.4

AIC(model2)  #279541.4

AIC(model3)  #275424.4

AIC(model4)  #275431.2

#The AIC for model1 is the lowest of all our models as well as it has the highest multiple R-squared value out of all our
#models.


#2 and 3 Shiny App for displaying various plots like scatterplots,boxplots,histograms and scatter plot with regression
#line and table of slope and intercept based on the selection from dropdown box.
ui <- fluidPage(
    
    # Application title
    titlePanel("Shiny Dashboard"),
    
    sidebarLayout(
        sidebarPanel(
        #radio buttons for choosing the plots
        radioButtons("plotChoice","Choices",c("scatterplot","boxplot","histogram","scatterplot_with_line")),
        selectInput("xvariableChoice","X Variable",c("year","month","day","hour","pm2.5","DEWP","TEMP","PRES","cbwd","Iws","Is","Ir")),
        selectInput("yvariableChoice","Y Variable",c("year","month","day","hour","pm2.5","DEWP","TEMP","PRES","cbwd","Iws","Is","Ir"))
        ),
    
    # Main Panel
    mainPanel(
        plotOutput("plot"),
        tableOutput("table")
    )
    )
)

# Define server logic required to draw a scatterplot/boxplot/histogram/scatterplot with regression line
server <- function(input, output) {
    
    output$plot <- renderPlot({
        
        if(input$plotChoice=="scatterplot")
        {
            plot(temperature_data[input$xvariableChoice][[1]],temperature_data[input$yvariableChoice][[1]],xlab=input$xvariableChoice,
                 ylab = input$yvariableChoice)
        } 
        else if(input$plotChoice=="boxplot")
        {
            boxplot(temperature_data[input$yvariableChoice][[1]]~temperature_data[input$xvariableChoice][[1]],
                    col = "blue", xlab=input$xvariableChoice,
                    ylab = input$yvariableChoice)
        }
        else if(input$plotChoice=="histogram")
        {
            #hist(temperature_data[,input$xvariableChoice],main="Histogram", xlab=input$xvariableChoice)
            xhist <- qplot(temperature_data[,input$xvariableChoice],col=I("green"),fill=I("white"),geom = "histogram",binwidth=0.25,xlab=input$xvariableChoice)
            yhist <- qplot(temperature_data[,input$yvariableChoice],col=I("blue"),fill=I("white"),geom = "histogram",binwidth=0.25,xlab=input$yvariableChoice)
            grid.arrange(xhist,yhist,ncol=2)
        }
        else
        {
            plot(temperature_data[input$xvariableChoice][[1]],temperature_data[input$yvariableChoice][[1]],xlab=input$xvariableChoice,
                 ylab = input$yvariableChoice)
            
            model=lm(temperature_data[input$yvariableChoice][[1]]~temperature_data[input$xvariableChoice][[1]])
            abline(model,col="blue")
        }
    })
    
    output$table <- renderTable({
        if(input$plotChoice=="scatterplot_with_line")
        {
          #Fit a linear model for the x and y variable selected from the dropdown
          model=lm(temperature_data[input$yvariableChoice][[1]]~temperature_data[input$xvariableChoice][[1]])
          #Create a table with the slope and intercept value taken from the fitted model coefficients
          table(Slope=model$coefficients[2],Intercept=model$coefficients[1])
        }
    })
    
}
# Run the application 
shinyApp(ui = ui, server = server)


##4 Monte Carlo Simulations for Temperature Prediction for subsequent years

#Taking model 1 and model 2 for monte carlo simuulations 

#setting up Monte Carlo simulations

#To store the result of simulation for model 1
sim_model1 <- NULL

#To store the result of simulation for model 2
sim_model2 <- NULL

#In our dataset, we have time data (year,month,day,hour), so using the last year or latest value to try to simulate
#into the future, taking 2014 as that is the last year for which the observations were recorded 
subset_data <- subset(temperature_data,year==2014)

View(subset_data)

#model dispersion for model1 by getting the mean absolute deviation to measure the spread of noise
disp_model1=mad((summary(model1)$resi))

#model dispersion for model4 by getting the mean absolute deviation to measure the spread of noise
disp_model2=mad((summary(model2)$resi))


#Running Simulation for model 1 and model 2, thereby predicting the temperatures 
#Since we are dealing with time data, lets use the average of each of the varaibles in the last year for simulation
#Used uniform distribution for displacement as the actual temperature distribution approximately looks uniform 

#For reproducibility
set.seed(599)

for(i in 1:5000){
    sim_model1=cbind(sim_model1 ,model1_coef[1] + model1_coef[2]*mean(subset_data$year)+ model1_coef[3]*mean(subset_data$month)+  
               model1_coef[4]*mean(subset_data$day) + model1_coef[5]*mean(subset_data$hour) + model1_coef[6]*mean(subset_data$pm2.5) +
               model1_coef[7]*mean(subset_data$DEWP) + model1_coef[8]*mean(subset_data$PRES) + model1_coef[9]*mean(subset_data$cbwd) +
               model1_coef[10]*mean(subset_data$Is) + model1_coef[11]*mean(subset_data$Ir) +
                   runif(1,-4*disp_model1,4*disp_model1) ) 
    
    sim_model2=cbind(sim_model2 ,model2_coef[1] + model2_coef[2]*mean(subset_data$DEWP) + model2_coef[3]*mean(subset_data$PRES)+
                         runif(1,-4*disp_model2,4*disp_model2) ) 
}


#simulated distribution for model1
hist(sim_model1)

#compare to the mean of the actual value
abline(v=mean(temperature_data$TEMP),col="red")

mean(sim_model1)

#simulated distribution for model2
hist(sim_model2)

#compare to the mean of the actual value
abline(v=mean(temperature_data$TEMP),col="red")

#Get the mean of simulation 2
mean(sim_model2)

#Get the actual temperature mean
mean(temperature_data$TEMP)

#compare distribution with original data
hist(temperature_data$TEMP)


##5 ESS Statistics

#linear model with TEMP(Temperature) as y variable and PRES(Pressure) as the x variable
ess_model1 <- lm(TEMP~PRES,temperature_data)

#Get the coefficients of ess_model1
ess_model1_coeff <- summary(ess_model1)$coef

#linear model with TEMP(Temperature) as y variable and PRES(Pressure) and Iws(Cumulative Wind speed) as the x variables
ess_model2 <- lm(TEMP~PRES+Iws,temperature_data)

#Get the coefficients of ess_model2
ess_model2_coeff <- summary(ess_model2)$coef

#Monte Carlo Simulation for generating a distribution

#Get the dispersion of the ess_model1 to measure the spread of the noise using mean absolute deviation
disp1=mad(summary(lm(TEMP ~ PRES, data = temperature_data))$resid)

#Get the dispersion of the ess_model2 to measure the spread of the noise using mean absolute deviation
disp2=mad(summary(lm(TEMP ~ PRES+Iws, data = temperature_data))$resid)


#F statistic - To check which model is better using the below formula:
#  F = ((SS1-SS2)/(SS2)) / ((DF1-DF2)/(DF2))

#  where SS1 = sum of squares of model1
#        SS2 = sum of squares of model2
#        DF1 = Degrees of Freedom for model1 (degree of freedom = number of records - no of parameters)
#        DF2 = Degrees of Freedom for model2

#The sum of squares and degrees of freedom can be taken from the performing an anova test

#sum of squares for ess_model1
ss1 <- anova(ess_model1)[2,2]

#ss1=sum((summary(ess_model1)$resi)^2) 

#sum of squares for ess_model2
ss2 <- anova(ess_model2)[3,2]

#ss2=sum((summary(ess_model2)$resi)^2) 
#Assuming the residuals are normal

#Degrees of Freedom for ess_model1
df1 <- anova(ess_model1)[2,1]

#Degrees of Freedom for ess_model2
df2 <- anova(ess_model2)[3,1]

#numerator
numerator <- (ss1-ss2)/(ss2)
denominator <- (df1-df2)/(df2)

#Calculating F-statistic now
f_score <- numerator/denominator
f_score

#Since, (ss1-ss2)/(ss2) < (df1-df2)/(df2) and also the f_score is 0.2776, we conclude that ess_model1 is statistically significant than ess_model2.
#i.e., the simpler model is better (i.e.,ess_model1 is better).


#lets look at the residual sums of squares of both models and calculate ess (f-score) and do the hypothesis test on that parameter
#H0 = rss of model 1 is the true rss (i.e. rss will not change for different data)
#Halt = rss of model 1 is not the true rss (i.e. it will be significantly different)

#Assuming the errors and resilduals are normal
#To store the rss for model 1
rsss_model1=NULL
rsss_model2=NULL

ysimulation1=NULL
ysimulation2=NULL
ess_simulation=NULL

#For reproducibility
set.seed(599)

#Simulating the ess using rss of both the models
for(i in 1:500){
    #simulate dataset 500 times, calcuate rss on each dataset
    ysimulation1=ess_model1_coeff[1] + ess_model1_coeff[2]*temperature_data$PRES+ rnorm(nrow(temperature_data),0,2*disp1)
    rsss_model1=cbind(rsss_model1,sum((lm(ysimulation1~temperature_data$PRES)$resid)^2))
    
    ysimulation2=ess_model2_coeff[1] + ess_model2_coeff[2]*temperature_data$PRES+
                 ess_model2_coeff[3]*temperature_data$Iws+ rnorm(nrow(temperature_data),0,2*disp2)
    
    rsss_model2=cbind(rsss_model2,sum((lm(ysimulation2~temperature_data$PRES+temperature_data$Iws)$resid)^2))
    
}

#numerator
numerator_sim <- (rsss_model1-rsss_model2)/(rsss_model2)
denominator_sim <- (df1-df2)/(df2)

#ess simulation vector
ess_simulation= numerator_sim/denominator_sim
print(ess_simulation)

#simulated distribution for ESS
hist(ess_simulation)

#compare to original f-score
abline(v=f_score,col="red")


