# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# AMA CV Risk Calculator
# http://my.americanheart.org/professional/StatementsGuidelines/
#   PreventionGuidelines/Prevention-Guidelines_UCM_457698_SubHomePage.jsp
# downloaded 2013-11-18 @ 21:45
# @ericpgreen
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
# This file is not intended to replace the AMA CV Risk Calculator or be used in 
#   a clincal context. The only purpose is to explore alternate forms of data
#   visualization.
# ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

library(shiny)
library(ggplot2)

# Define server logic required to summarize and view the selected dataset
  shinyServer(function(input, output) {
    Data <- reactive({
      
    # input ----
      age <- input$age
      male <- ifelse(input$gender=="Male", 1, 0)
      white <- ifelse(input$race=="White", 1, 0)
      cholesterol <- input$cholesterol
      hdl <- input$hdl
      sbp <- input$sbp
      treatment <- ifelse(input$treatment=="Yes", 1, 0)
      treatment.alt <- ifelse(treatment==1, 0, 1)
      diabetes <- ifelse(input$diabetes=="Yes", 1, 0)
      smoker <- ifelse(input$smoker=="Yes", 1, 0)
    
    # 10 year ASCVD ----
      # actual
        # construct from input
          lnage <- log(age)
          lntot <- log(cholesterol)
          lnhdl <- log(hdl)
          trlnsbp <- (log(sbp))*treatment
          ntlnsbp <- (log(sbp))*treatment.alt
          age2 <- lnage*lnage
          agetc <- lnage*lntot
          agehdl <- lnage*lnhdl
          agetsbp <- lnage*trlnsbp
          agentsbp <- lnage*ntlnsbp
          agesmoke <- lnage*smoker
          agedm <- lnage*diabetes
      
        # calculate
          s0_10.f.aa <- 0.95334
          s0_10.f.wh <- 0.96652
          s0_10.m.aa <- 0.89536
          s0_10.m.wh <- 0.91436
          mnxb.f.aa <- 86.6081
          mnxb.f.wh <- -29.1817
          mnxb.m.aa <- 19.5425
          mnxb.m.wh <- 61.1816
          predict.f.aa <- 17.1141*lnage+0.9396*lntot+(-18.9196*lnhdl)+
                          4.4748*agehdl+29.2907*trlnsbp+(-6.4321*agetsbp)+
                          27.8197*ntlnsbp+(-6.0873*agentsbp)+0.6908*smoker+
                          0.8738*diabetes
          predict.f.wh <- (-29.799*lnage)+4.884*age2+13.54*lntot+(-3.114*agetc)+
                          (-13.578*lnhdl)+3.149*agehdl+2.019*trlnsbp+
                          1.957*ntlnsbp+7.574*smoker+(-1.665*agesmoke)+
                          0.661*diabetes
          predict.m.aa <- 2.469*lnage+0.302*lntot+(-0.307*lnhdl)+1.916*trlnsbp+
                          1.809*ntlnsbp+0.549*smoker+0.645*diabetes
          predict.m.wh <- 12.344*lnage+11.853*lntot+(-2.664*agetc)+(-7.99*lnhdl)+
                          1.769*agehdl+1.797*trlnsbp+1.764*ntlnsbp+7.837*smoker+
                          (-1.795*agesmoke)+0.658*diabetes
          cvd.predict.f.aa <- 1-((s0_10.f.aa)^(exp(predict.f.aa-mnxb.f.aa)))
          cvd.predict.f.wh <- 1-((s0_10.f.wh)^(exp(predict.f.wh-mnxb.f.wh)))
          cvd.predict.m.aa <- 1-((s0_10.m.aa)^(exp(predict.m.aa-mnxb.m.aa)))
          cvd.predict.m.wh <- 1-((s0_10.m.wh)^(exp(predict.m.wh-mnxb.m.wh)))
          
          ascvd.10 <- ifelse(male==1 & white==1, cvd.predict.m.wh,
                      ifelse(male==1 & white==0, cvd.predict.m.aa,
                      ifelse(male==0 & white==1, cvd.predict.f.wh,
                      ifelse(male==0 & white==0, cvd.predict.f.aa,
                             NA))))
          ascvd.10.p <- ascvd.10*100
    
      # optimal ----
        cholesterol.o <- 170
        hdl.o <- 50
        sbp.o <- 110
        treatment.o <- 0
        treatment.alt.o <- 1
        diabetes.o <- 0
        smoker.o <- 0
        
        # construct
          lnage.o <- log(age)                          # no optimal for age
          lntot.o <- log(cholesterol.o)
          lnhdl.o <- log(hdl.o)
          trlnsbp.o <- 0
          ntlnsbp.o <- (log(sbp.o))*treatment.alt.o
          age2.o <- lnage.o*lnage.o
          agetc.o <- lnage.o*lntot.o
          agehdl.o <- lnage.o*lnhdl.o
          agetsbp.o <- lnage.o*trlnsbp.o
          agentsbp.o <- lnage.o*ntlnsbp.o
          agesmoke.o <- lnage.o*smoker.o
          agedm.o <- lnage.o*diabetes.o
       
        # calculate
          predict.f.aa.o <- 17.1141*lnage.o+0.9396*lntot.o+(-18.9196*lnhdl.o)+
                            4.4748*agehdl.o+29.2907*trlnsbp.o+
                            (-6.4321*agetsbp.o)+27.8197*ntlnsbp.o+
                            (-6.0873*agentsbp.o)+0.6908*smoker.o+
                            0.8738*diabetes.o
          predict.f.wh.o <- (-29.799*lnage.o)+4.884*age2.o+13.54*lntot.o+
                            (-3.114*agetc.o)+(-13.578*lnhdl.o)+3.149*agehdl.o+
                            2.019*trlnsbp.o+1.957*ntlnsbp.o+7.574*smoker.o+
                            (-1.665*agesmoke.o)+0.661*diabetes.o
          predict.m.aa.o <- 2.469*lnage.o+0.302*lntot.o+(-0.307*lnhdl.o)+
                            1.916*trlnsbp.o+1.809*ntlnsbp.o+0.549*smoker.o+
                            0.645*diabetes.o
          predict.m.wh.o <- 12.344*lnage.o+11.853*lntot.o+(-2.664*agetc.o)+
                            (-7.99*lnhdl.o)+1.769*agehdl.o+1.797*trlnsbp.o+
                            1.764*ntlnsbp.o+7.837*smoker.o+(-1.795*agesmoke.o)+
                            0.658*diabetes.o
          cvd.predict.f.aa.o <- 1-((s0_10.f.aa)^(exp(predict.f.aa.o-mnxb.f.aa)))
          cvd.predict.f.wh.o <- 1-((s0_10.f.wh)^(exp(predict.f.wh.o-mnxb.f.wh)))
          cvd.predict.m.aa.o <- 1-((s0_10.m.aa)^(exp(predict.m.aa.o-mnxb.m.aa)))
          cvd.predict.m.wh.o <- 1-((s0_10.m.wh)^(exp(predict.m.wh.o-mnxb.m.wh)))
        
          ascvd.10.o <- ifelse(male==1 & white==1, cvd.predict.m.wh.o,
                        ifelse(male==1 & white==0, cvd.predict.m.aa.o,
                        ifelse(male==0 & white==1, cvd.predict.f.wh.o,
                        ifelse(male==0 & white==0, cvd.predict.f.aa.o,
                               NA))))
          ascvd.10.o.p <- ascvd.10.o*100
    
    # lifetime risk ----
      # actual
        cholesterol.lt180 <- ifelse(cholesterol<180,1,0)   
        cholesterol.gte180 <- ifelse(cholesterol>=180,1,0) 
        cholesterol.lt200 <- ifelse(cholesterol<200,1,0)
        cholesterol.gte200 <- ifelse(cholesterol>=200,1,0)
        cholesterol.lt240 <- ifelse(cholesterol<240,1,0)
        cholesterol.gte240 <- ifelse(cholesterol>=240,1,0)
        cholesterol.opt <- cholesterol.lt180
        cholesterol.notopt <- cholesterol.gte180*cholesterol.lt200
        cholesterol.elevated <- cholesterol.gte200*cholesterol.lt240
        cholesterol.major <- cholesterol.gte240
        #treatment
        treatment.no <- ifelse(treatment==1,0,1)
        sbp.lt120 <- ifelse(sbp<120,1,0)
        sbp.gte120 <- ifelse(sbp>=120,1,0)
        sbp.lt140 <- ifelse(sbp<140,1,0)
        sbp.gte140 <- ifelse(sbp>=140,1,0)
        sbp.lt160 <- ifelse(sbp<160,1,0)
        sbp.gte160 <- ifelse(sbp>=160,1,0)
        sbp.opt <- sbp.lt120*treatment.no
        sbp.notopt <- sbp.gte120*sbp.lt140*treatment.no
        sbp.elevated <- sbp.gte140*sbp.lt160*treatment.no
        sbp.major <- treatment+sbp.gte160
        sum.opt <- cholesterol.opt+sbp.opt
        sum.notopt <- cholesterol.notopt+sbp.notopt
        sum.elevated <- cholesterol.elevated+sbp.elevated
        #smoker
        #diabetes
        major.sum <- cholesterol.major+sbp.major+smoker+diabetes
      # both genders
        major.gt2 <- ifelse(major.sum>=2,1,0)
        major.1 <- ifelse(major.sum==1,1,0)
          sum.elevated.gte1 <- ifelse(sum.elevated>=1,1,0)
          major.sum.0 <- ifelse(major.sum==0,1,0)
        elevated <- sum.elevated.gte1*major.sum.0
          sum.notopt.gte1 <- ifelse(sum.notopt>=1,1,0)
          sum.notopt.0 <- ifelse(sum.notopt==0,1,0)
        notopt <- sum.notopt.gte1*sum.notopt.0*major.sum.0
          sum.opt.2 <- ifelse(sum.opt==2,1,0)
        opt <- sum.opt.2*major.sum.0
      # females
        f.major.gt2.ascvd <- 50
        f.major.1.ascvd <- 39
        f.elevated.ascvd <- 39
        f.notopt.ascvd <- 27
        f.opt.ascvd <- 8
        f.ascvd <- (major.gt2*f.major.gt2.ascvd) +
                   (major.1*f.major.1.ascvd) +
                   (elevated*f.elevated.ascvd) +
                   (notopt*f.notopt.ascvd) +
                   (opt*f.opt.ascvd)
      # males
        m.major.gt2.ascvd <- 69
        m.major.1.ascvd <- 50
        m.elevated.ascvd <- 46
        m.notopt.ascvd <- 36
        m.opt.ascvd <- 5
        m.ascvd <- (major.gt2*m.major.gt2.ascvd) +
                   (major.1*m.major.1.ascvd) +
                   (elevated*m.elevated.ascvd) +
                   (notopt*m.notopt.ascvd) +
                   (opt*m.opt.ascvd)
        ascvd.life <- ifelse(male==1, m.ascvd, f.ascvd)
        ascvd.life.o <- ifelse(male==1, 5, 8)
  
    # dataframe ----
      dat <- NULL
      dat <- as.data.frame(dat)
      dat[1,1] <- 1
      dat[1,2] <- "ascvd.10"
      dat[1,3] <- ascvd.10.o.p
      dat[2,1] <- 0
      dat[2,2] <- "ascvd.10"
      dat[2,3] <- ascvd.10.p
      dat[3,1] <- 1
      dat[3,2] <- "ascvd.life"
      dat[3,3] <- ascvd.life.o
      dat[4,1] <- 0
      dat[4,2] <- "ascvd.life"
      dat[4,3] <- ascvd.life
      names(dat) <- c("optimal", "measure", "value")
      dat
      })

  # plot ----
    output$riskPlot <- renderPlot( {
      dat <- Data()
      p <- ggplot(dat, aes(x=measure, y=value, colour=factor(optimal))) +
                  geom_point(size=5) +
                  coord_flip() +
                  ylab("Risk (%)") +
                  scale_colour_manual(values=c("red", "green"),
                                    name="Comparison",
                                    breaks=c(0,1),
                                    labels=c("You", "The ideal you"))
      print(p)
    })
  })
