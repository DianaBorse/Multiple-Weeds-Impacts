
#R code for mixed model with poisson

summary(pfLatfly1<-glmer(data=Female, LatFly~  MArrival + Trial + PBSex +
                           SBcontrast +  Trial*PBSex  + SBcontrast*PBSex+  
                           + (1|FemID), family=poisson(), nAGQ = 0  )) 

#R code for extracting effects and building a figure

ss1r<- allEffects(BothSingYNr) #BothSingYNr is the name of the model
syn<-ss1r[[3]] # the effect I want to graph is the second in the model output print(e)
syn #(focal sex and contrast)
sls.df<- as.data.frame(syn) #makes it a dataframe for figures

#ggplot figure building - lots of extra code in here 
# my advice is to start with the bare bones and then build
#bar bones below the main code 
#the stat.smooth part adds the effects line - you can play with the k=# to adjust how bendy the line is.

songYNraw  <- ggplot() +theme_classic()+
  geom_jitter(data=BothPresent,aes(x=SBcontrast, y=Sing01, color=FocalSex, shape=Species), height = 0.1,
              width = 0.1, size= 3)+ 
  stat_smooth(data=sls.df, method = "gam", formula = y ~ s(x, k = 5),
              aes(y=fit,x=SBcontrast, color=FocalSex,
                  linetype=FocalSex),se=FALSE, size=3 ) +
  guides(scale = "none",size=FALSE, alpha=FALSE) + labs(fill="Focal Sex", shape= "Species/Population", 
                                                        color="Focal Sex", linetype="Focal Sex")+
  scale_shape_manual(values=c(0,1,2,10,13,5,6,7,8,9))+
  scale_color_manual(values=clrs, labels=c("Female"," Male")) +
  scale_linetype_discrete(labels=c("Female"," Male"))+
  labs(y="Probability of singing", x =  "Scaled contrast/conspiciousness") +
  theme(legend.position="bottom", legend.box="vertical", legend.margin=margin())+
  guides(color = guide_legend(order=1),shape = guide_legend(order=2), linetype=FALSE)
songYNraw


songYNraw  <- ggplot() +theme_classic()+
  geom_jitter(data=BothPresent,aes(x=SBcontrast, y=Sing01)) + 
  stat_smooth(data=sls.df, method = "gam", formula = y ~ s(x, k = 5),
              aes(y=fit,x=SBcontrast, color=FocalSex,
                  linetype=FocalSex),se=FALSE, size=3 ) +
  songYNraw

# code for stargazer which makes nice tables
#the first things in () are the names of models
# out = is the name of the Word file it exports

stargazer(fFly1sr,fLatfly1,fsApp1r,gfsTimeR, report = "vcs*p", type = "html",  
          title = "The relationship between behaviour and color in females- Flight and approach", 
          align=TRUE, model.numbers  = FALSE,
          single.row = TRUE,   intercept.bottom = FALSE, notes.align = "l",
          out="Female flight and app table Dec 25.doc")
