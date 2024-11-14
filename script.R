rm(list=ls())
options(OutDec = ',')

library(tidyverse)
library(ggplot2)
library(ggridges)
library(rjags)
library(coda)
library(dclone)
library(ggmcmc)
library(kableExtra)
library(pROC)

dat = 
  read.table(
    'dados.csv', header=T, sep = ';', dec = ',',
    quote = "", stringsAsFactors = T, fill = T
  ); head(dat)

params = 
  read.table(
    'params.csv', header=T, sep = ';', na.strings = "" ,
    stringsAsFactors = F, fill = T
  ); head(params)

modelos = names(params)

# ==============================================================
# ----- ORGANIZAÇÃO DO BANCO DE DADOS ------------------------ #
# ==============================================================

# ------------------------------------------------------------ #
# ----- Dando nomes as categorias ---------------------------- #
  dat = 
    dat %>%
    mutate(
      PE = dplyr::recode(
        PE,
        laranja_caipira = 'Laranja Caipira',
        limao_cravo = 'Limao Cravo',
        tangerina_cleopatra = 'Tangerina Cleopatra',
        tangerina_sunki = 'Tangerina Sunki'
      ),
      GE = dplyr::recode(
        GE,
        IAC_2000 = 'IAC2000',
        IpiguaIAC = 'IpiguaIAC'
      ),
      PEnum = as.numeric(PE),
      GEnum = as.numeric(GE)
    )

# ------------------------------------------------------------ #
# ----- Variável 'sujeito' ----------------------------------- #
  for(i in 1:36){
    dat$ID2[dat$CL==i] = (10*(i-1)+1):(10*i)
  }
  head(dat,-100)
  dat$ID = dat$planta + rep(seq(0,350,by=10),each=50)

# ------------------------------------------------------------ #
  dat$CL = paste(dat$PE,'\n',dat$GE,sep='')

# ==============================================================
# ----- DESCRITIVA ------------------------------------------- #
# ==============================================================
  dat %>%
    ggplot(aes(x=AV, y=Y)) +
    geom_line(aes(group=ID), colour="gray") +
    geom_point(aes(group=ID), colour="gray") +
    stat_summary(fun.y = "mean", geom = "line") +
    stat_summary(fun.y = "mean", geom = "point") +
    geom_smooth(method = lm, se = F, linetype = "dotted", colour = "black") +
    lims(y=c(0,.6)) +
    facet_grid(PE~GE) +
    theme_bw()
  
# ------------------------------------------------------------ #
# ----- Boxplot - GE ----------------------------------------- #
  BP_GE =
    ggplot(data = dat %>% 
             filter(Y > 0) %>%
             mutate(AV=recode(AV, "2"=3, "3"=6, "4"=9, "5"=12, "6"=15)), 
           aes(x=factor(AV), y=Y)) +
    geom_boxplot(aes(group=AV), fill="gray", outlier.size = 1,
                 outlier.shape=1, size=0.1) +
    stat_summary(aes(group=AV), fun.y = "mean", geom = "point", size=1) +
    geom_smooth(method=lm, aes(group=GE), se=F, colour="black", size=.2) +
    geom_point(data = dat %>% 
                 filter(Y == 0) %>%
                 mutate(AV=factor(recode(AV, "2"=3, "3"=6, "4"=9, "5"=12, "6"=15))) %>%
                 group_by(AV,GE) %>%
                 summarise(n=n()) %>%
                 mutate(p=n/40), 
               aes(x=AV, y=p), size=1) +
    geom_smooth(data = dat %>% 
                  filter(Y == 0) %>%
                  mutate(AV=factor(recode(AV, "2"=3, "3"=6, "4"=9, "5"=12, "6"=15))) %>%
                  group_by(AV,GE) %>%
                  summarise(n=n()) %>%
                  mutate(p=n/40), 
                aes(x=as.numeric(AV), y=p), 
                method = lm, se = F, colour = "black", size = .2) +
    lims(y=c(0,1)) +
    labs(x="Approximated time (months)", y="Leaf Citrus Canker Incidence") +
    facet_wrap(~GE, ncol=9) +
    theme_bw(); BP_GE

  ggsave(filename = "BP_GE.pdf", plot = BP_GE, device = "pdf", scale = 1.3, width = 170, height = 50, units = "mm")

# ------------------------------------------------------------ #
# ----- Boxplot - PE ----------------------------------------- #
  BP_PE =
    ggplot(data = dat %>% 
             filter(Y > 0) %>%
             mutate(AV=recode(AV, "2"=3, "3"=6, "4"=9, "5"=12, "6"=15)), 
           aes(x=factor(AV), y=Y)) +
    geom_boxplot(aes(group=AV), fill="gray", outlier.size = 1,
                 outlier.shape=1, size=0.1) +
    stat_summary(aes(group=AV), fun.y = "mean", geom = "point", size=1) +
    geom_smooth(method=lm, aes(group=PE), se=F, colour="black", size=.2) +
    geom_point(data = dat %>% 
                 filter(Y == 0) %>%
                 mutate(AV=factor(recode(AV, "2"=3, "3"=6, "4"=9, "5"=12, "6"=15))) %>%
                 group_by(AV,PE) %>%
                 summarise(n=n()) %>%
                 mutate(p=n/90), 
               aes(x=AV, y=p), size=1) +
    geom_smooth(data = dat %>% 
                  filter(Y == 0) %>%
                  mutate(AV=factor(recode(AV, "2"=3, "3"=6, "4"=9, "5"=12, "6"=15))) %>%
                  group_by(AV,PE) %>%
                  summarise(n=n()) %>%
                  mutate(p=n/90), 
                aes(x=as.numeric(AV), y=p), 
                method = lm, se = F, colour = "black", size = .2) +
    lims(y=c(0,1)) +
    labs(x="Approximated time (months)", y="Leaf Citrus Canker Incidence") +
    facet_wrap(~PE, ncol=9) +
    theme_bw(); BP_PE
  
  ggsave(filename = "BP_PE.pdf", plot = BP_PE, device = "pdf", scale = 1.3, width = 120, height = 50, units = "mm")

# ------------------------------------------------------------ #
# ----- Histograma - AV -------------------------------------- #
  dat.aux = 
    dat %>%
    group_by(AV) %>%
    summarise(n=length(Y), n0=length(Y[Y==0])) %>%
    mutate(p=n0/n) %>%
    left_join(dat, by="AV") %>%
    mutate(AV=dplyr::recode(AV, "2"="03 (meses)", "3"="06 (meses)", "4"="09 (meses)", "5"="12 (meses)", "6"="15 (meses)")) %>%
    rename(`Avaliação`=AV)

  HIST_AV =
    ggplot(data = dat.aux %>% filter(Y>0)) +
    geom_histogram(aes(x=Y, y=..count../sum(..count..)/0.045), 
                   binwidth = 0.045, fill="gray38") +
    geom_point(data = dat.aux %>% 
                 filter(Y>0) %>%
                 group_by(`Avaliação`) %>%
                 summarise(meanY=mean(Y)),
               aes(y=0, x=meanY), size=0.3, shape=2, colour="white") +
    geom_text(data = dat.aux %>% 
                filter(Y>0) %>%
                group_by(`Avaliação`) %>%
                summarise(meanY=mean(Y)),
              aes(y=0, x=meanY, label=round(meanY,3)),
              size=1.8, vjust=-1, check_overlap=T, colour="white") +
    geom_segment(aes(x=0, xend=0, y=0, yend=p), size=.25) + 
    geom_point(aes(x=0, y=p), size=0.75) + 
    geom_segment(aes(x=0, xend=1, y=p, yend=p), 
                 size=0.3, linetype="longdash") +
    geom_text(aes(x=1, y=p, label=paste(round(100*p,2),"%",sep="")), 
              check_overlap=T, size=2.5, vjust=-1, hjust=1) + 
    scale_x_continuous(limits = c(0,1), breaks = seq(0,1,.2)) +
    labs(x="Leaf Citrus Canker Incidence", y="Density") +
    facet_wrap(~`Avaliação`, ncol=9, labeller = label_both) +
    theme_bw(); HIST_AV

  ggsave(filename = "HIST_AV.pdf", plot = HIST_AV, device = "pdf", scale = 1.3, width = 170, height = 40, units = "mm")

# ------------------------------------------------------------ #

# ==============================================================
# ----- MODELO ----------------------------------------------- #
# ==============================================================

# ------------------------------------------------------------ #
# ----- Definição do modelo ---------------------------------- #
# 
# Cada estrutura de regressora deve ser construida sobre
# esse codigo base. Deve-se usar como referencia os 
# modelos descritos no arquivo params.csv
#
# ------------------------------------------------------------ #
  cat("
      model {
          # Modelo para cada unidade observacional
          for (i in 1:N) {
            ## -- (verossimilhança)
              logit(nu[i]) <- ( beta_nu_0 + u_nu[ID[i],1] ) +
              beta_nu_AV * AV[i] + beta_nu_AV2 * pow(AV[i],2) +
              beta_nu_PE[PEnum[i]] + beta_nu_GE[GEnum[i]]
            # -
              logit(mu[i]) <- ( beta_mu_0 + u_mu[ID[i],1] ) +
              beta_mu_AV * AV[i] + beta_mu_AV2 * pow(AV[i],2) +
              beta_mu_PE[PEnum[i]] + beta_mu_GE[GEnum[i]]
            # -
              log(sigma[i]) <- beta_sg_0
            # -
              l[i] <- loggam(a[i]+b[i]) - loggam(a[i]) - loggam(b[i]) + 
              (a[i]-1) * log(y[i]) + (b[i]-1) * log(1-y[i])
              a[i] <- mu[i] * sigma[i]
              b[i] <- (1-mu[i]) * sigma[i]
            
              logLik[i] <- id[i] * log(nu[i]) + (1-id[i]) * ( log(1-nu[i]) + l[i] )
              Lik[i] <- exp(logLik[i])
            
              p[i] <- Lik[i] / 10000
              ones[i] ~ dbern(p[i])
          }
          
          # PRIORIS -----------------------------------------------------------------
          ## -- (priori dos efeitos fixos)
            beta_nu_0 ~ dnorm( 0.0, 0.0001 )
            beta_nu_AV ~ dnorm( 0.0, 0.0001 )
            beta_nu_AV2 ~ dnorm( 0.0, 0.0001 )
          #
            beta_mu_0 ~ dnorm( 0.0, 0.0001 )
            beta_mu_AV ~ dnorm( 0.0, 0.0001 )
            beta_mu_AV2 ~ dnorm( 0.0, 0.0001 )
          #
            beta_sg_0 ~ dnorm( 0.0, 0.0001 )
          #
            beta_nu_GE[6] <- 0
            beta_mu_GE[6] <- 0
            for (j in c(1:5,7:9)){
              beta_nu_GE[j] ~ dnorm( 0.0, 0.0001 )
              beta_mu_GE[j] ~ dnorm( 0.0, 0.0001 )  
            }
          #
            beta_nu_PE[1] <- 0
            beta_mu_PE[1] <- 0
            for (j in 2:4){
              beta_nu_PE[j] ~ dnorm( 0.0, 0.0001 )
              beta_mu_PE[j] ~ dnorm( 0.0, 0.0001 )  
            }
          ## -- (priori dos efeitos aleatórios)
          # Intercepto aleatório para cada indivíduo
            for( i in 1:nID ){
              u_nu[i,1] ~ dmnorm(0,omega_u_nu)
              u_mu[i,1] ~ dmnorm(0,omega_u_mu)
            }
            sigma_u_nu <- 1/omega_u_nu
            omega_u_nu ~ dgamma(0.01,0.01)
          # 
            sigma_u_mu <- 1/omega_u_mu
            omega_u_mu ~ dgamma(0.01,0.01)
          }", file="Beta13.jag"
  )

# ------------------------------------------------------------ #
# ----- Dados para o JAGS ----------------------------------- #
  dados = dat
  dados = 
    dados %>%
    mutate(AV = dplyr::recode(AV, "2"=3, "3"=6, "4"=9, "5"=12, "6"=15) )

  jagsdata = 
    list(
      N = nrow(dados),
      nID = length(unique(dados$ID)),
      y =  ifelse(dados$Y==0,0.0001,dados$Y),
      id = ifelse(dados$Y==0,1,0),
      PEnum = dados$PEnum,
      GEnum = dados$GEnum,
      AV = dados$AV,
      ID = dados$ID,
      ones = rep(1,nrow(dados))
    )

# ------------------------------------------------------------ #
# ----- Inicializacao --------------------------------------- #
  inits = list(beta_sg_0 = 1)
  n.chains = 2
  n = nrow(dados)

# ------------------------------------------------------------ #
# ----- Para calcular as metricas de selecao ---------------- #
  # res <- NULL
  # model <- list()
  # DIC <- list()
  # for(i in 13:13){
  #   model[[i]] = 
  #     jags.model(
  #       file = paste("./Modelos/",modelos[i],".jag",sep=""),
  #       data = jagsdata,
  #       inits = inits, 
  #       n.chains = n.chains, 
  #       n.adapt = 15000
  #     )
  #   # update(model[[i]], n.iter=10)
  #   # mod.post <- coda.samples(model[[i]], na.omit(params[,modelos[i]]), n.iter=10, thin=1)
  #   # DIC[[i]] <- dic.samples(model[[i]], n.iter=1000, type="pD")
  #   # k <- length(na.omit(params[,modelos[i]]))
  #   # DICc <- sum(DIC[[i]]$deviance)
  #   # EAIC = DICc + 2*k
  #   # EBIC = DICc + k*log(n)
  #   # res <- rbind(res,c(i,DIC=DICc,EAIC=EAIC,EBIC=EBIC))
  # }
  
# ------------------------------------------------------------ #
# ----- Rodando o modelo selecionado ------------------------- #
  i = 13
  timings = vector('numeric', 2)
  timer = proc.time()
  cl = makePSOCKcluster(22)
  tmp = clusterEvalQ(cl, library(dclone))
  parLoadModule(cl, 'glm')
  parLoadModule(cl, 'lecuyer')
  parLoadModule(cl, 'dic')
  mod.post = 
    jags.parfit(
      cl = cl,
      data = jagsdata, inits = inits, params = c(na.omit(params[,modelos[i]]),'mu','nu','sigma','u_nu','u_mu'),
      model = paste("./Modelos/",modelos[i],".jag",sep=""),
      n.chains = 2, n.adapt = 0,
      n.update = 35000, n.iter = 100000, thin = 1
    )
  stopCluster(cl)
  time.taken = proc.time() - timer
  timings[2] = time.taken[3]
  
  save.image("../Resultados/RESULTADOSmodel13.RData")

# ------------------------------------------------------------ #
# ----- Resumos da distribuição a posteriori ----------------- #
  mod1.p = summary(mod.post)
  cbind(
    round(mod1.p$statistics,5)[,1:2],
    round(mod1.p$quantiles,4)[,c(1,3,5)]
  )

  dat.PE = unique(dat[,c("PE","PEnum")]) %>% arrange(PEnum)
  dat.GE = unique(dat[,c("GE","GEnum")]) %>% arrange(GEnum)
  
  mod.BP = mod1.p
  mod.BP[[2]][1:12,c(1,4)]
  I = 1:16
  
  nomes = rownames(mod.BP[[1]])
#
  padrao01 = 'beta_mu'
  padrao02 = 'sigma_u_mu'
  linhasMu = grepl(padrao01,nomes) | grepl(padrao02,nomes)
  
  MU = cbind(mod.BP[[1]][linhasMu,1:2],mod.BP[[2]][linhasMu,c(1,5)],mod.BP[[1]][linhasMu,3:4]) %>% as.data.frame() 
  MU = MU %>% mutate(Variavel=c("","","",as.character(dat.GE$GE),as.character(dat.PE$PE),""), Parametro="mu")
#
  padrao01 = 'beta_nu'
  padrao02 = 'sigma_u_nu'
  linhasNu = grepl(padrao01,nomes) | grepl(padrao02,nomes)
  
  NU = cbind(mod.BP[[1]][linhasNu,1:2],mod.BP[[2]][linhasNu,c(1,5)],mod.BP[[1]][linhasNu,3:4]) %>% as.data.frame()
  NU = NU %>% mutate(Variavel=c("","","",as.character(dat.GE$GE),as.character(dat.PE$PE),""), Parametro="nu")
#
  padrao01 = 'beta_sg'
  linhasSg = grepl(padrao01,nomes)
  
  SG = cbind(mod.BP[[1]][linhasSg,1:2,drop=F],mod.BP[[2]][linhasSg,c(1,5),drop=F],mod.BP[[1]][linhasSg,3:4,drop=F]) %>% as.data.frame()
  SG = SG %>% mutate(Variavel=c(""), Parametro="sg")
  
# ------------------------------------------------------------ #
# ----- Diagnósticos de convergência das cadeias ------------- #
  MCMC.aux = lapply(c(1,2), function(x) window(mod.post[[x]], thin=100))
  summaryGR = gelman.diag(MCMC.aux, multivariate = F)
  summaryHW = heidel.diag(MCMC.aux[[1]])
  summaryGW = geweke.diag(MCMC.aux)
  
  GR = round(summaryGR[[1]][linhasNu | linhasMu | linhasSg,1],3)
  HW = summaryHW[linhasNu | linhasMu | linhasSg,3]
  GW = ifelse(summaryGW[[1]]<0,2*pnorm(summaryGW[[1]]),2*pnorm(summaryGW[[1]],lower.tail=F))
  GW = GW[linhasNu | linhasMu | linhasSg]
  
  cbind(rbind(NU,MU,SG),GR,HW,GW) %>%
    dplyr::select(Variavel,Parametro,everything()) %>%
    kable(format = "latex", digits = 4, booktabs=T,
          col.names = c("Variable","Parameter","Posteriori Mean","Standard Deviation","2.5%","97.5%","Naive","Time-Series","GR statistics","HW p-value","GW p-value"),
          row.names = F, 
          caption = "Summary measures.") %>%
    add_header_above(c(" " = 4, "95% Credibility Interval" = 2, "Standard Error" = 2, "Convergence Diagnosis" = 3))
  
  MCMC = MCMC.aux[[1]][,(1:16)+16] 
  colnames(MCMC) = c("Intercepto","Avaliação","Avaliação (quad.)",as.character(dat.GE$GE),as.character(dat.PE$PE))
  S = ggs(MCMC)
  par = "nu"
  
  GHist = ggs_histogram(S) + theme_bw()
  GHist$facet$params$ncol = 4; GHist
  ggsave(filename = paste("Diag_Hist_",par,".pdf",sep=""), plot = GHist, device = "pdf", 
         scale = 1.3, width = 200, height = 120, units = "mm")
  
  GDens = ggs_density(S) + theme_bw()
  GDens$facet$params$ncol = 4; GDens
  ggsave(filename = paste("Diag_Dens_",par,".pdf",sep=""), plot = GDens, device = "pdf", 
         scale = 1.3, width = 200, height = 120, units = "mm")
  
  GTrac = ggs_traceplot(S) + theme_bw() + theme(axis.text.x = element_text(size = 6))
  GTrac$facet$params$ncol <- 4; GTrac
  ggsave(filename = paste("Diag_Trac_",par,".pdf",sep=""), plot = GTrac, device = "pdf", 
         scale = 1.3, width = 200, height = 120, units = "mm")
  
  GRunn = ggs_running(S) + theme_bw() + theme(axis.text.x = element_text(size = 6), legend.position = "none")
  GRunn$facet$params$ncol = 4; GRunn
  ggsave(filename = paste("Diag_Runn_",par,".pdf",sep=""), plot = GRunn, device = "pdf", 
         scale = 1.3, width = 200, height = 120, units = "mm")
  
  GAutCorr = ggs_autocorrelation(S) + theme_bw()
  GAutCorr$facet$params$ncol = 4; GAutCorr
  ggsave(filename = paste("Diag_AutCorr_",par,".pdf",sep=""), plot = GAutCorr, device = "pdf", 
         scale = 1.3, width = 200, height = 120, units = "mm")
  
  ggs_geweke(S)
  GCater = ggs_caterpillar(S); GCater
  ggsave(filename = paste("Diag_Cater_",par,".pdf",sep=""), plot = GCater, device = "pdf", 
         scale = 1.1, width = 100, height = 80, units = "mm")
  
  ggs_pairs(S, lower = list(continuous = "density"))

# ------------------------------------------------------------ #
# ----- Diagnostico das previsoes ---------------------------- #
  rowNames = rownames(mod1.p$statistics)
  
  mu = mod1.p$statistics[rowNames[grepl(pattern = 'mu\\[', x = rowNames) & !grepl(pattern = '_mu\\[', x = rowNames)],1]
  mu.sd = mod1.p$statistics[rowNames[grepl(pattern = 'mu\\[', x = rowNames) & !grepl(pattern = '_mu\\[', x = rowNames)],2]
  mu.md = mod1.p$quantiles[rowNames[grepl(pattern = 'mu\\[', x = rowNames) & !grepl(pattern = '_mu\\[', x = rowNames)],3]
  mu.li = mod1.p$quantiles[rowNames[grepl(pattern = 'mu\\[', x = rowNames) & !grepl(pattern = '_mu\\[', x = rowNames)],1]
  mu.ls = mod1.p$quantiles[rowNames[grepl(pattern = 'mu\\[', x = rowNames) & !grepl(pattern = '_mu\\[', x = rowNames)],5]
  
  nu = mod1.p$statistics[rowNames[grepl(pattern = 'nu\\[', x = rowNames) & !grepl(pattern = '_nu\\[', x = rowNames)],1]
  nu.sd = mod1.p$statistics[rowNames[grepl(pattern = 'nu\\[', x = rowNames) & !grepl(pattern = '_nu\\[', x = rowNames)],2]
  nu.md = mod1.p$quantiles[rowNames[grepl(pattern = 'nu\\[', x = rowNames) & !grepl(pattern = '_nu\\[', x = rowNames)],3]
  nu.li = mod1.p$quantiles[rowNames[grepl(pattern = 'nu\\[', x = rowNames) & !grepl(pattern = '_nu\\[', x = rowNames)],1]
  nu.ls = mod1.p$quantiles[rowNames[grepl(pattern = 'nu\\[', x = rowNames) & !grepl(pattern = '_nu\\[', x = rowNames)],5]
  
  sigma = mod1.p$statistics[rowNames[grepl(pattern = 'sigma\\[', x = rowNames)],1]
  sigma.sd = mod1.p$statistics[rowNames[grepl(pattern = 'sigma\\[', x = rowNames)],2]
  sigma.md = mod1.p$quantiles[rowNames[grepl(pattern = 'sigma\\[', x = rowNames)],3]
  sigma.li = mod1.p$quantiles[rowNames[grepl(pattern = 'sigma\\[', x = rowNames)],1]
  sigma.ls = mod1.p$quantiles[rowNames[grepl(pattern = 'sigma\\[', x = rowNames)],5]

  datResAux =
    dados %>%
    bind_cols(
      tibble(
        mu.md = mu.md,
        mu.sd = mu.sd,
        mu.li = mu.li,
        mu.ls = mu.ls,
        nu.md = nu.md,
        nu.sd = nu.sd,
        nu.li = nu.li,
        nu.ls = nu.ls,
        sigma.md = sigma.md,
        sigma.sd = sigma.sd,
        sigma.li = sigma.li,
        sigma.ls = sigma.ls
      )
    )

# ------------------------------------------------------------ #
# ----- Diagnostico das previsoes - Obs vs Pred -------------- #
  G00 = 
    datResAux %>%
    tibble() %>% 
    filter(Y != 0) %>% 
    summarise(
      .by = c(PE,GE,AV),
      mu.md = mean(mu.md),
      sigma.md = sigma.md,
      mu.sd = mean(mu.sd),
      mu.li = mean(mu.li),
      mu.ls = mean(mu.ls),
      Y = mean(Y)
    ) %>% 
    group_by(PE,GE) %>% 
    mutate(maxY=max(c(Y,mu.ls))) %>% 
    mutate(cy = purrr::pmap(.l=list(maxY,mu.md,mu.sd), .f=function(Y,mu,sd){
      shape1 = ( (1-mu)/sd**2 - 1/mu ) * mu**2
      shape2 = shape1 * (1/mu-1)
      x = seq(1e-5,0.65,l=101)
      y = dbeta(x = x, shape1 = shape1, shape2 = shape2)
      y = 0.85*y/max(y)
      tibble(cx=x, cy=y)
    })) %>%
    unnest_legacy() %>% 
    group_by(PE) %>% 
    mutate(maxY=max(c(Y,mu.ls))) %>% 
    ggplot() +
    geom_vridgeline(aes(x=AV, y=cx, width=cy, group=AV), alpha=0.5, colour='lightgray', linewidth=0.25) +
    geom_line(aes(x=AV, y=mu.md), linewidth=0.25) +
    geom_segment(aes(x=AV, xend=AV, y=mu.li, yend=mu.ls), linewidth=0.5) +
    geom_point(data=datResAux %>% filter(Y>0), aes(x=AV, y=Y), colour='red', size=1.5, shape=16) +
    facet_grid(PE~GE, scales = 'free_y') +
    theme_minimal() +
    theme(
      strip.background = element_rect(fill='lightgray', colour='black'),
      panel.background = element_rect(colour='black'),
      panel.grid = element_blank()
    ); G00
  
  ggsave(filename = 'ObsVsPredmu.pdf', plot = G00, device = 'pdf', scale = 1.0, width = 12, height = 6)

# ------------------------------------------------------------ #
# ----- Diagnostico das previsoes - boxplot ------------------ #
  G01 = 
    datResAux %>% 
    mutate(Y = ifelse(Y>0,1,Y)) %>% 
    mutate(Y = ifelse(Y==0,'Zero incidence\n(Y=0)','Non-zero incidence\n(Y>0)')) %>% 
    ggplot(aes(y=nu, x=Y)) +
    geom_boxplot(fill='gray', outlier.shape = 1, outlier.size = 4) +
    geom_violin(alpha=0.1, linewidth=0.15) +
    geom_jitter(alpha=0.05, size=5, width=0.075) +
    labs(x=NULL, y=bquote(nu)) +
    coord_flip() +
    theme_minimal(); G01
  
  ggsave(filename = 'ObsVsPrednu.pdf', plot = G00, device = 'pdf', scale = 1.0, width = 12, height = 6)

# ------------------------------------------------------------ #
# ----- Diagnostico das previsoes - ROC - AUC ---------------- #
  Y = ifelse(datResAux$Y>0,1,0)
  Ypred = datResAux$nu.md
  
  sim_roc = 
    roc(
      response = Y,
      predictor = Ypred,
      levels = c('0', '1')
    )

  G02 = 
    ggroc(
      sim_roc, 
      legacy.axes = TRUE
    ) +
    geom_abline(slope = 1, intercept = 0, linetype = 'dashed') +
    labs(x = 'False-positive rate', y = 'True-positive rate') +
    annotate('text', x = 0.125, y = 0.95, label = paste0('AUC: ',round(auc(sim_roc), digits = 2))) +
    theme_minimal(); G02

  ggsave(filename = 'ROCnu.pdf', plot = G00, device = 'pdf', scale = 1.0, width = 12, height = 6)

# ------------------------------------------------------------ #
# ----- Diagnostico das previsoes - Simulacao AUC ------------ #  
  set.seed(2022)
  samples = 25000
  yes_case_probs = sample(
    Ypred[Y==0],
    samples, 
    replace = TRUE
  )
  no_case_probs <- sample(
    Ypred[Y==1],
    samples, 
    replace = TRUE
  )
  prob_yes_higher_than_prob_no = yes_case_probs > no_case_probs
  
  running_proportion = cumsum(prob_yes_higher_than_prob_no) / seq_along(prob_yes_higher_than_prob_no)
  running_proportion_dat = data.frame(draw = 1:samples, running_proportion)
  
  G03 =
    running_proportion_dat %>% 
      ggplot(aes(x = draw, y = running_proportion)) +
      geom_line(color = 'cornflowerblue') +
      geom_hline(yintercept = round(auc(sim_roc), digits = 4), alpha = .5, color = 'salmon') +
      labs(y = 'Proportion', x = 'Number of samples') +
      annotate('text', x = 22500, y = (round(auc(sim_roc), digits = 2) + 0.1),
               label = 'AUC ROC', alpha = .5) + 
      ylim(0,1) + 
      theme_minimal(); G03

# ------------------------------------------------------------ #
# ----- Matriz de cores -------------------------------------- #  
  mod.BP = summary(mod.post)
  tab = cbind(
    Estimativa = exp(mod.BP[[1]][,1]),
    ErroPadrao = mod.BP[[1]][,4],
    LI = exp(mod.BP[[2]][,1]),
    LS = exp(mod.BP[[2]][,5]))
  tab = tab[18:32,]
  
  OR = tab[c(1:2,8,3:7,9:15),]
  OR[,1] <- ifelse(OR[,3]<1 & 1<OR[,4],1,OR[,1])
  AV <- exp(c(2,3,4,5,6)*log(OR[1,1]) + c(2,3,4,5,6)**2*log(OR[2,1]))
  PE <- OR[12:15,1]
  nPE <- c('Laranja\nCaipira','Limao\nCravo','Tangerina\nCleopatra','Tangerina\nSunki')
  GE <- OR[3:11,1]
  nGE <- c('IpiguaIAC','Arapongas','Bianchi','EEL','IAC','IAC2000','N58','N59','Olimpia')
  #
  pdf('OR_Reg_Long.pdf', width=10, height=6)
    par(mar=c(0,0,0,0))
    aj <- 0.04
    plot(NULL, xlab='Genotype', ylab='Rootstock', axes=F,
         xlim=c(-0.8+5*aj,9.8-5*aj), ylim=c(-1.8+5*aj,20.8-5*aj))
    for(i in 0:8){
      text(x=i+0.5, y=20.4, labels=nGE[i+1], font=2,
           col='black')
    }
    for(j in 0:3){
      text(x=9.4, y=5*j+2.5, labels=nPE[j+1], font=2,
           col='black', srt=270)    
      text(x=rep(-0.05,5), y=(1:5+5*j-0.5), labels=c(3,6,9,12,15), font=2, cex=.6)
    }
    for(i in 0:8){
      for(j in 0:3){
        for(a in 0:4){
          alf <- GE[i+1]*PE[j+1]*AV[a+1]
          rect(xleft=(i+aj), xright=(i+1-aj), 
               ybottom=(5*j+a+ifelse(a==0,0.125,0.00)), 
               ytop=(5*j+a-ifelse(a==4,0.125,0.00)+1), 
               col=adjustcolor('black',2.5*alf), border=NA)
          if(i==0 & j==0 & a==0){
            text(x=0.5, y=0.5, col='white', font=2, labels=paste('1,0000 (ref)'), cex=.7)
          } else {
            text(x=i+0.5, y=(5*j+a+ifelse(a==0,0.075,0.00)-ifelse(a==4,0.075,0.00)+0.5), 
                 col=ifelse(2.5*alf>0.5,'white','black'),
                 labels=formatC(x=alf, digits=4, format='f'), cex=.7)
          }
        }
      }
    }
    polygon(x=c(0+aj,0+aj,9-aj,9-aj), y=c(-1.8+5*aj,0.0-5*aj,0.0-5*aj,-1.8+5*aj), 
            col=adjustcolor('black'), border=NA)
    text(x=4.5, y=-0.825, label='GENOTYPES', font=2, col='white')
    polygon(x=c(-0.8+5*aj,-0.8+5*aj,0.0-5*aj,0.0-5*aj), y=c(0,20,20,0), 
            col=adjustcolor('black'), border=NA)
    text(x=-0.4, y=10, label='ROOTSTOCKS', font=2, col='white', srt=90)
  dev.off()

