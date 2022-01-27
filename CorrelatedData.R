## ----setup, include=FALSE----------------------------------
library(tidyverse); library(broom);
library(knitr); library(ggplot2)
knitr::opts_chunk$set(echo = T, warning = F, message = F);
knitr::knit_hooks$set(mysize = function(before, options, envir) {
  if (before) 
    return(options$size);
})

def.chunk.hook  <- knitr::knit_hooks$get("chunk")
knitr::knit_hooks$set(chunk = function(x, options) {
  x <- def.chunk.hook(x, options)
  ifelse(options$size != "normalsize", paste0("\n \\", options$size,"\n\n", x, "\n\n \\normalsize"), x)
})

#knitr::opts_chunk$set(width = 10);
#knitr::opts_chunk$set(tidy.opts=list(width.cutoff=40));
recache = F;
options(digits = 4);
figure_scaler = 1/2; #1/2 for ioslides; ~1/3 for word, pdf
text_scaler = 3/3;#1 for ioslides; 2/3 for word, pdf
fig.x = 16 * figure_scaler;
fig.y = 9 * figure_scaler;


## ---- echo = TRUE, include = TRUE, eval = FALSE------------
## library(lme4)
## ?sleepstudy


## ---- echo = FALSE-----------------------------------------
library(lme4);


## ---- echo = F, cache = F, size = "scriptsize", fig.width = fig.x, fig.height=fig.y----
ggplot(sleepstudy, aes(x = Days, y = Reaction, group = Subject)) + 
  geom_line() + 
  scale_x_continuous(breaks = 0:9, minor_breaks = NULL) + 
  theme(text = element_text(size = 22));


## ---- cache = F--------------------------------------------
cm_sleep <- lmer(Reaction ~ Days + (1 | Subject), sleepstudy)


## ---- cache = F--------------------------------------------
mm_sleep_ind <- lm(Reaction ~ Days, sleepstudy)


## ---- cache = F--------------------------------------------
library(nlme)
mm_sleep_cs <- gls(Reaction ~ Days, sleepstudy, 
                   correlation = corCompSymm(form = ~1|Subject))


## ---- cache = F--------------------------------------------
library(geepack)
mm_sleep_gee_cs <- 
  geeglm(Reaction ~ Days, 
         data = sleepstudy, 
         id = Subject,
         corstr = "exchangeable")


## ---- cache = F, message = F, results = F------------------
library(gee)
mm_sleep_gee_cs_alt <-
  gee(Reaction ~ Days, data = sleepstudy, id = Subject,
      corstr = "exchangeable")


## ---- cache = F--------------------------------------------
summary(cm_sleep)$coefficients
summary(mm_sleep_ind)$coefficients 
summary(mm_sleep_cs)$tTable
summary(mm_sleep_gee_cs)$coefficients 
summary(mm_sleep_gee_cs_alt)$coefficients


## ----------------------------------------------------------
set.seed(1)
sleepstudy %>% 
  # Randomly permuting rows
  slice_sample(n = nrow(.)) %>%
  # gives different result
  geeglm(Reaction ~ Days, 
         data = ., 
         id = Subject,
         corstr = "exchangeable")



## ----------------------------------------------------------
summary(mm_sleep_gee_cs)$corr


## ---- echo = F---------------------------------------------
ggplot(data = bind_cols(mm_sleep_gee_cs$data, 
                        Residuals = residuals(mm_sleep_gee_cs)[,1]),
       aes(x = Days, y = Residuals, group = Subject)) + 
  geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = 0:9, minor_breaks = NULL) + 
  theme(text = element_text(size = 22));


## ---- echo = F, fig.width = fig.x, fig.height =  1.3*fig.y----
library(ggcorrplot)
bind_cols(mm_sleep_gee_cs$data, 
          Residuals = residuals(mm_sleep_gee_cs)[,1]) %>% 
  as_tibble() %>%
  pivot_wider(id_cols = Subject, names_from = Days, values_from = Residuals, names_prefix = "Day") %>%
  select(-Subject) %>%
  as.matrix() %>%
  cor() %>%
  ggcorrplot(lab = TRUE, show.legend = FALSE)


## ---- cache = F--------------------------------------------
# Compound-symmetric correlation
summary(mm_sleep_gee_cs)$coefficients

# Autoregressive-1 correlation
mm_sleep_gee_ar1 <- 
  geeglm(Reaction ~ Days, 
         data = sleepstudy, 
         id = Subject,
         corstr = "ar1")
summary(mm_sleep_gee_ar1)$coefficients


## ----------------------------------------------------------
summary(cm_sleep)$coefficients


## ----------------------------------------------------------
cm_sleep_random_slope <- 
  lmer(Reaction ~ Days + (Days | Subject), sleepstudy);
summary(cm_sleep_random_slope)$coefficients


## ----------------------------------------------------------
extractAIC(cm_sleep)
extractAIC(cm_sleep_random_slope) # Better fit with random slope


## ---- echo = F---------------------------------------------
ranef(cm_sleep_random_slope)


## ---- echo = F, fig.width=1.1*fig.x, fig.height=1.1*fig.y----
ggplot(data = bind_cols(sleepstudy, 
                        Prediction = predict(cm_sleep_random_slope, newdata = sleepstudy)),
       aes(x = Days, y = Prediction, group = Subject)) + 
  geom_line(alpha = 0.5) + 
  scale_x_continuous(breaks = 0:9, minor_breaks = NULL) + 
  theme(text = element_text(size = 22));


## ---- echo = F, fig.width=1.1*fig.x, fig.height=1.1*fig.y----
ggplot(data = bind_cols(sleepstudy, 
                        Prediction = predict(cm_sleep_random_slope, newdata = sleepstudy)) %>%
         filter(Subject %in% c("308", "309", "310", "330")),
       aes(x = Days, y = Prediction, group = Subject, color = Subject)) + 
  geom_line(size = 1) + 
  geom_line(aes(y = Reaction), size = 1, linetype = "dashed") + 
  scale_color_brewer(palette = "Dark2") + 
  guides(color = "none") + 
  scale_x_continuous(breaks = 0:9, minor_breaks = NULL) + 
  theme(text = element_text(size = 22));

