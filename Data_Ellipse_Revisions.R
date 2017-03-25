
library(spida2)
library(car)
library(magrittr)
Prestige %>% head
plot(prestige ~ education, Prestige)
abline(lm(prestige~education, Prestige), col = 'blue', lwd = 2)
with(Prestige, lines(dell(education, prestige), lwd = 2, col = 'red'))
with(Prestige, lines(dellplus(education, prestige), lwd = 2, col = 'red'))
dellplus
ellplus
??ell
elltan
de <- with(Prestige, dell(education, prestige))
de
elltan(de)

Prestige %>% head  -> . %>% summary
