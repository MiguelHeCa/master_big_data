library(DistatisR)

data("DistAlgo")
data("SortingBeer")

?Chi2Dist

Punctuation = matrix(
  c(
    7836,
    13112,
    6026,
    53655,
    102383,
    42413,
    115615,
    184541,
    59226,
    161926,
    340479,
    62754,
    38177,
    105101,
    12670,
    46371,
    58367,
    14299
  ),
  ncol = 3,
  byrow = TRUE
)
colnames(Punctuation) <- c('Period', 'Comma', 'Other')
rownames(Punctuation) <- c('Rousseau',
                           'Chateaubriand',
                           'Hugo',
                           'Zola',
                           'Proust',
                           'Giroudoux')

Dres <- Chi2Dist(Punctuation)
Dres

?DistanceFromSort

DistanceFromSort(Sort)
