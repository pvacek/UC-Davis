## About me

I come from Santa Rosa, CA. I transferred to UC Davis from Santa Rosa Junior College at the age of 20, the Harvard of the west coast, as an Economics major with minimal programming experience. Thanks to the wonderful teaching abilities of the Statistics Department here, I developed an interest in the practice of Statistics, so I joined their program at the age of 22. The courses I have taken since then have allowed me to showcase my skills in R and Python.

### Work Samples from STA 141B: "Stat Data Technologies" AKA Pythonic Data Science for lil' undergrads

<ul>
<li><a href="STA 141B/Project/STA 141B Final Project (Patrick).ipynb" title="Final Project">White House Petitions (Final Project)</a></li>
<li><a href="STA 141B/Assignment 2/assignment2_PatrickVacek.ipynb" title="Numpy">Oops the cat</a></li>
<li><a href="STA 141B/Assignment 2/assignment3_PatrickVacek.ipynb" title="Pandas">Revenge of the cucumbers</a></li>
<li><a href="STA 141B/Assignment 2/assignment4_PatrickVacek.ipynb" title="API">Fruit Database (API's and Modeling)</a></li>
<li><a href="STA 141B/Assignment 2/assignment5_PatrickVacek.ipynb" title="Web Scraping / NLP">Neighborizing the articles of the Aggie</a></li>
<li><a href="STA 141B/Assignment 2/assignment6_PatrickVacek.ipynb" title="SQL / GIS">Taking a bite out of San Francisco crime, and food trucks</a></li>
</ul>

### Some code examples from R

#Cross-validation from STA 104: Nonparametric Statistics

```R
fold<-function(F,data){
  rndm<-data[sample(1:nrow(data)),]
  fold.lengths<-rep(trunc(nrow(data)/F),F)
  random.folds<-sample(1:F,nrow(data)%%F)
  fold.lengths[random.folds]<-fold.lengths[random.folds]+1
  fold.indices<-lapply(1:F,function(i)seq(1+sum(fold.lengths[min(1,i-1):(i-1)]),sum(fold.lengths[1:i])))
  folds<-lapply(1:F,function(i)rndm[fold.indices[[i]],])
  return(folds)
}
```

### Contact Me
Email: re.sub("p","pr","pvacek")+"@"+re.sub("UC-D","ucd","UC-Davis")+".edu"
