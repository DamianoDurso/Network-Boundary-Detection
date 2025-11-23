# Readme for Repo


### Example how to calculate corr from wave data from cosine_and_corr.csv
cor(as.numeric(df_cosines$gpt3.large[2][[1]]), as.numeric(df_cosines$empirical_corr[[2]]$`1`))


- logbook

- Explain folders structure
- Explains processing logic


# How to push large embedding file (cosine_scales.csv)
git lfs install
git lfs track "cosine_scales.csv"
echo ".gitattributes" >> .gitignore  
git add .gitattributes cosine_scales.csv
git commit -m "Track cosine_scales.csv with Git LFS"
git push origin main
..... 
