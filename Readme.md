# Readme for Repo


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
