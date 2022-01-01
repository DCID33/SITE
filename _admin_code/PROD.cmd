cd \Users\DC\Documents
"C:\Program Files\R\R-3.6.3\bin\RScript.exe" C:\Users\DC\Documents\Sites\SITE1\_r_code\PRODSITE.R
"C:\Program Files\R\R-3.6.3\bin\RScript.exe" C:\Users\DC\Documents\Sites\SITE1\_r_code\valueboxes.R

xcopy /y "C:\Users\DC\Documents\Sites\SITE1\_r_code\valueboxes.html" "C:\Users\DC\Documents\Sites\SITE1\valueboxes.html"
del "C:\Users\DC\Documents\Sites\SITE1\_r_code\valueboxes.html"
xcopy /y /E "C:\Users\DC\Documents\Sites\SITE1" "C:\Users\DC\Documents\GitHub\SITE"

#cd \Users\DC\Documents\GitHub\SITE
#git add .
#git commit -m "update"
#git push