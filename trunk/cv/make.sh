file=cv
pdflatex $file.tex
mv $file.pdf ~/public_html/tmp/$file.pdf
chmod 644 ~/public_html/tmp/$file.pdf
