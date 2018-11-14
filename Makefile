output:
	mkdir -p output

all: intro.md index.md | output
	pandoc --from markdown+footnotes+pandoc_title_block --toc -s intro.md -o output/intro.html
	pandoc --from markdown+footnotes+pandoc_title_block --toc -s index.md -o output/index.html


~/public_html/p:
	mkdir -p ~/public_html/p

publish: all | ~/public_html/p
	cp output/* ~/public_html/p/
