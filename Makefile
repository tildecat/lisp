output:
	mkdir -p output

all: intro.md | output
	pandoc --from markdown+footnotes+pandoc_title_block --toc -s intro.md -o output/intro.html

~/public_html/p:
	mkdir -p ~/public_html/p

publish: all | ~/public_html/p
	cp output/* ~/public_html/p/
