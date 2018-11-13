output:
	mkdir -p output

all: intro.md | output
	pandoc --from markdown+footnotes+pandoc_title_block --toc -s intro.md -o output/intro.html
