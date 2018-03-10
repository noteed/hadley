all: .hadley_touched .stack_pandoc_touched

dist/build/hadley/hadley: bin/hadley.hs .stack_pandoc_touched
	./build.sh

images/hadley/hadley: dist/build/hadley/hadley
	cp $< $@

images/hadley/hlint-2.1/hlint.1: .stack_pandoc_touched
	docker run --cidfile=container_id images.reesd.com/reesd/stack-pandoc true
	docker wait `cat container_id`
	docker cp `cat container_id`:/home/gusdev/.cabal/share/x86_64-linux-ghc-7.10.3/hlint-2.1 images/hadley/
	rm container_id

.hadley_touched: images/hadley/Dockerfile images/hadley/hadley images/hadley/hlint-2.1
	docker build -t noteed/hadley images/hadley
	touch $@

.stack_pandoc_touched: images/stack-pandoc/Dockerfile
	docker build -t images.reesd.com/reesd/stack-pandoc images/stack-pandoc
	touch $@
