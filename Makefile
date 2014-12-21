all: .hadley_touched .stack_pandoc_touched

dist/build/hadley/hadley: bin/hadley.hs
	./build.sh

images/hadley/hadley: dist/build/hadley/hadley
	cp $< $@

images/hadley/hlint-1.9.13:
	docker run --cidfile=container_id images.reesd.com/reesd/stack-pandoc true
	docker wait `cat container_id`
	docker cp `cat container_id`:/home/gusdev/.cabal/share/hlint-1.9.13 images/hadley/
	rm container_id

.hadley_touched: images/hadley/Dockerfile images/hadley/hadley images/hadley/hlint-1.9.13
	docker build -t noteed/hadley images/hadley
	touch $@

.stack_pandoc_touched:
	docker build -t images.reesd.com/reesd/stack-pandoc images/stack-pandoc
	touch $@
