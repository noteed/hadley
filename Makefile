all: .hadley_touched

dist/build/hadley/hadley:
	./build.sh

images/hadley/hadley: dist/build/hadley/hadley
	cp $< $@

images/hadley/hlint-1.9.13:
	docker run --cidfile=container_id images.reesd.com/reesd/stack-pandoc true
	docker wait `cat container_id`
	docker cp `cat container_id`:/home/gusdev/.cabal/share/hlint-1.9.13 images/hadley/
	rm container_id

.hadley_touched: images/hadley/Dockerfile images/hadley/hadley images/hadley/hlint-1.9.13
	docker build -t images.reesd.com/reesd/hadley images/hadley
	touch $@
