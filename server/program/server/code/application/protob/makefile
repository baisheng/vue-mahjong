SOURCES := $(wildcard proto/*.proto)

Destinations := $(subst proto/, src/, $(SOURCES))

.PHONY: all

all:$(Destinations) 

src/%.proto:proto/%.proto
	@cp  $< $@

clean:
	@rm -rf src/*.proto
