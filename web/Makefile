RM := rm -f
SRC := $(wildcard src/*.js)
ENTRY := src/main.js
BUNDLE := out/bundle.js
BROWSERIFY := ./node_modules/.bin/browserify
UGLIFY := ./node_modules/.bin/uglify-js

.PHONY: release debug clean

release: $(BUNDLE)
	$(UGLIFY) $(BUNDLE) > $(BUNDLE).tmp
	$(MV) $(BUNDLE).tmp $(BUNDLE)

debug: $(BUNDLE)

$(BUNDLE): $(SRC)
	$(BROWSERIFY) $(ENTRY) -o $(BUNDLE)

clean:
	$(RM) $(BUNDLE) $(BUNDLE).tmp


