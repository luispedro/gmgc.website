all:
	mkdir -p build
	elm make src/Main.elm --output=build/js/index.js
	elm make src/GenomeBinSearchResult.elm --output=build/js/GenomeBinSearchResults.js
	elm make src/SearchResults.elm --output=build/js/SearchResults.js
