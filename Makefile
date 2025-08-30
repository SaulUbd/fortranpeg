path = src
source = grammar/peg_grammar.pegjs
output = parser/peg_parser.js

${path}/${output}: ${path}/${source}
	npx peggy --format es -o $@ $<
