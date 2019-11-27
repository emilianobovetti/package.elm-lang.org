npx_elm_make := npx elm make src/Main.elm --output=artifacts/elm.js

all: node_modules/.bin artifacts
	$(npx_elm_make) --optimize

debug: node_modules/.bin artifacts
	$(npx_elm_make) --debug

node_modules/.bin:
	yarn

artifacts:
	mkdir artifacts

format:
	elm-format --yes src
