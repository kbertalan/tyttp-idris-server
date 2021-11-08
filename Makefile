package=tyttp-idris-server.ipkg
executable=tyttp-idris-server
idris2=idris2
codegen=node

.PHONY: build clean repl

build:
	bash -c 'time $(idris2) --build $(package) --codegen $(codegen)'

clean:
	rm -rf build

run: build
	bash -c 'time node build/exec/$(executable)'

install:
	$(idris2) --install $(package) --codegen $(codegen)

dev:
	find src/ -name *.idr | entr make run

dev-build:
	find src/ -name *.idr | entr make build

