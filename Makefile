#all: remake
all: quicklisp

LISP=sbcl

quicklisp: clean
	echo '(load "my.lisp")' |env QUICKLISP_ONLY=1 ${LISP}

remake: clean
	echo '(load "my.lisp")' |${LISP}

my.core:
	echo '(load "my.lisp")' |${LISP}

clean:
	rm -f my.core cepler

run: FORCE
	./run
run-game: FORCE
	echo '(cepler:start-game)' |./run
run-game-1536x864: FORCE
	echo '(cepler:start-game :width 1536 :height 864)' |./run
FORCE:

clean-fasls:
# ~/.cache/common-lisp/


exe: FORCE
	echo '(load "my.lisp")' |env QUICKLISP_ONLY=1 MAKE_EXECUTABLE=1 ${LISP}

dist: FORCE
	rm -rf dist
	mkdir dist
	cp -a cepler console.png stars planets set2 fonts sounds licenses README.md dist/
	mkdir dist/source
	cp -a *.asd *.lisp Makefile run dist/source/
