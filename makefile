all: compile

compile:
	alex Lexer.x
	happy Grammar.y
	ghc --make main.hs -o retina 

clean:
	rm -rf -v *.o *.hi retina
