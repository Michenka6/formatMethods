FSLEXYACC="/Users/amiralimov/.nuget/packages/fslexyacc/10.0.0/build"

all:
	mono $(FSLEXYACC)/fslex/net46/fslex.exe Lexer.fsl --unicode && mono $(FSLEXYACC)/fsyacc/net46/fsyacc.exe Parser.fsy --module book.Parser