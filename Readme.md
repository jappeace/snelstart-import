[![https://jappieklooster.nl](https://img.shields.io/badge/blog-jappieklooster.nl-lightgrey)](https://jappieklooster.nl/tag/haskell.html)
[![Github actions build status](https://img.shields.io/github/workflow/status/jappeace/haskell-template-project/Test)](https://github.com/jappeace/haskell-template-project/actions)
[![Jappiejappie](https://img.shields.io/badge/discord-jappiejappie-black?logo=discord)](https://discord.gg/Hp4agqy)
[![Hackage version](https://img.shields.io/hackage/v/template.svg?label=Hackage)](https://hackage.haskell.org/package/template) 

> In lyk persoan (sûnder skuld) is in ryk persoan

Dit is een snelstart importeer programma voor n26.
+ Pas SnelstartImport.hs met je account gegevens.
+ Exporteer n26 als csv en zet input.csv
+ nix develop
+ cabal run
+ nu kun je out.csv importeren in snelstart.

Onderwater zet dit de gegevens om van n26 formaat naar
dat van ING.

Neem contact op met mij als er problemen zijn

### Tools
Enter the nix shell.
```
nix develop
```
You can checkout the makefile to see what's available:
```
cat makefile
```

### Running
```
make run
```

### Fast filewatch which runs tests
```
make ghcid
```
